NetDebouncer = NetDebouncer or {}

local IN_DEV = false
NetDebouncer.netIncoming =
	(IN_DEV and NetDebouncer.netIncoming) or -- when in-dev, just reuse the oldest net.Incoming
	(NetDebouncer.CurrentDetour == net.Incoming and NetDebouncer.netIncoming) or -- reuse saved netIncoming if our detour is the top-most one
	net.Incoming -- use current top-most net.Incoming if all else fails

local ndb = NetDebouncer

--[==================================[
				config
--]==================================]

ndb.Punishment = "log" 			-- punishment mode, either "kick" or "log"			(kicking is not recommended)
ndb.PunishmentCooldown = 1		-- can only punish once every X seconds				(useful to not spam the console with logs when someone spams net messages)

ndb.ProcessingLimit = 200 		-- if processing time of a net message takes more than this many ms.			(higher = heavier lag required to punish)
ndb.ProcessingFrame = 2 		-- in this many seconds, do punish 												(lower = more consistent lag required to punish)
ndb.ProcessingThreshold = 50	-- punishment will happen if they send nets taking more than this many ms. 		(higher = more tolerance for random unintentional lag)
ndb.ViolatedIgnoreDuration = 3 	-- if processing time is exceeded, nets from this player will be ignored for this many seconds

ndb.AllowNetsPerSecond = 5		-- every second a player is allowed to send this many nets 						(higher = more spam per second is required to punish)
ndb.AmountLimit = 25			-- if they exceed this limit by this many nets, punish							(higher = more time it takes to detect spam)
ndb.ErrorCounts = 1				-- an erroring net counts as X nets for ratelimit								(higher = quicker the player gets owned for having nets errored)
								-- ^ keep in mind, this may be a result of a bad addon! if you have erroring addons, you should keep this at 1

ndb.IgnoreDummyNets = true		-- should we not count nets that don't do anything?
								-- realistically such nets shouldn't even be sent, and they won't do anything anyhow, so ignoring them is fine


-- nets to not track and punish for
-- only works if we're the topmost detour, ie no other addons detour net.Incoming
ndb.IgnoreNets = {
	["SomeIgnoredNet"] = true,
	["etc..."] = true,

	-- default ignore list:
	gauth_notification = true, 	-- https://github.com/notcake/vfs - likes to slam a whole bunch of data on boot
	gcompute = true, 			-- https://github.com/notcake/gcompute - likes to slam **a lot** of nets when sending client data
}

-- only `amount` and `processing` are the reasons for punishment (currently, at least)

ndb.ReasonTranslations = {
	amount = "exceeded the net ratelimit amount (" .. ndb.AmountLimit .. ")",

	processing = "exceeded the net processing time (" .. ndb.ProcessingLimit .. "ms.)"
}

--[==================================[
			internals
--]==================================]

-- note that *all* punishments go on cooldown, meaning if you log then kick on the next message, the kick might not go through

local logCol = Color (230, 110, 255, 255)

local function doLog(fmt, ...)
	MsgC(logCol, "[NetDebounce] ", color_white, fmt:format(...), "\n")
end

-- `netName` will be nil if there are other addons detouring net.Incoming
ndb.Punishments = {
	log = function(ply, reason, netName)
		doLog("Player %s (%s) %s (Net name: '%s')", ply:Nick(), ply:SteamID(),
			ndb.ReasonTranslations[reason] or ("did something bad? (%s)"):format(reason),
			netName)
	end,

	kick = function(ply, reason, netName)
		ndb.Punishments.log(ply, reason, netName)
		doLog("Punishment mode is set to `kick`, kicking...")

		local why = ndb.ReasonTranslations[reason] or ("net usage violation (`%s`)"):format(reason)
		ply:Kick("NetDebouncer - " .. why)
	end,
}

local myNetIncoming -- pre-def, will be our net.Incoming detour

local capped = debug.getinfo(NetDebouncer.netIncoming)
local is_vanilla = capped.short_src == "lua/includes/extensions/net.lua"

-- function: are we the only ones detour to gmod's net.Incoming?
local isOnlyDetour

if not is_vanilla then -- dont even have to bother
	isOnlyDetour = function() return false end
else
	function isOnlyDetour()
		-- captured is overridden elsewhere
		return myNetIncoming == net.Incoming
	end
end

--[==================================[
		actual ratelimit logic
--]==================================]

-- [player] = true; if they're in this table, we allow them to spam
-- cuz usually during boot a whole bunch of data is sent back and forth
ndb.IgnorePlayers = ndb.IgnorePlayers or {}

hook.Add("PlayerInitialSpawn", "NDB_AllowSpam", function(ply)
	ndb.IgnorePlayers[ply] = true
	timer.Simple(10, function() ndb.IgnorePlayers[ply] = nil end)
end)

--[==================================[
 	[player] = {
		lastRecv = CurTime(),
		recvAmt = 0,

		procTime = 0,
		[lastPunish = 0],

		bannedFrom = {
			["netName"] = bannedTillCurtime,
		},

		toBanFrom = { -- nets to ban from due to processing
			["netName"] = true,
		},
 	}
 --]==================================]
NetDebouncer.RateData = NetDebouncer.RateData or {}


local function getDat(ply)
	local dat = ndb.RateData[ply] or { lastRecv = CurTime(), recvAmt = 0, procTime = 0, bannedFrom = {}, toBanFrom = {} }
	ndb.RateData[ply] = dat

	return dat
end



ndb.CurrentNet = "?"

function ndb.Punish(ply, why)
	local dat = getDat(ply)
	local sinceLastPunish = CurTime() - (dat.lastPunish or 0)

	if sinceLastPunish < ndb.PunishmentCooldown then
		return
	end

	dat.lastPunish = CurTime()

	local netName = isstring(ndb.CurrentNet) and ndb.CurrentNet or ("Unknown-Net(%s)"):format(ndb.CurrentNet)

	local toRun = ndb.Punishments[ndb.Punishment] or ndb.Punishments.log
	toRun(ply, why, netName)
end

-- returns: should we let this net through?
-- `false`, `false` - ignore net, abort processing
-- `false`, string - ignore net, do punish; string is the reason
-- `true` means continue processing net

function ndb.NetReceived(ply)
	if ndb.IgnoreNets[ndb.CurrentNet] or ndb.IgnorePlayers[ply] then return true end -- don't care

	local dat = getDat(ply)
	local ct = CurTime()
	if dat.bannedFrom[ndb.CurrentNet] and ct < dat.bannedFrom[ndb.CurrentNet] then return false, false end

	local passed = ct - dat.lastRecv
	dat.recvAmt = math.Clamp(dat.recvAmt - passed * ndb.AllowNetsPerSecond, 0, ndb.AmountLimit) + 1

	-- exceeded nets-per-second limit
	if dat.recvAmt >= ndb.AmountLimit then
		return false, "amount"
	end

	return true
end

function ndb.NetProcessed(ply, time, ok)
	if ndb.IgnoreNets[ndb.CurrentNet] or ndb.IgnorePlayers[ply] then return end -- don't care

	local toIncr = (not ok and ndb.ErrorCounts or 1) - 1
	local dat = getDat(ply)
	local ct = CurTime()

	local passed = math.max(0, ct - dat.lastRecv - time)
	-- do processing time decay first
	dat.procTime = math.max(0, dat.procTime - passed / ndb.ProcessingFrame)
	dat.lastRecv = ct
	dat.recvAmt = dat.recvAmt + toIncr

	-- then do the check
	if time > ndb.ProcessingThreshold / 1000 then
		dat.toBanFrom[ndb.CurrentNet] = true -- if the net was fairly expensive, in the event they violate we ban them from that net

		if dat.procTime > ndb.ProcessingLimit / 1000 then
			-- ban them from all semi-expensive nets they sent
			local banTill = ct + ndb.ViolatedIgnoreDuration
			dat.bannedFrom[ndb.CurrentNet] = banTill

			for k,v in pairs(dat.toBanFrom) do
				dat.bannedFrom[k] = banTill
			end

			-- not clearing `toBanFrom` on purpose; saving all expensive nets they ever sent
			ndb.Punish(ply, "processing")
			return
		end
	end

	-- then increment time
	dat.procTime = dat.procTime + time

	-- this allows for 1 lag spike (some nets might just randomly shit the bed) once in a while
	-- second laggy net-send will kill them though
end

local function xpcallErrorer(why)
	ErrorNoHalt(("Net Error: %s\n%s"):format(why, debug.traceback()))
end

local function netRecv(ply)
	local ok, why = ndb.NetReceived(ply)
	if not ok then
		if why then
			ndb.Punish(ply, why)
		end
		return false -- dont process net further
	end

	return true
end

function net.Incoming(len, ply)
	if CLIENT then
		-- Aye this isnt even supposed to run clientside
		ndb.netIncoming(len, ply)
		return
	end

	if isOnlyDetour() then
		-- ez, we just replicate gmod behavior
		local i = net.ReadHeader()
		local strName = util.NetworkIDToString( i )
		ndb.CurrentNet = strName

		-- if not ignoring dummy nets, run NetReceived before validity checks
		-- otherwise, run after

		if not ndb.IgnoreDummyNets and not netRecv(ply) then
			return
		end

		if not strName then return end

		local func = net.Receivers[ strName:lower() ]
		if not func then return end

		-- if ignoring dummy nets, run after checks
		if ndb.IgnoreDummyNets and not netRecv(ply) then
			return
		end

		len = len - 16

		local s1 = SysTime()
		local ok = xpcall(func, xpcallErrorer, len, ply)
		local s2 = SysTime()

		ndb.NetProcessed(ply, s2 - s1, ok)
		return
	end

	-- We're not the only net.Incoming detour

	-- We cant pop ReadHeader off the net message without fucking other addons up...
	-- A proper way would be to guarantee we're on top and store the net.ReadHeader result,
	-- but thats too much work just for a log message.

	-- Since we don't control net.Incoming, all we'll do is log call-count,
	-- delegate the work to the net.Incoming below us (which we captured on boot)
	-- and see how much time it took

	ndb.CurrentNet = "Unknown net -- not controlling net.Incoming!" -- heavy mald

	if not netRecv(ply) then return end

	local s1 = SysTime()
	local ok = xpcall(ndb.netIncoming, xpcallErrorer, len, ply)
	local s2 = SysTime()

	ndb.NetProcessed(ply, s2 - s1, ok)
end

myNetIncoming = net.Incoming
NetDebouncer.CurrentDetour = net.Incoming