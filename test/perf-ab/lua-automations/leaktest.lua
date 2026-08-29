-- Minimal automation used for the perf A/B leak test.
--
-- setup() subscribes to a topic nobody publishes to, and loop()
-- blocks on the subscription channel, so the automation sits idle
-- (no CPU burn) until it is stopped. Every start/stop cycle exercises
-- the full Lua state lifecycle (mkRunAutomation + mkCleanupAutomation,
-- i.e. one Lua interpreter state each in the pre-fix build) plus two
-- SQLite state-persistence writes (StateManager updateRunning runs on
-- both Start and Stop).

chan = nil

function setup ()
   logDebugMsg("leaktest: setup")
   chan = subscribe("perf-ab/quiet")
end

function loop ()
   -- blocks until a message arrives on the subscribed topic; nobody
   -- publishes there, so this idles until the automation is stopped
   local msg = chan()
   logDebugMsg("leaktest: got msg " .. tostring(msg))
end

function cleanup ()
   logDebugMsg("leaktest: cleanup")
end