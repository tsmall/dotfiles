--[[
volumes.lua -- Automatic volume unmounting
==========================================

This script automatically unmounts my external hard drive whenever my
laptop switches from AC to battery power.

This code is borrowed from the config file asmagill graciously shared.
You can see his original here:
<https://github.com/asmagill/hammerspoon-config/blob/master/utils/_actions/battery_usbdrives.lua>
]]--

local alert   = require("hs.alert")
local battery = require("hs.battery")
local fs      = require("hs.fs")

local previousPowerSource = battery.powerSource()

local watcher = battery.watcher.new(function()
      local total, count = 0, 0
      local currentPowerSource = battery.powerSource()
      if currentPowerSource ~= previousPowerSource then
         if currentPowerSource ~= "AC Power" then
            for volume in fs.dir("/Volumes") do
               if volume == "My Passport" then
                  local _, _, _, rc = hs.execute("diskutil umount '" .. volume .. "'")
                  total = total + 1
                  if tonumber(rc) == 0 then count = count + 1 end
               end
            end
            if total > 0 then
               alert.show("Auto unmounted " .. tostring(count) .. " of " .. tostring(total))
            end
         end
         previousPowerSource = currentPowerSource
      end
end)

watcher:start()
