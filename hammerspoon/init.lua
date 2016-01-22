--------------------------------------------------------------------------------
-- Imports

local alert       = require "hs.alert"
local application = require "hs.application"
local caffeinate  = require "hs.caffeinate"
local grid        = require "hs.grid"
local hints       = require "hs.hints"
local hotkey      = require "hs.hotkey"
local mouse       = require "hs.mouse"
local screen      = require "hs.screen"
local window      = require "hs.window"

--------------------------------------------------------------------------------
-- Grid

grid.setGrid("6x8")
grid.setMargins("2x2")

local gw = grid.GRIDWIDTH;
local gh = grid.GRIDHEIGHT;

local goMiddle = { x = 1,    y = 1,    w = 4,    h = 6    }
local goLeft   = { x = 0,    y = 0,    w = gw/2, h = gh   }
local goRight  = { x = gw/2, y = 0,    w = gw/2, h = gh   }
local goUp     = { x = 0,    y = 0,    w = gw,   h = gh/2 }
local goDown   = { x = 0,    y = gh/2, w = gw,   h = gh/2 }
local goBig    = { x = 0,    y = 0,    w = gw,   h = gh   }

local goLeftThird  = { x = 0,    y = 0, w = (gw/3 * 2), h = gh }
local goRightThird = { x = gw/3, y = 0, w = (gw/3 * 2), h = gh }

local goTopLeft     = { x = 0,    y = 0,    w = gw/2, h = gh/2 }
local goTopRight    = { x = gw/2, y = 0,    w = gw/2, h = gh/2 }
local goBottomLeft  = { x = 0,    y = gh/2, w = gw/2, h = gh/2 }
local goBottomRight = { x = gw/2, y = gh/2, w = gw/2, h = gh/2 }

local gridset = function(frame)
   return function()
      local win = window.focusedWindow()
      if win then
         grid.set(win, frame, win:screen())
      else
         alert.show("No focused window.")
      end
   end
end

--------------------------------------------------------------------------------
-- Window hints

local windowHintsMoveMouse = function()
   local callback = function(win)
      mouse.setAbsolutePosition(win:topLeft())
   end
   hints.windowHints(nil, callback)
end

--------------------------------------------------------------------------------
-- Launcher

local launcher = hs.hotkey.modal.new()

local runOrRaise = function(appName)
   return function()
      if application.launchOrFocus(appName) then
         local win = window.focusedWindow()
         mouse.setAbsolutePosition(win:topLeft())
      end
      launcher:exit()
   end
end

-- Opens a directory in Finder. The directory has to be a favorite so it
-- shows up in the "Go" menu.
--
local openDirectory = function(dirName)
   return function()
      if application.launchOrFocus("finder") then
         local app = application.frontmostApplication()
         app:selectMenuItem({"Go", dirName})
      end
      launcher:exit()
   end
end

launcher:bind("", "c", runOrRaise("google chrome"))
launcher:bind("", "d", openDirectory("Downloads"))
launcher:bind("", "e", runOrRaise("/Users/tsmall/Applications/Emacs.app"))
launcher:bind("", "f", runOrRaise("firefox"))
launcher:bind("", "h", runOrRaise("hipchat"))
launcher:bind("", "m", runOrRaise("textmate"))
launcher:bind("", "o", runOrRaise("/Applications/Microsoft Office 2011/Microsoft Outlook.app"))
launcher:bind("", "s", runOrRaise("sequel pro"))
launcher:bind("", "t", runOrRaise("terminal"))

--------------------------------------------------------------------------------
-- Screens

local function getScreen(name)
   if name == "left" then
      return screen.allScreens()[2]
   elseif name == "right" then
      return screen.allScreens()[3]
   else
      return screen.allScreens()[1]
   end
end

local function moveToScreen(name)
   return function()
      local win = window.focusedWindow()
      local scr = getScreen(name)
      win:moveToScreen(scr)
   end
end

--------------------------------------------------------------------------------
-- Key binding

local mod      = {"alt"}
local modShift = {"alt", "shift"}
local modCtrl  = {"alt", "ctrl"}

hotkey.bind(mod, "e", windowHintsMoveMouse)

hotkey.bind(mod, "1", moveToScreen("left"))
hotkey.bind(mod, "2", moveToScreen("center"))
hotkey.bind(mod, "3", moveToScreen("right"))

hotkey.bind(mod, "h", gridset(goLeft))
hotkey.bind(mod, "j", gridset(goDown))
hotkey.bind(mod, "k", gridset(goUp))
hotkey.bind(mod, "l", gridset(goRight))
hotkey.bind(mod, "n", gridset(goMiddle))
hotkey.bind(mod, "m", gridset(goBig))

hotkey.bind(modCtrl, "h", grid.pushWindowLeft)
hotkey.bind(modCtrl, "j", grid.pushWindowDown)
hotkey.bind(modCtrl, "k", grid.pushWindowUp)
hotkey.bind(modCtrl, "l", grid.pushWindowRight)

hotkey.bind(modShift, "h", gridset(goLeftThird))
hotkey.bind(modShift, "l", gridset(goRightThird))

hotkey.bind(mod, "q", gridset(goTopLeft))
hotkey.bind(mod, "w", gridset(goTopRight))
hotkey.bind(mod, "a", gridset(goBottomLeft))
hotkey.bind(mod, "s", gridset(goBottomRight))

hotkey.bind(mod,      "[", grid.resizeWindowThinner)
hotkey.bind(mod,      "]", grid.resizeWindowWider)
hotkey.bind(modShift, "[", grid.resizeWindowShorter)
hotkey.bind(modShift, "]", grid.resizeWindowTaller)

hotkey.bind(mod, "Escape", caffeinate.startScreensaver)

hotkey.bind(mod, "i", function() launcher:enter() end)
