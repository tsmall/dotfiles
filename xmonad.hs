import Data.Ratio ((%))
import XMonad
import XMonad.Actions.CopyWindow(copy)
import XMonad.Actions.DynamicWorkspaces
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Prompt
import XMonad.Prompt.XMonad
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.Run(spawnPipe)
import System.IO


--------------------------------------------------------------------------------
-- Workspaces
-- The default number of workspaces (virtual screens) and their names.
--
-- myWorkspaces = ["1:notes","2:web","3:code","4:vm","5","6","7","8:media"] ++ map show [6..9]


--------------------------------------------------------------------------------
-- Promptable actions
--
myXmonadPrompt :: XPConfig -> X ()
myXmonadPrompt c =
  let cmds = [ ("selectWorkspace", selectWorkspace defaultXPConfig)
             , ("moveToWorkspace", withWorkspace defaultXPConfig (windows . W.shift))
             , ("copyToWorkspace", withWorkspace defaultXPConfig (windows . copy))
             , ("renameWorkspace", renameWorkspace defaultXPConfig)
             , ("removeWorkspace", removeWorkspace)
               
             , ("connectMonitor", spawn "xrandr --output VGA1 --mode '1920x1080' --above LVDS1")
             , ("disconnectMonitor", spawn "xrandr --output VGA1 --off")
             ]
  in xmonadPromptC cmds c


--------------------------------------------------------------------------------
-- Key bindings
--
myKeys = [
  -- Lock the screen.
  ((mod4Mask .|. shiftMask, xK_l), spawn "gnome-screensaver-command --lock"),

  -- Start the interactive prompt to ask for one of my custom-defined commands.
  ((mod4Mask, xK_x), myXmonadPrompt defaultXPConfig),

  -- Mute volume.
  ((0, 0x1008FF12), spawn "amixer -q set Master toggle"),

  -- Decrease volume.
  ((0, 0x1008FF11), spawn "amixer -q set Master 5%-"),

  -- Increase volume.
  ((0, 0x1008FF13), spawn "amixer -q set Master 5%+")
  ]


--------------------------------------------------------------------------------
-- Window rules
--
-- To find the property name associated with a program, use
-- `xprop | grep WM_CLASS` and click on the client you're interested in.
myManageHook = composeAll . concat $
  [ [ className =? c --> doFloat | c <- myCFloats ],
    [ title     =? t --> doFloat | t <- myTFloats ] ]
  where
    myCFloats = [ "Gimp", "Shutter", "VirtualBox" ]
    myTFloats = [ "Aurora Preferences"
                , "About Aurora"
                , "Firefox Preferences"
                , "Downloads"
                , "Cookies"
                , "Library"
                ]


--------------------------------------------------------------------------------
-- Layouts
--
imLayout = withIM ratio rosters chatLayout where
  ratio = 1%7
  rosters = (ClassName "Empathy") `And` (Role "contact_list")
  chatLayout = Grid

myLayout = avoidStruts (
  Tall 1 (3/100) (1/2) |||
  Mirror (Tall 1 (3/100) (1/2)) |||
  Full |||
  imLayout
  )


--------------------------------------------------------------------------------
-- Main
--
main = do
  spawn "setxkbmap -option ctrl:nocaps"
  xmproc <- spawnPipe "/usr/bin/xmobar /home/tom/.xmobarrc"
  xmonad $ defaultConfig
   { modMask = mod4Mask
   , manageHook = manageDocks <+> myManageHook
   , layoutHook = myLayout
   , logHook = dynamicLogWithPP xmobarPP
                   { ppOutput = hPutStrLn xmproc
                   , ppTitle = xmobarColor "green" "" . shorten 50
                   }
   } `additionalKeys` myKeys
