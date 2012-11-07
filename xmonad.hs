import Data.Ratio ((%))
import XMonad
import XMonad.Actions.CopyWindow(copy)
import XMonad.Actions.DynamicWorkspaces
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.SimpleFloat
import XMonad.Layout.Tabbed
import XMonad.Layout.TwoPane
import XMonad.Prompt
import XMonad.Prompt.XMonad
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run(spawnPipe)
import System.IO


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
               
             , ("sublime", namedScratchpadAction scratchpads "sublime")
             ]
  in xmonadPromptC cmds c


--------------------------------------------------------------------------------
-- Key bindings
--
myKeys = [
  ((mod4Mask .|. shiftMask, xK_l), spawn "xscreensaver-command -lock"),
  ((mod4Mask, xK_d), spawn "dmenu_run"),

  -- Scratchpads
  ((mod4Mask, xK_s), namedScratchpadAction scratchpads "sublime"),

  -- Start the interactive prompt to ask for one of my custom-defined commands.
  ((mod4Mask, xK_x), myXmonadPrompt defaultXPConfig)

  -- Volume
  -- Implementing these here overrides gnome-settings-daemon's handling of them,
  -- and since that includes the notification feedback, I'm commenting these out
  -- and letting it do its thing.
  --
  -- ((0, 0x1008FF12), spawn "amixer -q set Master toggle"),  -- mute
  -- ((0, 0x1008FF11), spawn "amixer -q set Master 5%-"),     -- volume down
  -- ((0, 0x1008FF13), spawn "amixer -q set Master 5%+")      -- volume up
  ]


--------------------------------------------------------------------------------
-- Scratchpads
--
scratchpads = [
  NS "sublime" "sublime" (className =? "Sublime") (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))
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
    myCFloats = [ "Gimp", "Shutter", "Skype", "VirtualBox" ]
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
  imLayout |||
  TwoPane (3/100) (1/2) |||
  simpleTabbed |||
  simpleFloat
  )


--------------------------------------------------------------------------------
-- Main
--
main = do
  spawn "setxkbmap -option ctrl:nocaps"
  xmproc <- spawnPipe "/usr/bin/xmobar /home/tom/.xmobarrc"
  xmonad $ defaultConfig
   { modMask = mod4Mask
   , manageHook = manageDocks <+> myManageHook <+> namedScratchpadManageHook scratchpads
   , layoutHook = myLayout
   , logHook = dynamicLogWithPP xmobarPP
                   { ppOutput = hPutStrLn xmproc
                   , ppTitle = xmobarColor "green" "" . shorten 50
                   }
   } `additionalKeys` myKeys
