import Data.Ratio ((%))
import Text.Printf (printf)
import XMonad
import XMonad.Actions.CopyWindow(copy)
import XMonad.Actions.DynamicWorkspaces
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers(isFullscreen, doFullFloat)
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.NoBorders(smartBorders)
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


-------------------------------------------------------------------------------
-- Helper Functions
--
runElisp :: String -> X ()
runElisp elisp = spawn $ printf "emacsclient --eval '%s'" elisp


-------------------------------------------------------------------------------
-- Terminal
--
myTerminal = "/usr/bin/urxvt"


-------------------------------------------------------------------------------
-- Workspaces
--
chatWorkspace = "4:chat"
myWorkspaces = ["1:web","2:code","3:ref",chatWorkspace] ++ map show [5..9]


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

             , ("pomodoroStart", runElisp "(pomodoro-start)")
             , ("pomodoroStartShortBreak", runElisp "(pomodoro-start-short-break)")
             , ("pomodoroStartLongBreak", runElisp "(pomodoro-start-long-break)")
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
--
myShiftHooks = [ className =? "Instantbird" --> doShift chatWorkspace ]
myFloatHooks = concat $
  [ [ className =? c --> doFloat | c <- myCFloats ]
  , [ title     =? t --> doFloat | t <- myTFloats ]
  , [(className =? "Firefox" <&&> resource =? "Dialog") --> doFloat]
  ]
  where
    myCFloats = [ "Gimp", "MPlayer", "Shutter", "Skype", "VirtualBox", "xpad" ]
    myTFloats = [ "About Aurora"
                , "Downloads"
                ]
myFullscreenHooks = [isFullscreen --> (doF W.focusDown <+> doFullFloat)]
myManageHook = composeAll (myShiftHooks ++ myFloatHooks ++ myFullscreenHooks)


--------------------------------------------------------------------------------
-- Layouts
--
imLayout = withIM ratio rosters chatLayout where
  ratio = 1%7
  rosters = (ClassName "Instantbird") `And` (Title "Instantbird")
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
xinit :: IO ()
xinit = do
  spawn "setxkbmap -option ctrl:nocaps"
  spawn "xsetroot -cursor_name left_ptr"
  spawn "synclient TapButton1=0 TapButton2=0 TapButton3=0"
  spawn "xrdb -merge ~/.Xdefaults"

main = do
  xinit
  xmproc <- spawnPipe "/usr/bin/xmobar /home/tom/.xmobarrc"
  xmonad $ withUrgencyHook NoUrgencyHook defaultConfig
   { modMask = mod4Mask
   , workspaces = myWorkspaces
   , terminal = myTerminal
   , manageHook = manageDocks <+> myManageHook <+> namedScratchpadManageHook scratchpads
   , layoutHook = smartBorders (myLayout)
   , handleEventHook = fullscreenEventHook
   , logHook = dynamicLogWithPP xmobarPP
                   { ppOutput = hPutStrLn xmproc
                   , ppTitle = xmobarColor "green" "" . shorten 50
                   , ppUrgent = xmobarColor "yellow" "red" . xmobarStrip
                   }
   } `additionalKeys` myKeys
