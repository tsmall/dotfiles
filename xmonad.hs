import Data.Ratio ((%))
import Text.Printf (printf)
import System.IO
import XMonad
import XMonad.Actions.CopyWindow (copy)
import XMonad.Actions.CycleWS (toggleWS)
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.WindowGo (runOrRaise)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat)
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Gaps
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.PerWorkspace
import XMonad.Layout.ResizableTile
import XMonad.Layout.SimpleFloat
import XMonad.Layout.Tabbed
import XMonad.Prompt
import XMonad.Prompt.XMonad
import XMonad.Util.EZConfig (additionalKeys, mkKeymap)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (spawnPipe)

import qualified Data.Map as M
import qualified XMonad.Actions.Submap as SM
import qualified XMonad.StackSet as W


-------------------------------------------------------------------------------
-- Helper Functions
--
runElisp :: String -> X ()
runElisp = spawn . printf "emacsclient --eval '%s'"

toggleVolume :: X ()
toggleVolume = toggle ["Headphone", "Master"]
  where toggle outputs = mapM_ spawn $ map amixerCmd outputs
        amixerCmd = printf "amixer -q set %s toggle"


--------------------------------------------------------------------------------
-- Pomodoro
--
pomodoroStart = runElisp "(pomodoro-start)"
pomodoroStartShortBreak = runElisp "(pomodoro-start-short-break)"
pomodoroStartLongBreak = runElisp "(pomodoro-start-long-break)"
pomodoroRemainingTime = runElisp "(pomodoro-remaining-time)"


-------------------------------------------------------------------------------
-- Modifier Key
--
myModMask :: KeyMask
myModMask = mod4Mask


-------------------------------------------------------------------------------
-- Terminal
--
myTerminal = "/usr/bin/urxvt"


-------------------------------------------------------------------------------
-- Workspaces
--
chatWorkspace = "2:chat"
myWorkspaces = ["1:web",chatWorkspace,"3:code"] ++ map show [4..9]


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

             , ("lock", spawn "xscreensaver-command -lock")
             , ("sleep", spawn "sudo pm-suspend")

             , ("pomodoroStart", pomodoroStart)
             , ("pomodoroStartShortBreak", pomodoroStartShortBreak)
             , ("pomodoroStartLongBreak", pomodoroStartLongBreak)
             , ("pomodoroRemainingTime", pomodoroRemainingTime)
             ]
  in xmonadPromptC cmds c


--------------------------------------------------------------------------------
-- Run or Raise shortcuts
--
runOrRaiseAurora = runOrRaise "aurora" (className =? "Firefox")
runOrRaiseChrome = runOrRaise "google-chrome" (className =? "Google-chrome")
runOrRaiseEmacs = runOrRaise "emacs" (className =? "Emacs")


--------------------------------------------------------------------------------
-- Key bindings
--
xK_XF86AudioMute = 0x1008FF12
xK_XF86AudioLowerVolume = 0x1008FF11
xK_XF86AudioRaiseVolume = 0x1008FF13
xK_XF86AudioPlay = 0x1008FF14
xK_XF86AudioStop = 0x1008FF15
xK_XF86AudioPrev = 0x1008FF16
xK_XF86AudioNext = 0x1008FF17

myKeys = [ ((myModMask, xK_d), spawn "dmenu_run")
         , ((myModMask .|. shiftMask, xK_l), launchKeymap)
         , ((myModMask, xK_g), sendMessage $ ToggleGaps)
         , ((myModMask, xK_o), toggleWS)
         , ((myModMask, xK_p), pomodoroKeymap)
         , ((myModMask, xK_s), scratchpadKeymap)
         , ((myModMask, xK_x), myXmonadPrompt defaultXPConfig)

         , ((myModMask .|. shiftMask, xK_comma), mapM_ sendMessage [IncGap 50 R, IncGap 50 L])
         , ((myModMask .|. shiftMask, xK_period), mapM_ sendMessage [DecGap 50 R, DecGap 50 L])

           -- Quick App Shortcuts
         , ((myModMask, xK_F1), runOrRaiseEmacs)
         , ((myModMask, xK_F2), runOrRaiseChrome)
         , ((myModMask, xK_F3), runOrRaiseAurora)

         , ((myModMask, xK_F9),  namedScratchpadAction scratchpads "pandora")
         , ((myModMask, xK_F10), namedScratchpadAction scratchpads "rdio")
         , ((myModMask, xK_F11), namedScratchpadAction scratchpads "google-music")

           -- Volume
         , ((0, xK_XF86AudioMute), toggleVolume)
         , ((0, xK_XF86AudioLowerVolume), spawn "amixer -q set Master 5%-")
         , ((0, xK_XF86AudioRaiseVolume), spawn "amixer -q set Master 5%+")

           -- Music Control
         , ((0, xK_XF86AudioPlay), spawn "mpc toggle")
         , ((0, xK_XF86AudioStop), spawn "mpc pause")
         , ((0, xK_XF86AudioPrev), spawn "mpc prev")
         , ((0, xK_XF86AudioNext), spawn "mpc next")

           -- Resizable Tile
         , ((myModMask, xK_a), sendMessage MirrorShrink)
         , ((myModMask, xK_z), sendMessage MirrorExpand)
         ]
  where launchKeymap = SM.submap . M.fromList $ [
          ((0, xK_a), runOrRaiseAurora),
          ((0, xK_c), runOrRaiseChrome),
          ((0, xK_e), runOrRaiseEmacs)
          ]
        scratchpadKeymap = SM.submap . M.fromList $ [
          ((0, xK_g), namedScratchpadAction scratchpads "google-music"),
          ((0, xK_p), namedScratchpadAction scratchpads "pandora"),
          ((0, xK_r), namedScratchpadAction scratchpads "rdio")
          ]
        pomodoroKeymap = SM.submap . M.fromList $ [
          ((0, xK_p), pomodoroStart),
          ((0, xK_s), pomodoroStartShortBreak),
          ((0, xK_l), pomodoroStartLongBreak),
          ((0, xK_r), pomodoroRemainingTime)
          ]


--------------------------------------------------------------------------------
-- Scratchpads
--
scratchpads = [ webappScratchpad "google-music"
              , webappScratchpad "pandora"
              , webappScratchpad "rdio"
              ]
  where
    webappScratchpad exe = NS exe exe (className =? exe) scratchpadFloat
    scratchpadFloat = (customFloating $ W.RationalRect 0.1 0.1 0.8 0.8)


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
    myCFloats = [ "B2g", "Gimp", "MPlayer", "Shutter", "Skype", "VirtualBox", "xpad" ]
    myTFloats = [ "About Aurora"
                , "Aurora Preferences"
                , "Downloads"
                , "Library"
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

myTall = Tall 1 (3/100) (1/2)

myLayout = avoidStruts (ResizableTall 1 (3/100) (1/2) []
                        ||| Mirror (myTall)
                        ||| Full
                        ||| simpleTabbed
                        ||| imLayout
                       )

myLayoutHook = smartBorders $
               onWorkspace (myWorkspaces !! 0) (gaps [(L, 250), (R, 250)] myLayout) $
               smartBorders (myLayout)


--------------------------------------------------------------------------------
-- Main
--
xinit :: IO ()
xinit = mapM_ spawn [ "setxkbmap -option ctrl:nocaps"
                    , "xsetroot -cursor_name left_ptr"
                    , "synclient TapButton1=0 TapButton2=0 TapButton3=0"
                    , "xrdb -merge ~/.Xdefaults"
                    ]

main = do
  xinit
  xmproc <- spawnPipe "/usr/bin/xmobar /home/tom/.xmobarrc"
  xmonad $ withUrgencyHook NoUrgencyHook defaultConfig
   { modMask = myModMask
   , workspaces = myWorkspaces
   , terminal = myTerminal
   , manageHook = manageDocks <+> myManageHook <+> namedScratchpadManageHook scratchpads
   , layoutHook = myLayoutHook
   , handleEventHook = fullscreenEventHook
   , logHook = dynamicLogWithPP xmobarPP
                   { ppOutput = hPutStrLn xmproc
                   , ppTitle = xmobarColor "green" "" . shorten 50
                   , ppUrgent = xmobarColor "yellow" "red" . xmobarStrip
                   }
   } `additionalKeys` myKeys
