import Data.Ratio ((%))
import System.IO
import Text.Printf (printf)
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
import XMonad.Layout.TwoPane
import XMonad.Prompt
import XMonad.Prompt.XMonad
import XMonad.Util.EZConfig (additionalKeys, mkKeymap)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (spawnPipe)

import qualified Data.Map as M
import qualified XMonad.Actions.Submap as SM
import qualified XMonad.StackSet as W


--------------------------------------------------------------------------------
-- Chat settings
--
useChatWorkspace = False
chatWorkspace = "8:chat"

-- Hangouts Chrome Extension
chatClass = "Google-chrome"
chatRosters = (ClassName chatClass) `And` (Title "Hangouts")
isChatWindow = (className =? chatClass <&&>
                stringProperty "WM_WINDOW_ROLE" =? "pop-up")

-- Instantbird
-- chatClass = "Instantbird"
-- chatRosters = (ClassName chatClass `And` Title "Instantbird")
-- isChatWindow = (className =? chatClass)


-------------------------------------------------------------------------------
-- Helper Functions
--
runElisp :: String -> X ()
runElisp = spawn . printf "emacsclient --eval '%s'"

toggleVolume :: X ()
toggleVolume = toggle ["Headphone", "Master"]
  where toggle outputs = mapM_ spawn $ map amixerCmd outputs
        amixerCmd = printf "amixer -q set %s toggle"

lockScreen :: X ()
lockScreen = spawn "xscreensaver-command -lock"


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
myTerminal = "/usr/bin/urxvtcd"


-------------------------------------------------------------------------------
-- Workspaces
--
myWorkspaces = namedWorkspaces ++ map show [3..7] ++ finalWorkspaces
  where namedWorkspaces = ["1:web","2:code"]
        finalWorkspaces = if useChatWorkspace
                          then [chatWorkspace,"9"]
                          else map show [8,9]

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

             , ("lock", lockScreen)
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
runOrRaiseAurora   = runOrRaise "aurora-dev"       (className =? "Aurora")
runOrRaiseFirefox  = runOrRaise "firefox"          (className =? "Firefox")
runOrRaiseChrome   = runOrRaise "google-chrome"    (className =? "Google-chrome")
runOrRaiseChromium = runOrRaise "chromium-browser" (className =? "Chromium-browser")
runOrRaiseEmacs    = runOrRaise "emacs"            (className =? "Emacs")
runOrRaiseGterm    = runOrRaise "gnome-terminal"   (className =? "Gnome-terminal")
runOrRaiseUrxvt    = runOrRaise "urxvtcd"          (className =? "URxvt")


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
         , ((myModMask, xK_i), runOrRaiseKeymap)
         , ((myModMask, xK_g), sendMessage $ ToggleGaps)
         , ((myModMask, xK_o), toggleWS)
         , ((myModMask, xK_p), pomodoroKeymap)
         , ((myModMask, xK_x), myXmonadPrompt defaultXPConfig)

         , ((myModMask .|. shiftMask, xK_comma), mapM_ sendMessage [IncGap 50 R, IncGap 50 L])
         , ((myModMask .|. shiftMask, xK_period), mapM_ sendMessage [DecGap 50 R, DecGap 50 L])
         , ((myModMask .|. shiftMask, xK_l), lockScreen)

           -- Quick App Shortcuts
         , ((myModMask, xK_F9),  spRdio)
         , ((myModMask, xK_F10), spGoogleMusic)
         , ((myModMask, xK_F11), spPandora)
         , ((myModMask, xK_F12), spFocusAtWill)

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
         , ((myModMask, xK_a), sendMessage MirrorExpand)
         , ((myModMask, xK_z), sendMessage MirrorShrink)
         ]
  where pomodoroKeymap = SM.submap . M.fromList $ [
          ((0, xK_p), pomodoroStart),
          ((0, xK_s), pomodoroStartShortBreak),
          ((0, xK_l), pomodoroStartLongBreak),
          ((0, xK_r), pomodoroRemainingTime)
          ]
        runOrRaiseKeymap = SM.submap . M.fromList $ [
          ((0, xK_a),         runOrRaiseAurora),
          ((0, xK_c),         runOrRaiseChrome),
          ((shiftMask, xK_c), runOrRaiseChromium),
          ((0, xK_e),         runOrRaiseEmacs),
          ((0, xK_f),         runOrRaiseFirefox),
          ((0, xK_g),         runOrRaiseGterm),
          ((0, xK_k),         runOrRaise "conkeror" (className =? "Conkeror")),
          ((shiftMask, xK_2), spFocusAtWill),
          ((0, xK_m),         spGoogleMusic),
          ((0, xK_p),         spPandora),
          ((0, xK_r),         spRdio),
          ((0, xK_u),         runOrRaiseUrxvt)
          ]


--------------------------------------------------------------------------------
-- Scratchpads
--
scratchpads = map webappScratchpad [ "focus-at-will"
                                   , "google-music"
                                   , "pandora"
                                   , "rdio"
                                   ]
  where
    webappScratchpad exe = NS exe exe (className =? exe) scratchpadFloat
    scratchpadFloat = (customFloating $ W.RationalRect 0.1 0.1 0.8 0.8)


runScratchpad :: String -> X ()
runScratchpad = namedScratchpadAction scratchpads

spFocusAtWill = runScratchpad "focus-at-will"
spGoogleMusic = runScratchpad "google-music"
spPandora     = runScratchpad "pandora"
spRdio        = runScratchpad "rdio"


--------------------------------------------------------------------------------
-- Window rules
--
-- To find the property name associated with a program, use
-- `xprop | grep WM_CLASS` and click on the client you're interested in.
--
myShiftHooks = [ isChatWindow --> doShift chatWorkspace ]

myFloatHooks = concat $
  [ [ className =? c --> doFloat | c <- myCFloats ]
  , [ title     =? t --> doFloat | t <- myTFloats ]
  , [(className =? "Firefox" <&&> resource =? "Dialog") --> doFloat]
  ]
  where
    myCFloats = [ "B2g"
                , "MPlayer"
                , "Nitrogen"
                , "Shutter"
                , "Skype"
                , "xpad"
                ]
    myTFloats = [ "About Aurora"
                , "About Mozilla Firefox"
                , "Aurora Preferences"
                , "Firefox Preferences"
                , "Downloads"
                , "Library"
                ]
myFullscreenHooks = [isFullscreen --> (doF W.focusDown <+> doFullFloat)]
myManageHook = composeAll (myShiftHooks ++ myFloatHooks ++ myFullscreenHooks)


--------------------------------------------------------------------------------
-- Layouts
--
imLayout = withIM ratio chatRosters chatLayout where
  ratio = 1%7
  chatLayout = Grid

myTall = Tall 1 (3/100) (1/2)

myLayout = avoidStruts (ResizableTall 1 (3/100) (1/2) []
                        ||| Mirror (myTall)
                        ||| simpleTabbed
                        ||| Full
                        ||| TwoPane (3/100) (1/2)
                       )

myLayoutHook = smartBorders $ avoidStruts $ (
  onWorkspace (myWorkspaces !! 0) (gaps [(L, 250), (R, 250)] myLayout) $
  onWorkspace chatWorkspace imLayout $
  smartBorders (myLayout)
  )


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
