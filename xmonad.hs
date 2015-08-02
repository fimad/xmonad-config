{-# LANGUAGE UnicodeSyntax #-}
import Data.Ratio ((%))
import System.Environment
import System.IO
import System.Process
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.NoBorders
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ICCCMFocus
import XMonad.Hooks.ManageDocks
import XMonad.Layout
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.LayoutHints
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.Spacing
import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Util.WorkspaceCompare

import qualified Data.Map as M
import qualified XMonad.Actions.PhysicalScreens as PS
import qualified XMonad.StackSet as W


--------------------------------------------------------------------------------
-- View Layouts

myLayout = avoidStruts
         $ onWorkspace "gimp" gimpLayout
         $ onWorkspace "wine" fsLayout
         $ standardLayout
    where
        spaced = spacing 10

        tall = spaced $ Tall 1 (3 / 100) (1 / 2)

        standardLayout =   fsLayout
                       ||| named "tall" tall
                       ||| named "wide" (Mirror tall)
                       ||| named "grid" (spaced Grid)

        fsLayout = named "full" $ noBorders Full

        gimpLayout = named "gimp"
                   $ withIM (1%7) (Role "gimp-toolbox")
                   $ reflectHoriz
                   $ withIM (1%7) (Role "gimp-dock") Full


--------------------------------------------------------------------------------
-- Workspaces

myWorkspaces :: [String]
myWorkspaces = [
        "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"
    ,   "gimp", "wine"
    ]

myWorkspaceKeys :: [KeySym]
myWorkspaceKeys = [
        xK_1, xK_2, xK_3, xK_4, xK_5, xK_6, xK_7, xK_8, xK_9
    ,   xK_0, xK_minus, xK_equal
    ]


{-------------------------------------------------------------------------------
  - Handle all the applications that fuck up when they are run in a tiled window
  - manager
-------------------------------------------------------------------------------}
myManager = composeAll [
    title =? "xfce4-notifyd" --> doIgnore
  , title =? "html-hud" --> doFloat
  , className =? "stalonetray" --> doIgnore
  , className =? "net-minecraft-LauncherFrame" --> doFloat

  -- Attempt to shove all of the instant messaging programs on to the IM space
  , className =? "Pidgin" --> doShift "IM"
  , className =? "Instantbird" --> doShift "IM"
  , className =? "Skype" --> doShift "IM"

  -- Wine gets a whole space to its self it's so bad
  , title =? "exe" --> doIgnore
  , className =? "mb_warband.exe" --> doIgnore
  , className =? "Wine" --> doFloat
  , className =? "Wine" --> doWinify

  -- Gimp
  , className =? "Gimp" --> doShift "Gimp"
  , stringProperty "WM_WINDOW_ROLE"  =? "gimp-scale-tool" --> doFloat
  , stringProperty "WM_WINDOW_ROLE"  =? "gimp-shear-tool" --> doFloat
  , stringProperty "WM_WINDOW_ROLE"  =? "gimp-rotate-tool" --> doFloat
  , stringProperty "WM_WINDOW_ROLE"  =? "gimp-perspective-tool" --> doFloat
  , stringProperty "WM_WINDOW_ROLE"  =? "gimp-flip-tool" --> doFloat
  , stringProperty "WM_WINDOW_ROLE"  =? "gimp-layer-new" --> doFloat
  ]
  where doWinify = doF (W.shift "Wine")

{-------------------------------------------------------------------------------
  - Key bindings
-------------------------------------------------------------------------------}

spawnWebApp url = spawn $ "google-chrome --app='" ++ url ++ "'"

spawnChromeApp app =  spawn
                   $  "google-chrome "
                   ++ "--app-id=" ++ app ++ " "
                   ++ "--profile-directory=Default"

chromeShellApp = "pnhechapfaindjhompbnflcldabbghjo"
chromeRemoteApp =  "gbchcmhmhahfdphkhkmpfmihenigjmpp"

myAdditionalKeys = [
  -- Print screen
    ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
  , ((0, xK_Print), spawn "scrot")

  -- Commonly run programs
  , ((controlMask .|. mod1Mask, xK_bracketright), spawn "thunar")
  , ((controlMask .|. mod1Mask, xK_bracketleft), spawn "google-chrome")
  , ((controlMask .|. mod1Mask, xK_apostrophe), spawn "wicd-client -n")
  , ((controlMask .|. mod1Mask, xK_Return), spawn "urxvt")
  , ((controlMask .|. mod1Mask, xK_backslash), spawn "gvim")
  , ((mod4Mask, xK_p), spawn "dmenu_run -nb '#002b36' -nf '#839496' -sb '#073642' -sf '#93a1a1' -fn '-*-fixed-*-*-*-*-*-*-*-*-*-*-*-*'")
  -- Don't use dmenu_run, using hmenu_run
  -- , ((mod4Mask, xK_p), spawn "hmenu_run")

  -- Common web apps
  , ((controlMask .|. mod4Mask, xK_i), spawnWebApp "http://inbox.google.com")
  , ((controlMask .|. mod4Mask, xK_m), spawnWebApp "http://music.google.com")
  , ((controlMask .|. mod4Mask, xK_d), spawnWebApp "http://drive.google.com")
  , ((controlMask .|. mod4Mask, xK_c), spawnWebApp "http://calendar.google.com")
  , ((controlMask .|. mod4Mask, xK_k), spawnWebApp "http://keep.google.com")
  , ((controlMask .|. mod4Mask, xK_f), spawnWebApp "http://facebook.com")

  -- Common chrome apps
  , ((controlMask .|. mod4Mask, xK_Return), spawnChromeApp chromeShellApp)
  , ((controlMask .|. mod4Mask, xK_r), spawnChromeApp chromeRemoteApp)

  -- Audio Key configuration
  , ((controlMask .|. mod1Mask, xK_Page_Up), spawn "~/.xmonad/bin/pulse_control.pl -inc")
  , ((controlMask .|. mod1Mask, xK_Page_Down), spawn "~/.xmonad/bin/pulse_control.pl -dec")
  , ((controlMask .|. mod1Mask, xK_End), spawn "~/.xmonad/bin/pulse_control.pl -toggle")

  -- Screen brightness configuration
  , ((controlMask .|. mod4Mask, xK_Page_Up), spawn "xbacklight -inc 10")
  , ((controlMask .|. mod4Mask, xK_Page_Down), spawn "xbacklight -dec 10")

  -- Useful key strokes for dealing with apps that should go full screen but
  -- don't really.
  , ((mod4Mask, xK_b), withFocused toggleBorder )
  , ((mod4Mask, xK_f), (do
                      withFocused float
                      withFocused rmBorder
                      spawn "fs-window")
    )
  ] ++
  -- Generate the key bindings for all of our workspaces
  (
    concat
    $ map ( \(key,view) ->
            [
                -- Switch the primary screen to the view
                ((mod4Mask, key), windows $ W.greedyView view)
                -- Switch the secondary screen to the view
              , ((controlMask .|. mod4Mask, key), switchSecondScreen view)
            ]
          )
    $ zip myWorkspaceKeys myWorkspaces
  )


{-------------------------------------------------------------------------------
  - Main method
  - Put all the pieces together
-------------------------------------------------------------------------------}
main = do
  home <- getEnv "HOME"
  runCommand (home ++ "/.xmonad/init.sh")
  h <- spawnPipe "xmobar -x 1"
  xmonad $ ewmh $ defaultConfig {
      focusedBorderColor = "#cb4b16"
    , normalBorderColor = "#002b36"
    , borderWidth = 0
    , handleEventHook = fullscreenEventHook
    , manageHook = manageDocks <+> myManager
    , layoutHook = myLayout
    , workspaces = myWorkspaces
    , modMask = mod4Mask -- Use the window key
    , logHook = takeTopFocus >> (dynamicLogWithPP $ printStatusBar h)
  } `removeKeys` map fst myAdditionalKeys `additionalKeys` myAdditionalKeys

printStatusBar :: Handle -> PP
printStatusBar h = defaultPP {
        ppCurrent = (\c -> concat ["<fc=#fdf6e3,#93a1a1> ",c,"</fc>"])
      , ppVisible = (\c -> concat ["<fc=#073642,#93a1a1> (",c,")</fc>"])
      , ppHidden = (\c -> concat ["<fc=#073642,#93a1a1> ",c,"</fc>"])
      , ppTitle   = (\c -> concat [
                "<fc=#b58900,#073642>", leftSep
            ,   "<fc=#839496,#073642> ", c, " "
            ,   "<fc=#073642,#002b36>", leftSep
            ,   "</fc></fc></fc>"
            ])
      , ppLayout  = (\c -> concat [
                " <fc=#93a1a1,#b58900>", leftSep
            ,   "<fc=#fdf6e3,#b58900>", c, "</fc></fc>"
            ])
      , ppWsSep = ""
      , ppSep = ""
      , ppOutput = hPutStrLn h
    }
    where
        leftSep = "\57520"


{-------------------------------------------------------------------------------
  - Utility functions
-------------------------------------------------------------------------------}

-- Switches the second screen's current workspace to v
switchSecondScreen:: String -> X ()
switchSecondScreen v = do
  PS.viewScreen 1
  windows $ W.greedyView v
  PS.viewScreen 0

-- Remove the border from a window
rmBorder :: Window -> X ()
rmBorder w = do
  withDisplay $ \d -> io $ setWindowBorderWidth d w 0
