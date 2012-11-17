import System.IO
import System.Process
import Data.Ratio ((%))

import XMonad
import qualified XMonad.StackSet as W

import XMonad.Util.Run
import XMonad.Util.WorkspaceCompare
import XMonad.Util.EZConfig(additionalKeys)

import XMonad.Hooks.DynamicLog	
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops

import XMonad.Actions.CycleWS
import XMonad.Actions.NoBorders
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.PhysicalScreens	as PS

import XMonad.Layout
import XMonad.Layout.IM
import XMonad.Layout.Grid
import XMonad.Layout.Reflect 
import XMonad.Layout.NoBorders
import XMonad.Layout.LayoutHints	
import XMonad.Layout.PerWorkspace


{-------------------------------------------------------------------------------
  - View Layouts
-------------------------------------------------------------------------------}
myImLayout = withIM (1%7) (Role "buddy_list") skypeLayout
	where
		skypeLayout = reflectHoriz
		            $ withIM (1%7) skype Grid
		skype = Title "fsmismynantidrug - Skype™ (Beta)" `Or` Title "Skype™ 2.2 (Beta) for Linux"
		
myGimpLayout = withIM (1%7) (Role "gimp-toolbox")
             $ reflectHoriz
             $ withIM (1%7) (Role "gimp-dock") Full

-- A generic layout for views that should just be full screen
myFSLayout = noBorders Full

-- Use the standard layout for each of the
myDefaultLayout = smartBorders $ layoutHints $ layoutHook defaultConfig


{-------------------------------------------------------------------------------
  - Workspaces
-------------------------------------------------------------------------------}
myWorkspaces :: [String]
myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "IM", "Gimp", "Wine"]
myWorkspaceKeys :: [KeySym]
myWorkspaceKeys = [xK_1, xK_2, xK_3, xK_4, xK_5, xK_6, xK_7, xK_8, xK_9, xK_0, xK_minus, xK_equal]


{-------------------------------------------------------------------------------
  - Handle all the applications that fuck up when they are run in a tiled window
  - manager 
-------------------------------------------------------------------------------}
myManager = composeAll [
    title =? "xfce4-notifyd" --> doIgnore
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
myAdditionalKeys = [
  -- Print screen
    ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
  , ((0, xK_Print), spawn "scrot")

  -- Commonly run programs
  , ((controlMask .|. mod1Mask, xK_bracketright), spawn "thunar")
  , ((controlMask .|. mod1Mask, xK_bracketleft), spawn "google-chrome")
  , ((controlMask .|. mod1Mask, xK_apostrophe), spawn "wicd-client -n")
  , ((controlMask .|. mod1Mask, xK_Return), spawn "terminator")
  , ((controlMask .|. mod1Mask, xK_backslash), spawn "gvim")

  -- Audio Key configuration
  , ((controlMask .|. mod1Mask, xK_Page_Up), spawn "~/.xmonad/bin/pulse_control.pl -inc")
  , ((controlMask .|. mod1Mask, xK_Page_Down), spawn "~/.xmonad/bin/pulse_control.pl -dec")
  , ((controlMask .|. mod1Mask, xK_End), spawn "~/.xmonad/bin/pulse_control.pl -toggle")

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
  runCommand "/home/will/.xmonad/init.sh"
  h <- spawnPipe "~/.xmonad/status_bar.pl"
  xmonad $ ewmh $ defaultConfig {
      focusedBorderColor = "#cb4b16"
    , normalBorderColor = "#002b36"
    , handleEventHook = fullscreenEventHook
    , manageHook = manageDocks <+> myManager
    , layoutHook = avoidStruts
                 $ onWorkspace "IM" myImLayout
                 $ onWorkspace "Gimp" myGimpLayout 
                 $ onWorkspace "Wine" myFSLayout 
                 $ myDefaultLayout
    , workspaces = myWorkspaces
    , modMask = mod4Mask -- Use the window key
    , logHook = dynamicLogWithPP $ printStatusBar h
  } `additionalKeys` myAdditionalKeys

printStatusBar :: Handle -> PP
printStatusBar h = defaultPP {
    ppCurrent = (\c -> concat ["<CURRENT>[",c,"]</CURRENT>"])
  , ppVisible = (\c -> concat ["<VISIBLE>(",c,")</VISIBLE>"])
  , ppTitle   = (\c -> dzenEscape $ concat ["<TITLE>",c,"</TITLE>"])
  , ppLayout  = (\c -> concat ["<LAYOUT>",c,"</LAYOUT>"])
  , ppSep = " "
  , ppOutput = hPutStrLn h
}


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
  withDisplay $ \d -> io $ do
        setWindowBorderWidth d w 0

