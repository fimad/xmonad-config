import XMonad
import XMonad.Hooks.DynamicLog	
import System.Process
import XMonad.Util.Run
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Hooks.ManageDocks
import XMonad.Actions.DynamicWorkspaces
import XMonad.Layout.PerWorkspace
import XMonad.Layout
import XMonad.Layout.IM
import XMonad.Layout.Grid
import XMonad.Layout.Reflect 
import Data.Ratio ((%))
import XMonad.Util.WorkspaceCompare
import XMonad.Actions.CycleWS
import XMonad.Layout.LayoutHints	
import qualified XMonad.StackSet as W
import XMonad.Actions.PhysicalScreens	as PS

--myLayout = withIM (1%7) (ClassName "Pidgin") Grid ||| ResizableTall 1 (3/100) (1/2) [] ||| Full
--myImLayout = withIM (1%7) (ClassName "Pidgin") Grid ||| Full

myImLayout = withIM (1%7) (Role "buddy_list") skypeLayout
	where
		--skypeLayout = Grid
		skypeLayout = reflectHoriz $ withIM (1%7) skype Grid
		skype = Title "fsmismynantidrug - Skype™ (Beta)" `Or` Title "Skype™ 2.2 (Beta) for Linux"
		

myGimpLayout = withIM (1%7) (Role "gimp-toolbox") $ reflectHoriz $ withIM (1%7) (Role "gimp-dock") Full

myDefaultLayout = layoutHints $ layoutHook defaultConfig

myManager = composeAll [
	title =? "xfce4-notifyd" --> doIgnore,
	className =? "stalonetray" --> doIgnore,
	className =? "mb_warband.exe" --> doIgnore,
	className =? "Pidgin" --> doShift "IM",
	className =? "Instantbird" --> doShift "IM",
	className =? "Skype" --> doShift "IM",
	title =? "VLC (XVideo output)" --> doIgnore,
	title =? "exe" --> doIgnore,
	className =? "Gimp" --> doShift "Gimp",
	className =? "net-minecraft-LauncherFrame" --> doFloat,
	stringProperty "WM_WINDOW_ROLE"  =? "gimp-scale-tool" --> doFloat,
	stringProperty "WM_WINDOW_ROLE"  =? "gimp-shear-tool" --> doFloat,
	stringProperty "WM_WINDOW_ROLE"  =? "gimp-rotate-tool" --> doFloat,
	stringProperty "WM_WINDOW_ROLE"  =? "gimp-perspective-tool" --> doFloat,
	stringProperty "WM_WINDOW_ROLE"  =? "gimp-flip-tool" --> doFloat,
	stringProperty "WM_WINDOW_ROLE"  =? "gimp-layer-new" --> doFloat
	]

myWorkspaces :: [String]
myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "IM", "Gimp"]
myWorkspaceKeys :: [KeySym]
myWorkspaceKeys = [xK_1, xK_2, xK_3, xK_4, xK_5, xK_6, xK_7, xK_8, xK_9, xK_0, xK_minus]

switchSecondScreen:: String -> X ()
switchSecondScreen v = do
  PS.viewScreen 1
  windows $ W.greedyView v
  PS.viewScreen 0

myAdditionalKeys = [
  ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s"),
  ((0, xK_Print), spawn "scrot"),
  ((controlMask .|. mod1Mask, xK_bracketright), spawn "nautilus --no-desktop"),
--  ((controlMask .|. mod1Mask, xK_bracketleft), spawn "swiftfox -P Vimperator"),
  ((controlMask .|. mod1Mask .|. shiftMask, xK_bracketleft), spawn "swiftfox -P Vimperator"),
  ((controlMask .|. mod1Mask, xK_bracketleft), spawn "google-chrome"),
  ((controlMask .|. mod1Mask, xK_apostrophe), spawn "wicd-client -n"),
  ((controlMask .|. mod1Mask, xK_semicolon), spawn "thunderbird"),
  ((controlMask .|. mod1Mask, xK_Return), spawn "terminator"),
  ((controlMask .|. mod1Mask, xK_backslash), spawn "gvim"),
  ((controlMask .|. mod1Mask, xK_Page_Up), spawn "amixer -c 0 set Master 3dB+"),
  ((controlMask .|. mod1Mask, xK_Page_Down), spawn "amixer -c 0 set Master 3dB-"),
  --((controlMask .|. mod1Mask, xK_End), spawn "amixer -c 0 set Master toggle"),
  ((controlMask .|. mod1Mask, xK_End), spawn "pulse_toggle_mute.pl"),
  ((mod4Mask, xK_0), windows $ W.greedyView "IM" ) ,
  ((mod4Mask, xK_minus), windows $ W.greedyView "Gimp" )] ++ (map (\(key,view) -> ((controlMask .|. mod4Mask, key), switchSecondScreen view)) $ zip myWorkspaceKeys myWorkspaces)


main = do
	runCommand "/home/will/.xmonad/init.sh"
	h <- spawnPipe "xmobar ~/.xmobarrc"
	xmonad $ defaultConfig {
		manageHook = manageDocks <+> myManager,
		layoutHook = avoidStruts $ onWorkspace "IM" myImLayout $ onWorkspace "Gimp" myGimpLayout $ myDefaultLayout,
		workspaces = myWorkspaces,
		modMask = mod4Mask,
		logHook = dynamicLogWithPP $ xmobarPP { ppOutput = hPutStrLn h }
	} `additionalKeys` myAdditionalKeys



