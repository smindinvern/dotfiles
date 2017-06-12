import XMonad
import XMonad.Util.EZConfig
import qualified XMonad.StackSet as W
import qualified XMonad.Layout.SimpleDecoration as SD
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed
import XMonad.Util.Themes
import XMonad.Hooks.ManageDocks
import XMonad.Config.Desktop
import XMonad.Hooks.EwmhDesktops

myLayout = SD.simpleDeco SD.shrinkText (theme xmonadTheme) (spacing 15 (layoutHook desktopConfig))

main :: IO ()
main = do
  xmonad (addKeys . remKeys $ desktopConfig) { modMask = mod4Mask -- use super instad of alt
                            , terminal = "urxvt"
			    , focusFollowsMouse = False
			    , layoutHook = desktopLayoutModifiers myLayout
			    , focusedBorderColor = "#0000ff"
			    , borderWidth = 2
			    , clickJustFocuses = False
			    , handleEventHook = fullscreenEventHook
			    }
  where remKeys = (flip removeKeysP) [ "S-M4-" ++ [n] | n <- ['1'..'9'] ]
  	addKeys conf = additionalKeysP conf ([ ("C-M4-l", spawn "xscreensaver-command -lock")
		     		             ] ++ [ ("S-M4-" ++ n, windows $ (W.view n) . (W.shift n))
					          | n <- workspaces conf ])
