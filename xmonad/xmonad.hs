import System.Taffybar.Hooks.PagerHints (pagerHints)
import XMonad
import XMonad.Actions.WindowGo
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.ManageDocks
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.Spacing
import XMonad.Util.EZConfig

main = xmonad $
  ewmh $
  pagerHints $
  defaultConfig
  { borderWidth = 0                -- handled in myLayout with addTopBar
  , layoutHook  = myLayout
  , modMask     = mod4Mask
  , terminal    = "termite"
  }
  `additionalKeysP`
  [ ("M-p", spawn "rofi -show run")
  , ("M-x e", raiseMaybe (spawn "emacsclient -c") (className =? "Emacs"))
  , ("M-x s", runOrRaiseNext "slack" (className =? "Slack"))
  ]

myBorderWidth   = 6
myActiveColor   = "#cfb53b"
myInactiveColor = "#808080"

myLayout = addTopBar $ smartSpacingWithEdge (fromIntegral myBorderWidth) $ avoidStruts $ layoutHook defaultConfig
  where
    addTopBar = noFrillsDeco shrinkText topBarTheme
    topBarTheme = def
      { decoHeight          = myBorderWidth
      , inactiveColor       = myInactiveColor
      , inactiveBorderColor = myInactiveColor
      , inactiveTextColor   = myInactiveColor
      , activeColor         = myActiveColor
      , activeTextColor     = myActiveColor
      , activeBorderColor   = myActiveColor
      }
