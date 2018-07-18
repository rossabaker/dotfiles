import System.Taffybar.Hooks.PagerHints (pagerHints)
import XMonad
import XMonad.Actions.WindowGo
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.ManageDocks
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.Renamed
import XMonad.Layout.Spacing
import XMonad.Util.EZConfig

main = xmonad $
  ewmh $
  pagerHints $
  docks def
  { borderWidth = 0                -- handled in myLayout with addTopBar
  , layoutHook  = myLayout
  , modMask     = mod4Mask
  , terminal    = "termite"
  }
  `additionalKeysP`
  [ ("M-b", sendMessage ToggleStruts)
  , ("M-p", spawn "rofi -show run")
  , ("M-x e", raiseMaybe (spawn "emacsclient -c") (className =? "Emacs"))
  , ("M-x s", runOrRaiseNext "slack" (className =? "Slack"))
  ]

myBorderWidth   = 6
myActiveColor   = "#cfb53b"
myInactiveColor = "#808080"

myLayout = avoidStruts $ tall ||| full
  where
    tall = renamed [Replace "tall"] $
      addTopBar $
      smartSpacingWithEdge (fromIntegral myBorderWidth) $
      Tall 1 0.025 0.5
    full = renamed [Replace "full"] $ Full
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
