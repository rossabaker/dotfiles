import XMonad
import XMonad.Config.Mate
import XMonad.Hooks.ManageDocks
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.Spacing

main = xmonad mateConfig
  { borderWidth = 0                -- handled in myLayout with addTopBar
  , layoutHook  = myLayout
  , modMask     = mod4Mask
  }

myBorderWidth   = 6
myActiveColor   = "#cfb53b"
myInactiveColor = "#808080"

myLayout = addTopBar $ smartSpacingWithEdge (fromIntegral myBorderWidth) $ avoidStruts $ layoutHook mateConfig
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
