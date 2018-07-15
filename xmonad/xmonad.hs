import XMonad
import XMonad.Config.Mate
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Spacing

main = xmonad mateConfig
  { layoutHook = myLayout
  }

myLayout = spacingWithEdge 6 $ avoidStruts $ layoutHook mateConfig
