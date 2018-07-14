import XMonad
import XMonad.Config.Mate
import XMonad.Hooks.ManageDocks

main = xmonad mateConfig
  { layoutHook = myLayout
  }

myLayout = avoidStruts $ layoutHook mateConfig