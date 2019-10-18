import XMonad

import XMonad.Hooks.ManageDocks
import XMonad.Util.EZConfig

myKeys = [ ("<XF86AudioMute>"       , spawn "amixer -c 0 set Master toggle")
         , ("<XF86AudioLowerVolume>", spawn "amixer -c 0 set Master 5%-")
         , ("<XF86AudioRaiseVolume>", spawn "amixer -c 0 set Master 5%+")
         , ("<XF86AudioMicMute>"    , spawn "amixer -c 0 set Capture toggle")
         ]

main = xmonad $ docks defaultConfig
  { borderWidth = 4
  , modMask = mod4Mask
  , terminal = "termite"
  , layoutHook = avoidStruts $ layoutHook defaultConfig
  , manageHook = manageHook defaultConfig <+> manageDocks
  }
  `additionalKeysP` myKeys
