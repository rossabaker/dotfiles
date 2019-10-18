import XMonad

import System.Taffybar.Support.PagerHints (pagerHints)
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.ManageDocks
import XMonad.Util.EZConfig

myKeys = [ ("<XF86AudioMute>"       , spawn "amixer -c 0 set Master toggle")
         , ("<XF86AudioLowerVolume>", spawn "amixer -c 0 set Master 5%-")
         , ("<XF86AudioRaiseVolume>", spawn "amixer -c 0 set Master 5%+")
         , ("<XF86AudioMicMute>"    , spawn "amixer -c 0 set Capture toggle")
         ]

main = xmonad $
       docks $
       ewmh $
       pagerHints $
       def
  { borderWidth = 4
  , modMask = mod4Mask
  , terminal = "termite"
  , layoutHook = avoidStruts $ layoutHook defaultConfig
  , manageHook = manageHook defaultConfig <+> manageDocks
  }
  `additionalKeysP` myKeys
