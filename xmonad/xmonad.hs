import XMonad

main = xmonad defaultConfig
  { borderWidth = 4
  , modMask = mod4Mask
  , terminal = "termite"
  }
