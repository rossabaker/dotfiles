import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.Run(spawnPipe)
import System.IO

main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $ def
    { manageHook = manageDocks <+> manageHook def
    , layoutHook = avoidStruts  $  layoutHook def
    , logHook = dynamicLogWithPP xmobarPP
                { ppCurrent = xmobarColor "white" "#4285F4"
                , ppVisible = xmobarColor "white" "#4285F4"
                , ppHidden = xmobarColor "white" "#303030"
                , ppHiddenNoWindows = xmobarColor "gray50" "#282828"
                , ppOutput = hPutStrLn xmproc
                , ppTitle = xmobarColor "white" "" . shorten 40
                }
    , modMask = mod4Mask     -- Rebind Mod to the Windows key
    } `additionalKeys`
        [ ((mod4Mask, xK_b), sendMessage ToggleStruts)
        ]
