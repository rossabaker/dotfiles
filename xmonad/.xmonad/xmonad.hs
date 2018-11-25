import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Decoration
import XMonad.Layout.NoBorders
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.Renamed
import XMonad.Layout.Spacing
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.Run
import System.IO

main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $ def
    { manageHook = manageDocks <+> manageHook def
    , layoutHook = myLayout
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
    , ((mod4Mask, xK_p), rofi)
    ]

myLayout = avoidStruts $ tall ||| wide ||| full
  where
    gap = (fromIntegral 6)
    tall = renamed [Replace "tall"] $
      addTopBar $
      noBorders $
      smartSpacingWithEdge gap $
      Tall 1 0.025 0.5
    wide = renamed [Replace "wide"] $
      addTopBar $
      noBorders $
      smartSpacingWithEdge gap $
      Mirror $
      Tall 1 0.025 0.5
    full = renamed [Replace "full"] $ noBorders $ Full
    addTopBar = noFrillsDeco shrinkText topBarTheme
    topBarTheme = def 
      { inactiveBorderColor   = "gray40"
      , inactiveColor         = "gray40"
      , inactiveTextColor     = "gray40"
      , activeBorderColor     = "#7BAAF7"
      , activeColor           = "#7BAAF7"
      , activeTextColor       = "#7BAAF7"
      , urgentBorderColor     = "#EA4335"
      , urgentTextColor       = "#EA4335"
      , decoHeight            = 10
      }

rofi = safeSpawn "rofi" [ "-show", "run" ]
