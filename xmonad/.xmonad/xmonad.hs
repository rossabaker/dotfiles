import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Decoration
import XMonad.Layout.NoBorders
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.Renamed
import XMonad.Layout.Spacing
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.Run(spawnPipe)
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
      , activeBorderColor     = "#4285F4"
      , activeColor           = "#4285F4"
      , activeTextColor       = "#4285F4"
      , urgentBorderColor     = "#EA4335"
      , urgentTextColor       = "#EA4335"
      , decoHeight            = 10
      }
