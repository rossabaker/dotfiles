import XMonad
import XMonad.Actions.WorkspaceNames
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Decoration
import XMonad.Layout.NoBorders
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.Renamed
import XMonad.Layout.Spacing
import XMonad.Prompt
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.Run

main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $ ewmh def
    { manageHook = manageDocks <+> manageHook def
    , layoutHook = myLayout
    , logHook = workspaceNamesPP xmobarPP
                { ppCurrent = xmobarColor "white" "#4285F4"
                , ppVisible = xmobarColor "white" "#4285F4"
                , ppHidden = xmobarColor "white" "#303030"
                , ppHiddenNoWindows = xmobarColor "gray50" "#282828"
                , ppOutput = hPutStrLn xmproc
                , ppTitle = xmobarColor "white" "" . shorten 40
                } >>= dynamicLogWithPP
    , modMask = mod4Mask     -- Rebind Mod to the Windows key
    } `additionalKeys`
    [ ((mod4Mask, xK_b), sendMessage ToggleStruts)
    , ((mod4Mask, xK_o), rofi "window")
    , ((mod4Mask, xK_p), rofi "run")
    , ((mod4Mask, xK_r), renameWorkspace prompt)
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

rofi cmd = safeSpawn "rofi" [ "-show", cmd ]

prompt :: XPConfig
prompt = def
  { font = "xft:DejaVu Sans Mono:size=16"
  , height = 28
  , position = Top
  }
