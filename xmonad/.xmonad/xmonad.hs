import XMonad
import XMonad.Actions.WindowGo
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
                { ppCurrent = xmobarColor "white" "#00BCD4"
                , ppVisible = xmobarColor "#81C4BF" ""
                , ppHidden = xmobarColor "white" ""
                , ppHiddenNoWindows = xmobarColor "gray50" ""
                , ppOutput = hPutStrLn xmproc
                , ppTitle = xmobarColor "white" "" . shorten 40
                } >>= dynamicLogWithPP
    , modMask = mod4Mask     -- Rebind Mod to the Windows key
    } `additionalKeys`
    [ ((mod4Mask, xK_b), sendMessage ToggleStruts)
    -- google-chrome for personal, google-chrome-beta for work
    , ((mod4Mask, xK_f), runOrRaiseNext "google-chrome" (className =? "Google-chrome"))
    , ((mod4Mask, xK_g), runOrRaiseNext "google-chrome-beta" (className =? "Google-chrome-beta"))
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
      { inactiveBorderColor   = "#90A4AE"
      , inactiveColor         = "#90A4AE"
      , inactiveTextColor     = "#90A4AE"
      , activeBorderColor     = "#1DE9B6"
      , activeColor           = "#1DE9B6"
      , activeTextColor       = "#1DE9B6"
      , urgentBorderColor     = "#FF5252"
      , urgentTextColor       = "#FF5252"
      , decoHeight            = 10
      }

rofi cmd = safeSpawn "rofi" [ "-show", cmd ]

prompt :: XPConfig
prompt = def
  { font = "xft:DejaVu Sans Mono:size=16"
  , height = 28
  , position = Top
  }
