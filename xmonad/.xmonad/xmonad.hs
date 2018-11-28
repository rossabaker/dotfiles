import XMonad
import XMonad.Actions.CopyWindow
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

import qualified XMonad.StackSet as W

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
    , modMask = myModMask
    , workspaces = myWorkspaces
    } `additionalKeys` myKeys

myModMask = mod4Mask -- Rebind Mod to the Windows key

myWorkspaces = ["1","2","3","4","5","6","7","8","9"]

myKeys =
  [ ((myModMask, xK_b), sendMessage ToggleStruts)
  , ((myModMask, xK_c), kill1)
  , ((myModMask, xK_d), (raiseMaybe . spawn) "emacsclient -c -n" (className =? "Emacs"))
  -- google-chrome for personal, google-chrome-beta for work
  , ((myModMask, xK_f), runOrRaiseNext "google-chrome" (className =? "Google-chrome"))
  , ((myModMask, xK_g), runOrRaiseNext "google-chrome-beta" (className =? "Google-chrome-beta"))
  , ((myModMask, xK_o), rofi "window")
  , ((myModMask, xK_p), rofi "run")
  , ((myModMask, xK_r), renameWorkspace prompt)
  ] ++
  [((m .|. myModMask, k), windows $ f i)
  | (i, k) <- zip myWorkspaces [xK_1 ..]
  , (f, m) <- [(W.view, 0), (W.shift, shiftMask), (copy, shiftMask .|. controlMask)]]  

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

copyMaybe :: X () -> Query Bool -> X ()
copyMaybe f qry = ifWindow qry copyWin f
    where copyWin = ask >>= \w -> doF (\ws -> copyWindow w (W.currentTag ws) ws)
