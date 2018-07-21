{-# LANGUAGE OverloadedStrings #-}

import DBus
import DBus.Client
import System.Taffybar.Hooks.PagerHints (pagerHints)
import XMonad
import XMonad.Actions.WindowGo
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.ManageDocks
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.Renamed
import XMonad.Layout.Spacing
import XMonad.StackSet
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad

import qualified XMonad.StackSet as W

main =
  do
    dbusClient <- connectSession
    xmonad $
      ewmh $
      pagerHints $
      docks def
      { borderWidth = 0                -- handled in myLayout with addTopBar
      , layoutHook  = myLayout
      , manageHook  = myManageHook
      , modMask     = mod4Mask
      , terminal    = "termite"
      }
      `additionalKeysP`
      [ ("M-b", sendMessage ToggleStruts)
      , ("M-p", spawn "rofi -show run")
      , ("M-m <Space>", spotifyCtrl dbusClient "PlayPause")
      , ("M-m n", spotifyCtrl dbusClient "Next")
      , ("M-m p", spotifyCtrl dbusClient "Previous")
      , ("M-x e", raiseMaybe (spawn "emacsclient -c") (className =? "Emacs"))
      , ("M-x m", namedScratchpadAction myScratchpads "spotify")
      , ("M-x v", namedScratchpadAction myScratchpads "mixer")
      , ("M-x s", runOrRaiseNext "slack" (className =? "Slack"))
      , ("<XF86AudioMute>", spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
      , ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ +5%")
      , ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%")
      , ("<XF86AudioMicMute>", spawn "pactl set-source-mute @DEFAULT_SOURCE@ toggle")
      , ("<XF86MonBrightnessDown>", spawn "xbacklight -dec 10%")
      , ("<XF86MonBrightnessUp>"  , spawn "xbacklight -inc 10%")
      ]

myBorderWidth   = 6
myActiveColor   = "#cfb53b"
myInactiveColor = "#808080"
myLayout = avoidStruts $ tall ||| full
  where
    tall = renamed [Replace "tall"] $
      addTopBar $
      smartSpacingWithEdge (fromIntegral myBorderWidth) $
      Tall 1 0.025 0.5
    full = renamed [Replace "full"] $ Full
    addTopBar = noFrillsDeco shrinkText topBarTheme
    topBarTheme = def
      { decoHeight          = myBorderWidth
      , inactiveColor       = myInactiveColor
      , inactiveBorderColor = myInactiveColor
      , inactiveTextColor   = myInactiveColor
      , activeColor         = myActiveColor
      , activeTextColor     = myActiveColor
      , activeBorderColor   = myActiveColor
      }

myManageHook = manageHook def <+>
               namedScratchpadManageHook myScratchpads

myScratchpads =
  [ NS "mixer" "pavucontrol" (className =? "Pavucontrol") (customFloating $ W.RationalRect 0.2 0.2 0.6 0.6)
    -- May want to set app.window.position.saved=false in ~/.config/spotify/prefs
  , NS "spotify" "spotify --force-device-scale-factor=1.75" (className =? "Spotify") (customFloating $ W.RationalRect 0.1 0.1 0.8 0.8)
  ]

-- https://gist.github.com/htr/6267335
spotifyCtrl :: Client -> MemberName -> X ()
spotifyCtrl client command = liftIO $ do
  call_ client
    (methodCall "/org/mpris/MediaPlayer2" "org.mpris.MediaPlayer2.Player" command) {
      methodCallDestination = Just "org.mpris.MediaPlayer2.spotify" }
  return ( )
