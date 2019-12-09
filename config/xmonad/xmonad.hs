import qualified Codec.Binary.UTF8.String as UTF8
import qualified DBus as D
import qualified DBus.Client as D
import XMonad
import XMonad.Actions.WindowGo
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.ToggleLayouts
import XMonad.Util.EZConfig
import XMonad.Util.Run

myLogHook :: D.Client -> PP
myLogHook dbus = def {ppOutput = dbusOutput dbus}

-- Emit a DBus signal on log updates
dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
  let signal =
        (D.signal objectPath interfaceName memberName)
          { D.signalBody = [D.toVariant $ UTF8.decodeString str]
          }
  D.emit dbus signal
  where
    objectPath = D.objectPath_ "/org/xmonad/Log"
    interfaceName = D.interfaceName_ "org.xmonad.Log"
    memberName = D.memberName_ "Update"

myKeys =
  [ ("<XF86AudioMute>", spawn "pactl set-sink-mute 0 toggle"),
    ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume 0 -5%"),
    ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume 0 +5%"),
    ("<XF86AudioMicMute>", spawn "pactl set-source-mute 1 toggle"),
    ("<XF86MonBrightnessDown>", spawn "xbacklight -dec 5"),
    ("<XF86MonBrightnessUp>", spawn "xbacklight -inc 5"),
    ("M-;", raiseNextMaybe (safeSpawn "emacsclient" ["-n", "-c"]) (className =? "Emacs")),
    ("M-'", runOrRaiseNext "google-chrome-stable" (className =? "Google-chrome")),
    ("M-f", sendMessage ToggleLayout),
    ("M-p", spawn "rofi -show run")
  ]

myLayout =
  smartBorders
    $ toggleLayouts Full
    $ avoidStruts
    $ tiled ||| Mirror tiled
  where
    tiled = Tall 1 (3 / 100) (1 / 2)

main = do
  dbus <- D.connectSession
  -- Request access to the DBus name
  D.requestName
    dbus
    (D.busName_ "org.xmonad.Log")
    [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]
  xmonad
    $ docks
    $ fullscreenSupport
    $ ewmh
    $ def
      { borderWidth = 4,
        focusedBorderColor = "#81a2be",
        normalBorderColor = "#373b41",
        modMask = mod4Mask,
        terminal = "termite",
        layoutHook = myLayout,
        logHook = dynamicLogWithPP (myLogHook dbus),
        manageHook = manageHook defaultConfig <+> manageDocks
      }
      `additionalKeysP` myKeys
