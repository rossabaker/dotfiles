import qualified Codec.Binary.UTF8.String as UTF8
import qualified DBus as D
import qualified DBus.Client as D
import Graphics.X11.ExtraTypes.XF86
import XMonad
import XMonad.Actions.WindowGo
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers (doCenterFloat, isDialog, transience')
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.ToggleLayouts
import XMonad.Util.EZConfig (additionalKeys)
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

myKeys modKey =
  [ ((0, xF86XK_AudioMute), spawn "pactl set-sink-mute 0 toggle"),
    ((0, xF86XK_AudioLowerVolume), spawn "pactl set-sink-volume 0 -5%"),
    ((0, xF86XK_AudioRaiseVolume), spawn "pactl set-sink-volume 0 +5%"),
    ((0, xF86XK_AudioMicMute), spawn "pactl set-source-mute 1 toggle"),
    ((0, xF86XK_MonBrightnessDown), spawn "xbacklight -dec 5"),
    ((0, xF86XK_MonBrightnessUp), spawn "xbacklight -inc 5"),
    ((modKey, xK_semicolon), raiseNextMaybe (safeSpawn "emacsclient" ["-n", "-c"]) (className =? "Emacs")),
    ((modKey, xK_apostrophe), runOrRaiseNext "google-chrome-stable" (className =? "Google-chrome")),
    ((modKey, xK_f), sendMessage ToggleLayout),
    ((modKey, xK_p), spawn "rofi -show run")
  ]

myLayout =
  smartBorders
    $ toggleLayouts Full
    $ avoidStruts
    $ tiled ||| Mirror tiled
  where
    tiled = Tall 1 (3 / 100) (1 / 2)

myManageHook =
  composeAll
    [ manageHook defaultConfig,
      manageDocks,
      isDialog --> doCenterFloat,
      transience'
    ]

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
        modMask = modKey,
        terminal = "termite",
        layoutHook = myLayout,
        logHook = dynamicLogWithPP (myLogHook dbus),
        manageHook = myManageHook
      }
      `additionalKeys` (myKeys modKey)
  where
    modKey = mod4Mask
