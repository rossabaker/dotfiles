import XMonad

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.ManageDocks
import XMonad.Util.EZConfig

import qualified DBus as D
import qualified DBus.Client as D
import qualified Codec.Binary.UTF8.String as UTF8

myLogHook :: D.Client -> PP
myLogHook dbus = def { ppOutput = dbusOutput dbus }

-- Emit a DBus signal on log updates
dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
    let signal = (D.signal objectPath interfaceName memberName) {
            D.signalBody = [D.toVariant $ UTF8.decodeString str]
        }
    D.emit dbus signal
  where
    objectPath = D.objectPath_ "/org/xmonad/Log"
    interfaceName = D.interfaceName_ "org.xmonad.Log"
    memberName = D.memberName_ "Update"

myKeys = [ ("<XF86AudioMute>"       , spawn "amixer -c 0 set Master toggle")
         , ("<XF86AudioLowerVolume>", spawn "amixer -c 0 set Master 5%-")
         , ("<XF86AudioRaiseVolume>", spawn "amixer -c 0 set Master 5%+")
         , ("<XF86AudioMicMute>"    , spawn "amixer -c 0 set Capture toggle")
         ]

main = do
    dbus <- D.connectSession
    -- Request access to the DBus name
    D.requestName dbus (D.busName_ "org.xmonad.Log")
      [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]
    xmonad $
      docks $
      ewmh $
      def
      { borderWidth = 4
      , modMask = mod4Mask
      , terminal = "termite"
      , layoutHook = avoidStruts $ layoutHook defaultConfig
      , logHook = dynamicLogWithPP (myLogHook dbus)
      , manageHook = manageHook defaultConfig <+> manageDocks
      }
      `additionalKeysP` myKeys
