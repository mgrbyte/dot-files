import System.IO
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.EZConfig
import XMonad.Util.Run (spawnPipe)

workspaces :: [WorkspaceId]
workspaces = ["1:dev", "2:mail", "3:web", "4:comm", "5:ham", "6:tmp" :: String]

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myModMask :: KeyMask
myModMask = mod4Mask

myManageHook :: ManageHook
myManageHook = composeAll
    [
      className =? "Gimp"      --> doFloat,
      className =? "Vncviewer" --> doFloat
    ]

main :: IO ()
main = do
    xmonad $ defaultConfig
             { manageHook = manageDocks <+> myManageHook -- make sure to include myManageHook definition from above
                            <+> manageHook defaultConfig,
               layoutHook = avoidStruts  $  layoutHook defaultConfig,
               logHook = dynamicLogWithPP xmobarPP
                         { ppOutput = hPutStrLn xmproc,
                           ppTitle = xmobarColor "aqua" "" . shorten 50
                          },
               modMask = mod4Mask,
               terminal = "uxterm"
	     } `additionalKeysP`
             [ ("M-e", spawn "emacsclient -c"),
               ("M-f", spawn "firefox"),
	       ("M-m", spawn "thunderbird"),
	       ("<XF86AudioLowerVolume>", spawn "amixer set Master 2-"),
	       ("<XF86AudioRaiseVolume>", spawn "amixer set Master 2+"),
	       ("<XF86AudioMute>", spawn "amixer set Master toggle")

             ]
