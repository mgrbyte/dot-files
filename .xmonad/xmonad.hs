import System.IO
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.EZConfig
import XMonad.Util.Run(spawnPipe)

myManageHook = composeAll
    [ className =? "Gimp"      --> doFloat
    , className =? "Vncviewer" --> doFloat
    ]

main = do
    xmproc <- spawnPipe "xmobar ~/.xmobarrc"
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
	       ((0, 0x1008FF11), spawn "amixer set Master 2-"),
	       ((0, 0x1008FF13), spawn "amixer set Master 2+"),
	       ((0, 0x1008FF12), spawn "amixer set Master toggle")
             ]


