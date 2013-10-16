import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import System.IO
import XMonad.Util.Themes
import XMonad.Util.Run
import XMonad.Layout.Tabbed

mainModMask :: KeyMask
mainModMask = mod4Mask

main = do
    xmproc <- spawnPipe "/usr/bin/xmobar"
    xmonad $ defaultConfig
        { manageHook = myManageHook <+> manageHook defaultConfig
        , layoutHook = avoidStruts  $  layoutHook defaultConfig
        , logHook = dynamicLogWithPP xmobarPP
                         { ppOutput = hPutStrLn xmproc
                         , ppTitle = xmobarColor "green" "" . shorten 50
                         }
        , focusFollowsMouse = False
        , modMask = mainModMask
        , terminal =
            "/usr/bin/xterm -e zsh -c \"exec tmux new-session\""
        }

myManageHook = composeAll
    [ className =? "Mathematica" --> doFloat
    , className =? "pwsafe" --> doFloat
    , manageDocks
    ]

