import Control.Monad
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import System.IO
import XMonad.Util.Themes
import XMonad.Util.Run
import XMonad.Layout.Tabbed
import XMonad.Layout.NoBorders(noBorders)
import XMonad.Layout.PerWorkspace(onWorkspace)
import qualified XMonad.StackSet as W
import XMonad.Actions.CycleWS
import qualified Data.Map as M
import XMonad.Prompt
import XMonad.Prompt.Workspace

-- Config partially taken from
-- http://www.nepherte.be/step-by-step-configuration-of-xmonad/

mainModMask :: KeyMask
mainModMask = mod4Mask

myWorkspaces :: [String]
myWorkspaces = ["Tor", "Chromium", "Terminal", "4", "5", "6", "7", "Chat",
                "Music"]

main = do
    xmproc <- spawnPipe "/usr/bin/xmobar"
    xmonad $ defaultConfig
        { manageHook = myManageHook
        , layoutHook = avoidStruts myLayouts
        , logHook = dynamicLogWithPP xmobarPP
                         { ppOutput = hPutStrLn xmproc
                         , ppTitle = xmobarColor "green" "" . shorten 40
                         }
        , focusFollowsMouse = False
        , modMask = mainModMask
        , workspaces = myWorkspaces
        , keys = myKeys
        , startupHook = myStartupHook
        }

myManageHook = composeAll $ concat $
    [
        [className =? name --> doFloat | name <- floatedClassNames]
      , [className =? name --> doF (W.shift "Tor") | name <- torClassNames]
      , [className =? name --> doF (W.shift "Chromium") |
         name <- chromiumClassNames]
      , [className =? name --> doF (W.shift "Chat") | name <- chatClassNames]
      , [className =? name --> viewShift "Music" | name <- musicClassNames]
      , [className =? name --> viewShift "Terminal" |
         name <- terminalClassNames]
    ]
    where
        viewShift = doF . liftM2 (.) W.greedyView W.shift
        floatedClassNames = ["Mathematica", "pwsafe"]
        -- It seems the difference between classes and resources is a little
        -- dicey
        torClassNames = ["Firefox", "Vidalia"]
        chromiumClassNames = ["Chromium"]
        musicClassNames = ["ncmpcpp"]
        chatClassNames = ["Pidgin"]
        terminalClassNames = ["terminal"]

defaultLayouts = noBorders Full ||| tiled ||| Mirror tiled
    where
        tiled = Tall nmaster delta ratio
        nmaster = 1
        delta = 3/100
        ratio = 1/2

chatLayout = Tall nmaster delta ratio
    where
        nmaster = 1
        delta = 3/100
        ratio = 33/100

myLayouts = onWorkspace "Chat" chatLayout defaultLayouts

keysToAdd x =
    [
          ((modMask x, xK_c), kill)
        , ((modMask x, xK_Left), prevWS)
        , ((modMask x, xK_Right), nextWS)
        , (((modMask x .|. shiftMask), xK_Left), shiftToPrev)
        , (((modMask x .|. shiftMask), xK_Right), shiftToNext)
        , ((modMask x, xK_p), spawn "mpc toggle")
        , ((modMask x, xK_Up), spawn "mpc prev")
        , ((modMask x, xK_Down), spawn "mpc next")
        , ((modMask x, xK_p), spawn "mpc toggle")
        , ((modMask x, xK_Up), spawn "mpc prev")
        , ((modMask x, xK_Down), spawn "mpc next")
        , ((modMask x, xK_bracketleft), sendMessage Shrink)
        , ((modMask x, xK_bracketright), sendMessage Expand)
        , ((modMask x .|. shiftMask, xK_bracketleft),
           sendMessage (IncMasterN 1))
        , ((modMask x .|. shiftMask, xK_bracketright),
           sendMessage (IncMasterN (-1)))
        , ((modMask x, xK_equal), refresh)
        , ((modMask x, xK_b), sendMessage ToggleStruts)
        , ((modMask x, xK_t),
           workspacePrompt defaultXPConfig (windows . W.shift))
    ]
 
keysToRemove x =
    [
          (modMask x .|. shiftMask, xK_p)
        , (modMask x .|. shiftMask, xK_c)
        , (modMask x, xK_n)
        , (modMask x, xK_comma)
        , (modMask x, xK_period)
        , (modMask x, xK_h)
        , (modMask x, xK_l)
    ]
 
-- Delete the keys combinations we want to remove.
strippedKeys x = foldr M.delete (keys defaultConfig x) (keysToRemove x)
 
-- Compose all my new key combinations.
myKeys x = M.union (strippedKeys x) (M.fromList (keysToAdd x))

myStartupHook =
    spawn "tor-browser-en" >>
    spawn "chromium" >>
    spawn "pidgin"
