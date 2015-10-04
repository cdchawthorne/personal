import Control.Monad
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
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

-- myWorkspaces :: [String]
-- myWorkspaces = ["Tor", "Chromium", "Terminal", "4", "5", "6", "News", "Chat",
--                 "Music"]

main = do
    xmproc <- spawnPipe "/usr/bin/xmobar"
    {- xmonad $ withUrgencyHook (CommandUrgencyHook "notify-send Urgency: '" "'") -}
    xmonad $ withUrgencyHook dzenUrgencyHook
        $ ewmh defaultConfig
        { manageHook = myManageHook
        , layoutHook = avoidStruts myLayouts
        , logHook = dynamicLogWithPP xmobarPP
                         { ppOutput = hPutStrLn xmproc
                         , ppTitle = xmobarColor "green" "" . shorten 40
                         }
        , focusFollowsMouse = False
        , modMask = mainModMask
--      , workspaces = myWorkspaces
        , keys = myKeys
        , startupHook = myStartupHook
        }

myManageHook = composeAll $ concat $
    [
        [className =? name --> doFloat | name <- floatedClassNames]
      , [className =? name --> doF (W.shift "1") | name <- torClassNames]
      , [className =? name --> doF (W.shift "2") | name <- chromiumClassNames]
      , [className =? name --> viewShift "7" | name <- calendarClassNames]
      , [className =? name --> viewShift "8" | name <- chatClassNames]
      , [className =? name --> viewShift "9" | name <- musicClassNames]
      , [className =? name --> viewShift "3" | name <- terminalClassNames]
    ]
    where
        viewShift = doF . liftM2 (.) W.greedyView W.shift
        floatedClassNames = ["Mathematica", "pwsafe"]
        -- It seems the difference between classes and resources is a little
        -- dicey
        torClassNames = ["Tor Browser"]
        chromiumClassNames = ["Chromium"]
        musicClassNames = ["music"]
        chatClassNames = ["irc"]
        terminalClassNames = ["terminal"]
        calendarClassNames = ["calendar"]

myLayouts = noBorders Full ||| tiled ||| Mirror tiled
    where
        tiled = Tall nmaster delta ratio
        nmaster = 1
        delta = 3/100
        ratio = 1/2

keysToAdd x =
    [
          ((modMask x, xK_c), kill)
        , ((modMask x, xK_Left), prevWS)
        , ((modMask x, xK_Right), nextWS)
        , (((modMask x .|. shiftMask), xK_Left), shiftToPrev)
        , (((modMask x .|. shiftMask), xK_Right), shiftToNext)
        , ((modMask x, xK_braceleft), sendMessage Shrink)
        , ((modMask x, xK_braceright), sendMessage Expand)
        , ((modMask x .|. shiftMask, xK_braceleft),
           sendMessage (IncMasterN 1))
        , ((modMask x .|. shiftMask, xK_braceright),
           sendMessage (IncMasterN (-1)))
        , ((modMask x, xK_equal), refresh)
        , ((modMask x, xK_b), sendMessage ToggleStruts)
        , ((modMask x, xK_g),
           workspacePrompt defaultXPConfig (windows . W.greedyView))
        , ((modMask x .|. shiftMask, xK_g),
           workspacePrompt defaultXPConfig (windows . W.shift))
    ]
    ++
    [((m .|. modMask x, k), windows $ f i)
            | (i, k) <- zip (XMonad.workspaces x)
              [xK_exclam, xK_at, xK_numbersign, xK_dollar, xK_percent,
               xK_asciicircum, xK_ampersand, xK_asterisk, xK_parenleft,
               xK_parenright]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
 
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

{- myStartupHook = spawn "tor-browser-en" >> spawn "chromium" -}
myStartupHook = spawn "chromium"

data CommandUrgencyHook = CommandUrgencyHook String String deriving (Read, Show)
instance UrgencyHook CommandUrgencyHook where
    urgencyHook (CommandUrgencyHook prefix suffix) w = spawn $ prefix ++ show w ++ suffix
