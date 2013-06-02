import XMonad
import qualified XMonad.StackSet as W
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Actions.GridSelect
import XMonad.Actions.CopyWindow
import System.IO

import XMonad.Prompt
import XMonad.Prompt.Shell

import XMonad.Layout
import XMonad.Layout.NoBorders
import XMonad.Layout.Accordion
import XMonad.Layout.Circle
import XMonad.Layout.StackTile
import XMonad.Layout.WorkspaceDir

main = do
    xmproc <- spawnPipe "~/bin/xmobar"
    xmonad $ myConfig xmproc

modm = mod1Mask

myLayout =  setOptions $ tiled ||| Mirror Accordion ||| Circle ||| StackTile 1 (3/100) (1/2)
    where setOptions = workspaceDir "~" . smartBorders . avoidStruts
          tiled = Tall nmaster delta ratio
          nmaster = 1
          ratio = 1/2
          delta = 3/100

myWorkspaces :: [WorkspaceId]
myWorkspaces = zipWith joinStr numbers names
    where names = ["general", "web", "programming", "music", "other"]
          numbers = (map show [1..])
          joinStr = (\num name -> num ++ ":" ++ name)

myConfig pipeproc = additionalKeys myConfig' myKeys
    where myConfig' = defaultConfig
                        { terminal = "urxvt"
                        , workspaces = myWorkspaces
                        , manageHook = manageDocks <+> manageHook defaultConfig
                        , layoutHook = myLayout
                        , modMask = modm
                        , logHook = dynamicLogWithPP xmobarPP
                            { ppOutput = hPutStrLn pipeproc
                            , ppTitle = xmobarColor "green" "" . shorten 50
                            }
                        }

myKeys = [ ((modm .|. shiftMask, xK_l), spawn "~/bin/lock")
         , ((modm, xK_a), goToSelected defaultGSConfig)
         , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
         , ((0, xK_Print), spawn "scrot")
         , ((modm .|. controlMask, xK_x), shellPrompt myXPConfig)
         , ((modm .|. shiftMask, xK_a), changeDir myXPConfig)
         ]
         ++
         [((m .|. modm, k), windows $ f i)
         | (i,k) <- zip (myWorkspaces) [xK_1..]
         , (f,m) <- [(W.view, 0), (W.shift, shiftMask), (copy, shiftMask .|. controlMask)]
         ]

myXPConfig = defaultXPConfig
                { font = "-*-fixed-*-*-*-*-12-*-*-*-*-*-*-*"
                , bgColor = "black"
                , fgColor = "white"
                , bgHLight = "white"
                , fgHLight = "black"
                , borderColor = "white"
                , promptBorderWidth = 0
                , position = Top
                , height = 14
                }
