import XMonad
import qualified XMonad.StackSet as W
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys, additionalKeysP)
import XMonad.Actions.GridSelect
import XMonad.Actions.CopyWindow
import XMonad.Actions.WindowGo
import System.IO

import XMonad.Prompt
import XMonad.Prompt.Shell

import XMonad.Layout
import XMonad.Layout.NoBorders
import XMonad.Layout.WorkspaceDir

import JackStack

main = do
    xmproc <- spawnPipe "~/bin/xmobar"
    xmonad $ myConfig xmproc

modm = mod1Mask

myLayout =  setOptions $ tiled ||| JackStack (3/4)
    where setOptions = workspaceDir "~" . smartBorders . avoidStruts
          tiled = Tall nmaster delta ratio
          nmaster = 1
          ratio = 1/2
          delta = 3/100

myWorkspaces :: [WorkspaceId]
myWorkspaces = map show [1..9::Int]

myConfig pipeproc = addAllMyKeys myConfig'
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

addAllMyKeys config = config `additionalKeys` keyMaskKeys `additionalKeysP` stringKeys
    where keyMaskKeys = myKeys ++ myWorkspaceKeys
          stringKeys = myMediaKeys

myKeys = [ ((modm .|. shiftMask, xK_l), spawn "~/bin/lock")
         , ((modm, xK_a), goToSelected defaultGSConfig)
         , ((controlMask, xK_Print), spawn "~/bin/screenshot win")
         , ((0, xK_Print), spawn "~/bin/screenshot scr")
         , ((modm, xK_p), shellPrompt myXPConfig)
         , ((modm .|. shiftMask, xK_a), changeDir myXPConfig)
         , ((modm, xK_b), sendMessage ToggleStruts)
         , ((modm .|. controlMask, xK_backslash), spawn "sudo /usr/bin/systemctl suspend")
         , ((mod4Mask, xK_w), raiseBrowser)
         ]

myWorkspaceKeys =
    [((m .|. modm, k), windows $ f i)
    | (i,k) <- zip (myWorkspaces) [xK_1..]
    , (f,m) <- [(W.greedyView, 0), (W.shift, shiftMask), (copy, shiftMask .|. controlMask)]
    ]
    ++
    [ ((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
    | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
    , (f, m) <- [(W.greedyView, controlMask)]]

myMediaKeys =
    [ ("<XF86AudioLowerVolume>", spawn "~/bin/volume down")
    , ("<XF86AudioRaiseVolume>", spawn "~/bin/volume up")
    , ("<XF86AudioMute>", spawn "~/bin/volume mute")
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
