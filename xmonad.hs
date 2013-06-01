import XMonad
import qualified XMonad.StackSet as W
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Actions.GridSelect
import XMonad.Actions.CopyWindow
import System.IO

modm = mod1Mask

myWorkspaces :: [WorkspaceId]
myWorkspaces = map show [1..9 :: Int]

main = do
    xmproc <- spawnPipe "~/bin/xmobar"
    xmonad $ defaultConfig
        { terminal = "urxvt"
        , workspaces = myWorkspaces
        , manageHook = manageDocks <+> manageHook defaultConfig
        , layoutHook = avoidStruts $ layoutHook defaultConfig
        , modMask = modm
        , logHook = dynamicLogWithPP xmobarPP
            { ppOutput = hPutStrLn xmproc
            , ppTitle = xmobarColor "green" "" . shorten 50
            }
        } `additionalKeys` (
        [ ((modm .|. shiftMask, xK_l), spawn "~/bin/lock")
        , ((modm, xK_g), goToSelected defaultGSConfig)
        , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
        , ((0, xK_Print), spawn "scrot")
        ]
        ++ [((m .|. modm, k), windows $ f i)
            | (i,k) <- zip (myWorkspaces) [xK_1..]
            , (f,m) <- [(W.view, 0), (W.shift, shiftMask), (copy, shiftMask .|. controlMask)]]
        )
