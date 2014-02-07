{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}

module JackStack (
    JackStack(JackStack)) where

import XMonad
import qualified XMonad.StackSet as W
import Data.Ratio

data JackStack a = JackStack deriving ( Read, Show )

instance LayoutClass JackStack Window where
    pureLayout _ (Rectangle sx sy sw sh) ws =
        if total == 0
           then [(W.focus ws, Rectangle sx sy sw sh)]
           else (focus:(reverse tops)) ++ bottoms
     where
        ups = W.up ws
        dns = W.down ws
        total = (length ups) + (length dns)
        delta = div (sw) $ fromIntegral (4 * (total))
        getPos i = Rectangle
            { rect_x = sx + (fromIntegral (delta * (fromIntegral i)))
            , rect_y = sy
            , rect_height = sh
            , rect_width = div (3 * (sw)) (fromIntegral 4)
            }
        focus = ((W.focus ws), getPos (length ups))
        tops = zip (reverse ups) (map getPos [0..])
        bottoms = zip dns (map getPos[((length ups) + 1) ..])
