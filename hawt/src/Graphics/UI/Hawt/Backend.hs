-----------------------------------------------------------------------------
--
-- Module      :  Graphics.UI.Hawt.Backend
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies #-}
module Graphics.UI.Hawt.Backend (
    UIBackend(..), BackendEvent(..)
) where

import Graphics.Rendering.OpenGL
import Reactive.Banana.Frameworks

class UIBackend a where
    data WindowH a
    createWindow :: a -> String -> Int -> Int -> IO (Maybe (WindowH a, AddHandler BackendEvent))
    initBackend :: IO a
    runMainLoop :: WindowH a -> IO ()
    swapBuffers :: WindowH a -> IO ()
    getWindowSize :: WindowH a -> IO (Int, Int)

data BackendEvent = RepaintEvent
