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
    UIBackend(..), DisplayCallback
) where

import Graphics.Rendering.OpenGL

class UIBackend a where
    data WindowH a
    createWindow :: a -> String -> Int -> Int -> IO (Maybe (WindowH a))
    initBackend :: IO a
    setDisplayCallback :: WindowH a -> DisplayCallback -> IO ()
    runMainLoop :: WindowH a -> IO ()
    swapBuffers :: WindowH a -> IO ()
    getWindowSize :: WindowH a -> IO (Int, Int)

type DisplayCallback = IO ()
