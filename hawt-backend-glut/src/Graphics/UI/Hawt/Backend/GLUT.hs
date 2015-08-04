
-----------------------------------------------------------------------------
--
-- Module      :  Graphics.UI.Hawt.Backend.GLUT
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
module Graphics.UI.Hawt.Backend.GLUT (
    GLUTBackend
) where

import Graphics.UI.GLUT as GLUT
import Graphics.UI.Hawt.Backend
import Control.Event.Handler
import Reactive.Banana


data GLUTBackend = GLUTBackend

instance UIBackend GLUTBackend where
    data WindowH GLUTBackend = GLUTWindow Window
    initBackend = do
        getArgsAndInitialize
        return GLUTBackend
    createWindow GLUTBackend windowTitle width height  = do
        initialDisplayMode $= [DoubleBuffered, RGBAMode, WithAlphaComponent]
        wh <- GLUT.createWindow windowTitle
        perWindowKeyRepeat $= PerWindowKeyRepeatOff
        windowSize $= Size (fromIntegral width) (fromIntegral height)
        actionOnWindowClose $= MainLoopReturns
        ah <- createEvents wh
        return $ Just (GLUTWindow wh, ah)
    swapBuffers window = GLUT.swapBuffers
    getWindowSize window = do
        Size width height <- get windowSize
        return (fromIntegral width, fromIntegral height)
    runMainLoop be = GLUT.mainLoop

createEvents :: Window -> IO (AddHandler BackendEvent)
createEvents w = do
    (addHandler, fire) <- newAddHandler
    displayCallback $= windowRefresh fire w
    return addHandler

windowRefresh :: Handler BackendEvent-> Window -> IO()
windowRefresh fire w = fire RepaintEvent
