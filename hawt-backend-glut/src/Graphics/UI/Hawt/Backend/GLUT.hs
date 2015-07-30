
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

data GLUTBackend = GLUTBackend

instance UIBackend GLUTBackend where
    data WindowH GLUTBackend = GLUTWindow Window
    initBackend = do
        getArgsAndInitialize
        return GLUTBackend
    createWindow GLUTBackend windowTitle width height  = do
        initialDisplayMode $= [DoubleBuffered, RGBAMode,
            WithDepthBuffer,WithStencilBuffer, WithAlphaComponent]
        wh <- GLUT.createWindow windowTitle
        perWindowKeyRepeat $= PerWindowKeyRepeatOff
        windowSize $= Size (fromIntegral width) (fromIntegral height)
        actionOnWindowClose $= MainLoopReturns
        return $ Just (GLUTWindow wh)
    setResizeCallback window callback = reshapeCallback $= Just (reshapeInt callback)
    setDisplayCallback window callback = displayCallback $= callback
    swapBuffers window = GLUT.swapBuffers
    getWindowSize window = do
        Size width height <- get windowSize
        return (fromIntegral width, fromIntegral height)
    runMainLoop be = GLUT.mainLoop

reshapeInt :: ResizeCallback -> ReshapeCallback
reshapeInt resizeCallback (Size width height) = resizeCallback (fromIntegral width) (fromIntegral height)
