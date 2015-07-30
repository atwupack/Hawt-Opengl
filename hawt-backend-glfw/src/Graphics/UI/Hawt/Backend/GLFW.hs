-----------------------------------------------------------------------------
--
-- Module      :  Graphics.UI.Hawt.Backend.GLFW
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
module Graphics.UI.Hawt.Backend.GLFW (
    GLFWBackend
) where

import Graphics.UI.GLFW as GLFW
import Graphics.UI.Hawt.Backend
import Control.Monad.Loops

data GLFWBackend = GLFWBackend

instance UIBackend GLFWBackend where
    data WindowH GLFWBackend = GLFWWindow Window
    initBackend = do
        result <- GLFW.init
        return GLFWBackend
    createWindow GLFWBackend windowTitle width height  = do
        --initialDisplayMode $= [DoubleBuffered, RGBAMode,
        --    WithDepthBuffer,WithStencilBuffer, WithAlphaComponent]
        wh <- GLFW.createWindow width height windowTitle Nothing Nothing
        --perWindowKeyRepeat $= PerWindowKeyRepeatOff
        -- actionOnWindowClose $= MainLoopReturns
        return $ GLFWWindow <$> wh
    setResizeCallback (GLFWWindow window) callback = setWindowSizeCallback window (Just (reshapeInt callback))
    setDisplayCallback (GLFWWindow window) callback = setWindowRefreshCallback window (Just (refreshInt callback))
    swapBuffers (GLFWWindow window) = GLFW.swapBuffers window
    getWindowSize (GLFWWindow window) = GLFW.getWindowSize window
    runMainLoop (GLFWWindow window) = mainLoopGLFW window

reshapeInt :: ResizeCallback -> WindowSizeCallback
reshapeInt resizeCallback window = resizeCallback

refreshInt :: DisplayCallback -> WindowRefreshCallback
refreshInt refreshCallback window = refreshCallback

mainLoopGLFW :: Window -> IO ()
mainLoopGLFW window = whileM_ (windowShouldClose window) waitEvents

