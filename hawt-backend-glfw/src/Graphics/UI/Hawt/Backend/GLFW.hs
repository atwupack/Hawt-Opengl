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
        --windowHint $ WindowHint'sRGBCapable True
        defaultWindowHints
        wh <- GLFW.createWindow width height windowTitle Nothing Nothing
        --perWindowKeyRepeat $= PerWindowKeyRepeatOff
        -- actionOnWindowClose $= MainLoopReturns
        return $ GLFWWindow <$> wh
    setDisplayCallback (GLFWWindow window) callback = setWindowRefreshCallback window (Just (refreshInt callback))
    swapBuffers (GLFWWindow window) = GLFW.swapBuffers window
    getWindowSize (GLFWWindow window) = GLFW.getWindowSize window
    runMainLoop (GLFWWindow window) = mainLoopGLFW window

refreshInt :: DisplayCallback -> WindowRefreshCallback
refreshInt refreshCallback window = refreshCallback

mainLoopGLFW :: Window -> IO ()
mainLoopGLFW window = do
    makeContextCurrent $ Just window
    swapInterval 1
    whileM_ (not <$> windowShouldClose window) waitEvents
    destroyWindow window
    terminate
