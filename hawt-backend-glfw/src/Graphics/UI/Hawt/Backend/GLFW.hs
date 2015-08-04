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
import Control.Event.Handler
import Reactive.Banana

data GLFWBackend = GLFWBackend

instance UIBackend GLFWBackend where
    data WindowH GLFWBackend = GLFWWindow Window
    initBackend = do
        result <- GLFW.init
        return GLFWBackend
    createWindow GLFWBackend windowTitle width height  = do
        defaultWindowHints
        Just wh <- GLFW.createWindow width height windowTitle Nothing Nothing
        ah <- createEvents wh
        return $ Just (GLFWWindow wh, ah)
    swapBuffers (GLFWWindow window) = GLFW.swapBuffers window
    getWindowSize (GLFWWindow window) = GLFW.getWindowSize window
    runMainLoop (GLFWWindow window) = mainLoopGLFW window

mainLoopGLFW :: Window -> IO ()
mainLoopGLFW window = do
    makeContextCurrent $ Just window
    swapInterval 1
    whileM_ (not <$> windowShouldClose window) waitEvents
    destroyWindow window
    terminate

createEvents :: Window -> IO (AddHandler BackendEvent)
createEvents w = do
    (addHandler, fire) <- newAddHandler
    setWindowRefreshCallback w (Just (windowRefresh fire))
    return addHandler


windowRefresh :: Handler BackendEvent-> Window -> IO()
windowRefresh fire w = fire RepaintEvent
