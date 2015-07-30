-----------------------------------------------------------------------------
--
-- Module      :  Graphics.UI.Hawt.Window
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
module Graphics.UI.Hawt.Window (
    window, show
) where

import Graphics.Rendering.OpenGL
import Graphics.UI.Hawt.Widget
import Graphics.UI.Hawt.Drawing
import Graphics.UI.Hawt.Backend
import Data.IORef

import Control.Monad.Trans.Reader

import Prelude hiding (init, show)

data Window = Window { title :: String, content :: Widget }

window :: String -> Widget -> Window
window = Window

show :: (UIBackend a) => a -> Window -> IO ()
show be (Window title content) = do
    Just w <- createWindow be title 1000 600
    initGL
    root <- init content
    newWidget <- newIORef root
    setResizeCallback w resizeGLScene
    setDisplayCallback w $ drawGLScene w newWidget newContext
    runMainLoop w

-- Callback for reshapeCallback
resizeGLScene :: Int -> Int -> IO ()
resizeGLScene width height = do
    viewport $= (Position 0 0, Size (fromIntegral width) (fromIntegral height))
    matrixMode $= Projection
    loadIdentity
    ortho2D 0.0 (fromIntegral (width-1)) 0.0 (fromIntegral (height-1))
    matrixMode $= Modelview 0

initGL :: IO ()
initGL = do
    shadeModel $= Smooth
    clearColor $= Color4 0.0 0.0 0.0 0.0
    clearDepth $= 1.0
    depthFunc $= Just Lequal
    hint PerspectiveCorrection $= Nicest



-- Callback for displayCallback
drawGLScene :: (UIBackend a) => WindowH a -> IORef Widget -> RenderContext -> IO ()
drawGLScene window widgetState context = do
    --print "Hallo"
    widget <- readIORef widgetState
    let
        pWidth = fst.prefSize $ widget
        pHeight = snd.prefSize $ widget
        yTranslate = pHeight
    clear [ColorBuffer, DepthBuffer]
    loadIdentity
    (width, height) <- getWindowSize window
    translate $ Vector3 0.0 (minimum [fromIntegral height-pHeight,0.0]) 0.0
    runReaderT (render widget (maximum [pWidth,fromIntegral width]) (maximum [pHeight,fromIntegral height])) context
    swapBuffers window
