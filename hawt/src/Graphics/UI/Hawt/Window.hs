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
{-# LANGUAGE TypeFamilies, RankNTypes #-}
module Graphics.UI.Hawt.Window (
    window, show
) where

import Graphics.Rendering.OpenGL
import Graphics.UI.Hawt.Widget
import Graphics.UI.Hawt.Drawing
import Graphics.UI.Hawt.Backend
import Data.IORef
import Reactive.Banana.Frameworks
import Reactive.Banana

import Control.Monad.Trans.Reader

import Prelude hiding (init, show)

data Window = Window { title :: String, content :: Widget }

window :: String -> Widget -> Window
window = Window

show :: (UIBackend a) => a -> Window -> IO ()
show be (Window title content) = do
    Just (w, ah) <- createWindow be title 1000 600
    initGL
    root <- init content
    newWidget <- newIORef root
    let networkDescription :: forall t. Frameworks t => Moment t ()
        networkDescription = do
            backendEvent <- fromAddHandler ah
            reactimate $ fmap (drawGLScene w newWidget newContext) backendEvent

    network <- compile networkDescription
    actuate network
    runMainLoop w

-- Callback for reshapeCallback
resizeGLScene :: Int -> Int -> IO ()
resizeGLScene width height = do
    viewport $= (Position 0 0, Size (fromIntegral width) (fromIntegral height))
    matrixMode $= Projection
    loadIdentity
    ortho2D 0.0 (fromIntegral width) 0.0 (fromIntegral height)
    matrixMode $= Modelview 0

initGL :: IO ()
initGL = do
    shadeModel $= Smooth
    clearColor $= Color4 0.0 0.0 0.0 0.0
    clearDepth $= 1.0
    depthFunc $= Just Lequal
    hint PerspectiveCorrection $= Nicest



-- Callback for displayCallback
drawGLScene :: (UIBackend a) => WindowH a -> IORef Widget -> RenderContext-> BackendEvent -> IO ()
drawGLScene window widgetState context event = do
    (width, height) <- getWindowSize window
    resizeGLScene width height
    widget <- readIORef widgetState
    let
        pWidth = fst.prefSize $ widget
        pHeight = snd.prefSize $ widget
        yTranslate = pHeight
    clear [ColorBuffer, DepthBuffer]
    loadIdentity
    translate $ Vector3 0.0 (minimum [fromIntegral height-pHeight,0.0]) 0.0
    runReaderT (render widget (maximum [pWidth,fromIntegral width]) (maximum [pHeight,fromIntegral height])) context
    swapBuffers window
