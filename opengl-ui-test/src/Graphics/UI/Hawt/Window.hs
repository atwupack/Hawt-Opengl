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

module Graphics.UI.Hawt.Window (
    window, show
) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT hiding (Window)
import Graphics.UI.Hawt.Widget
import Data.IORef

import Prelude hiding (init, show)

data Window = Window { title :: String, content :: Widget }

window :: String -> Widget -> Window
window = Window

show :: Window -> IO ()
show (Window title content) = do
    createGLWindow title 1000 600
    initGL
    root <- init content
    newWidget <- newIORef root
    displayCallback $= drawGLScene newWidget
    reshapeCallback $= Just (resizeGLScene newWidget)
    mouseCallback $= Just (notifyMouseEvent newWidget)
    mainLoop

notifyMouseEvent :: IORef Widget -> MouseButton -> KeyState -> Position -> IO ()
notifyMouseEvent widgetState button state (Position x y) = do
    widget <- readIORef widgetState
    Size width height <- get windowSize
    let
        newWidget = notify widget $ MouseMoved (fromIntegral x) (fromIntegral (height-y))
    writeIORef widgetState newWidget


-- Callback for reshapeCallback
resizeGLScene :: IORef Widget -> Size -> IO ()
resizeGLScene widget size@(Size width height) = do
    viewport $= (Position 0 0, size)
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

createGLWindow :: String -> GLsizei -> GLsizei -> IO ()
createGLWindow windowTitle width height = do
    initialDisplayMode $= [DoubleBuffered, RGBAMode,
        WithDepthBuffer,WithStencilBuffer, WithAlphaComponent]
    createWindow windowTitle
    perWindowKeyRepeat $= PerWindowKeyRepeatOff
    windowSize $= Size width height
    actionOnWindowClose $= MainLoopReturns

-- Callback for displayCallback
drawGLScene :: IORef Widget -> IO ()
drawGLScene widgetState = do
    --print "Hallo"
    widget <- readIORef widgetState
    let
        pWidth = fst.prefSize $ widget
        pHeight = snd.prefSize $ widget
        yTranslate = pHeight
    clear [ColorBuffer, DepthBuffer]
    loadIdentity
    Size width height <- get windowSize
    translate $ Vector3 0.0 (minimum [fromIntegral height-pHeight,0.0]) 0.0
    render widget (maximum [pWidth,fromIntegral width]) (maximum [pHeight,fromIntegral height])
    swapBuffers
