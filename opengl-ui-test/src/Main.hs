import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.IORef
import Control.Monad
import Control.Lens

import Graphics.UI.Hawt.Widget
import Graphics.UI.Hawt.Widget.Panel
import Graphics.UI.Hawt.Widget.Label
import Graphics.UI.Hawt.Layout.BorderLayout

-- Callback for reshapeCallback
resizeGLScene :: Size -> IO ()
resizeGLScene (Size width 0) = resizeGLScene (Size width 1)
resizeGLScene size@(Size width height) = do
    viewport $= (Position 0 0, size)
    matrixMode $= Projection
    loadIdentity
    ortho2D 0.0 (fromIntegral (width-1)) 0.0 (fromIntegral (height-1))
    matrixMode $= Modelview 0


buildSimpleUI :: Widget
buildSimpleUI = emptyPanel (Color4 1.0 0.0 0.0 1.0)

-- Build a dummy UI
buildUI :: Widget
buildUI = borderLayout
    (panel
        (Color4 1.0 0.0 0.0 1.0)
        (borderLayout
            (emptyPanel $ Color4 0.0 0.0 0.0 1.0)
            (emptyPanel $ Color4 0.2 0.2 0.2 1.0)
            (emptyPanel $ Color4 0.4 0.4 0.4 1.0)
            (emptyPanel $ Color4 0.6 0.6 0.6 1.0)
            (emptyPanel $ Color4 0.8 0.8 0.8 1.0)))
    (emptyPanel $ Color4 0.0 1.0 0.0 1.0)
    (emptyPanel $ Color4 0.0 0.0 1.0 1.0)
    (emptyPanel $ Color4 1.0 1.0 0.0 1.0)
    (panel
        (Color4 1.0 0.0 1.0 1.0)
        (label "Hallo" "c:\\Projekte\\arial.ttf" (Color4 1.0 1.0 1.0 1.0)))

-- Callback for displayCallback
drawGLScene :: IORef Widget -> IO ()
drawGLScene widgetState = do
    print "Hallo"
    widget <- readIORef widgetState
    let
        pWidth = widget^.preferredSize._1
        pHeight = widget^.preferredSize._2
        yTranslate = pHeight
    clear [ColorBuffer, DepthBuffer]
    loadIdentity
    Size width height <- get windowSize
    translate $ Vector3 0.0 (minimum [(fromIntegral height)-pHeight,0.0]) 0.0
    (widget^.render) (maximum [pWidth,(fromIntegral width)]) (maximum [pHeight,(fromIntegral height)])
    swapBuffers

notifyMouseEvent :: IORef Widget -> MouseButton -> KeyState -> Position -> IO ()
notifyMouseEvent widgetState button state (Position x y) = do
    widget <- readIORef widgetState
    Size width height <- get windowSize
    let
        newWidget = (widget^.notify) $ MouseMoved (fromIntegral x) (fromIntegral (height-y))
    writeIORef widgetState newWidget




createGLWindow :: String -> GLsizei -> GLsizei -> IO ()
createGLWindow windowTitle width height = do
    initialDisplayMode $= [DoubleBuffered, RGBAMode,
        WithDepthBuffer,WithStencilBuffer, WithAlphaComponent]
    createWindow windowTitle
    perWindowKeyRepeat $= PerWindowKeyRepeatOff
    windowSize $= Size width height
    actionOnWindowClose $= Exit

initGL = do
    shadeModel $= Smooth
    clearColor $= Color4 0.0 0.0 0.0 0.0
    clearDepth $= 1.0
    depthFunc $= Just Lequal
    hint PerspectiveCorrection $= Nicest


main = do
    getArgsAndInitialize
    createGLWindow "Test" 1000 600
    initGL
    newWidget <- newIORef buildUI
    --newWidget <- newIORef buildSimpleUI
    displayCallback $= drawGLScene newWidget
    reshapeCallback $= Just resizeGLScene
    mouseCallback $= Just (notifyMouseEvent newWidget)
    mainLoop


