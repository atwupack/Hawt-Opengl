import Graphics.UI.GLUT
import Graphics.UI.Hawt.Window
import Graphics.UI.Hawt.Widget
import Graphics.UI.Hawt.Widget.Panel
import Graphics.UI.Hawt.Widget.Button
import Graphics.UI.Hawt.Widget.Label
import Graphics.UI.Hawt.Layout.BorderLayout
import Prelude hiding (init, show)



-- Build a dummy UI
buildUI :: Widget
buildUI = borderLayout
    (panel (Color4 1.0 0.0 0.0 1.0)
        +> borderLayout
            (panel (Color4 0.0 0.0 0.0 1.0)
                +> button "Click me!" (Color4 0.7 0.7 0.7 1.0))
            (emptyPanel $ Color4 0.2 0.2 0.2 1.0)
            (emptyPanel $ Color4 0.4 0.4 0.4 1.0)
            (emptyPanel $ Color4 0.6 0.6 0.6 1.0)
            (emptyPanel $ Color4 0.8 0.8 0.8 1.0))
    (emptyPanel $ Color4 0.0 1.0 0.0 1.0)
    (emptyPanel $ Color4 0.0 0.0 1.0 1.0)
    (panel (Color4 1.0 1.0 0.0 1.0)
        +> label "goog" "c:\\Projekte\\arial.ttf" (Color4 0.0 0.0 0.0 1.0))
    (panel (Color4 1.0 0.0 1.0 1.0)
        +> label "Hello" "c:\\Projekte\\arial.ttf" (Color4 0.0 0.0 0.0 1.0))

main = do
    getArgsAndInitialize
    show $ window "Test" buildUI


