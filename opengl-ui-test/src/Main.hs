
import Graphics.UI.Hawt.Window
import Graphics.UI.Hawt.Widget
import Graphics.UI.Hawt.Widget.Panel
import Graphics.UI.Hawt.Widget.Button
import Graphics.UI.Hawt.Widget.Label
import Graphics.UI.Hawt.Layout.BorderLayout
import Graphics.UI.Hawt.Backend.GLUT
import Graphics.UI.Hawt.Backend.GLFW
import Graphics.UI.Hawt.Backend
import Graphics.Rendering.OpenGL

import Prelude hiding (init, show, Left, Right)

-- Build a dummy UI
buildUI :: UI BorderLayout
buildUI = borderLayout
            +> (Center, panel (Color4 1.0 0.0 0.0 1.0)
                +> (Content, borderLayout
                    +> (Center, panel (Color4 0.0 0.0 0.0 1.0)
                        +> (Content, button "Click me!" (Color4 0.7 0.7 0.7 1.0)))
                    +> (Top, panel $ Color4 0.2 0.2 0.2 1.0)
                    +> (Bottom, panel $ Color4 0.4 0.4 0.4 1.0)
                    +> (Left, panel $ Color4 0.6 0.6 0.6 1.0)
                    +> (Right, panel $ Color4 0.8 0.8 0.8 1.0)))
            +> (Top, panel $ Color4 0.0 1.0 0.0 1.0)
            +> (Bottom, panel $ Color4 0.0 0.0 1.0 1.0)
            +> (Left, panel (Color4 1.0 1.0 0.0 1.0)
                +> (Content, label "goog" "c:\\Projekte\\arial.ttf" (Color4 0.0 0.0 0.0 1.0)))
            +> (Right, panel (Color4 1.0 0.0 1.0 1.0)
                +> (Content, label "Hello" "c:\\Projekte\\arial.ttf" (Color4 0.0 0.0 0.0 1.0)))

main :: IO()
main = do
    --be <- initBackend :: IO GLUTBackend
    be <- initBackend :: IO GLFWBackend
    show be $ window "Test" buildUI
