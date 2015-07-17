-----------------------------------------------------------------------------
--
-- Module      :  Graphics.UI.Hawt.Widget.Panel
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

module Graphics.UI.Hawt.Widget.Panel (
    emptyPanel,
    panel

) where

import qualified Graphics.Rendering.OpenGL as GL

import Graphics.UI.Hawt.Widget
import Prelude hiding (init)
import Control.Arrow

data Panel = Panel { color :: GL.Color4 GL.GLfloat}


instance IsContainerState Panel where
    initStateC = return
    renderStateC = renderPanel
    prefStateSizeC p = foldl cmax (0,0)
        where
            cmax (w,h) current = (max w *** max h)(prefSize current)
    notifyStateC p event = p

-- create an empty panel w/o any content
emptyPanel :: GL.Color4 GL.GLfloat -> Widget
emptyPanel color = makeStateContainer $ Panel color

-- create a panel with content
panel :: GL.Color4 GL.GLfloat -> Widget
panel color = makeStateContainer $ Panel color


renderPanel ::  Panel -> [Widget] -> GL.GLfloat -> GL.GLfloat -> IO ()
renderPanel p@(Panel color) [] width height = do
    GL.color color
    GL.renderPrimitive GL.Quads $ do
        GL.vertex $ GL.Vertex2 0 (0 :: GL.GLfloat)
        GL.vertex $ GL.Vertex2 width 0
        GL.vertex $ GL.Vertex2 width height
        GL.vertex $ GL.Vertex2 0 height
renderPanel p@(Panel color) children width height = do
    renderPanel p [] width height
    mapM_ (\x -> renderChild x 0.0 0.0 width height) children

