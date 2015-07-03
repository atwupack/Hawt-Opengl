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
import Control.Lens

data Panel = Panel (GL.Color4 GL.GLfloat) (Maybe Widget)


emptyPanel :: GL.Color4 GL.GLfloat -> Widget
emptyPanel color = panelWidget (Panel color Nothing) (100, 100)

panel :: GL.Color4 GL.GLfloat -> Widget -> Widget
panel color child = panelWidget (Panel color (Just child)) (child^.preferredSize)

panelWidget :: Panel -> (GL.GLfloat, GL.GLfloat) -> Widget
panelWidget panel pSize = Widget (renderPanel panel) pSize

renderPanel ::  Panel -> GL.GLfloat -> GL.GLfloat -> IO Widget
renderPanel p@(Panel color Nothing) width height = do
    GL.color color
    GL.renderPrimitive GL.Quads $ do
        GL.vertex $ GL.Vertex2 0 (0 :: GL.GLfloat)
        GL.vertex $ GL.Vertex2 width (0 :: GL.GLfloat)
        GL.vertex $ GL.Vertex2 width (height :: GL.GLfloat)
        GL.vertex $ GL.Vertex2 0 (height :: GL.GLfloat)
    return $ panelWidget p (width, height)
renderPanel p@(Panel color (Just child)) width height = do
    renderPanel (Panel color Nothing) width height
    _render child width height
    return $ panelWidget p (width, height)
