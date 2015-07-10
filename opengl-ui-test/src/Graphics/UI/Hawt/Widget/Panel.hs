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
{-# LANGUAGE TemplateHaskell #-}
module Graphics.UI.Hawt.Widget.Panel (
    emptyPanel,
    panel

) where

import qualified Graphics.Rendering.OpenGL as GL

import Graphics.UI.Hawt.Widget
import Control.Lens

data Panel = Panel { _color :: GL.Color4 GL.GLfloat, _child ::Maybe Widget }
makeLenses ''Panel

instance IsWidgetState  Panel where
    renderState = renderPanel
    prefStateSize (Panel _ Nothing) = (100,100)
    prefStateSize (Panel _ (Just w)) = w^.preferredSize
    notifyState (Panel _ Nothing) event = Panel (GL.Color4 1.0 1.0 1.0 1.0) Nothing
    notifyState (Panel color (Just w)) event = Panel color (Just newChild)
        where
            newChild = (w^.notify) event

-- create an empty panel w/o any content
emptyPanel :: GL.Color4 GL.GLfloat -> Widget
emptyPanel color = makeStateWidget (Panel color Nothing)

-- create a panel with content
panel :: GL.Color4 GL.GLfloat -> Widget -> Widget
panel color child = makeStateWidget (Panel color (Just child))


renderPanel ::  Panel -> GL.GLfloat -> GL.GLfloat -> IO ()
renderPanel p@(Panel color Nothing) width height = do
    GL.color color
    GL.renderPrimitive GL.Quads $ do
        GL.vertex $ GL.Vertex2 0 (0 :: GL.GLfloat)
        GL.vertex $ GL.Vertex2 width 0
        GL.vertex $ GL.Vertex2 width height
        GL.vertex $ GL.Vertex2 0 height
renderPanel p@(Panel color (Just child)) width height = do
    renderPanel (Panel color Nothing) width height
    renderChild child 0.0 0.0 width height

