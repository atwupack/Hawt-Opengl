-----------------------------------------------------------------------------
--
-- Module      :  Graphics.UI.Hawt.Drawing
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

module Graphics.UI.Hawt.Drawing (
    renderBox, RenderC, gl, RenderContext, newContext
) where

import Graphics.Rendering.OpenGL
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class

renderBox ::(Color a) => a -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> RenderC
renderBox c xp yp width height =
    gl $ do
        color c
        renderPrimitive Quads $ do
            vertex $ Vertex2 xp yp
            vertex $ Vertex2 (xp+width) yp
            vertex $ Vertex2 (xp+width) (yp+height)
            vertex $ Vertex2 xp (yp+height)

data RenderContext = RenderContext

newContext :: RenderContext
newContext = RenderContext

type RenderC = ReaderT RenderContext IO ()

gl :: IO () -> RenderC
gl = liftIO

