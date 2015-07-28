-----------------------------------------------------------------------------
--
-- Module      :  Graphics.UI.Hawt.Widget.Button
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
{-# LANGUAGE NamedFieldPuns #-}
module Graphics.UI.Hawt.Widget.Button (
    button
) where

import Graphics.UI.Hawt.Widget
import Graphics.UI.Hawt.Widget.Label
import Graphics.UI.Hawt.Drawing
import Graphics.Rendering.OpenGL as GL
import Prelude hiding (init)

data Button = Button { blabel :: Widget , bcolor :: Color4 GLfloat}


instance IsWidgetState Button where
    renderState = renderButton
    prefStateSize Button {blabel} = (lw + 20.0, lh + 20.0)
        where
            (lw, lh) = prefSize blabel
    notifyState b event = b
    initState Button{blabel, bcolor} = Button <$> init blabel <*> return bcolor

button :: String -> Color4 GLfloat -> Widget
button text lcolor = makeStateWidget $ Button blabel lcolor
    where
        blabel = label text "c:\\Projekte\\arial.ttf" (Color4 0.0 0.0 0.0 1.0)

renderButton :: Button -> GLfloat -> GLfloat -> RenderC
renderButton (Button {blabel, bcolor}) width height = do
    renderBox (Color3 0 0 (0::GLfloat)) 0 0 width height
    gl $ GL.renderPrimitive GL.Quads $ do
        GL.color bcolor
        GL.vertex $ GL.Vertex2 2.0 (2.0 :: GL.GLfloat)
        GL.color $ darker bcolor
        GL.vertex $ GL.Vertex2 (width-2.0) 2.0
        GL.color bcolor
        GL.vertex $ GL.Vertex2 (width-2.0) (height-2.0)
        GL.color $ lighter bcolor
        GL.vertex $ GL.Vertex2 2.0 (height-2.0)
    renderChild blabel lx ly lw lh
    where
        lx = (width - lw) / 2.0
        ly = (height - lh) / 2.0
        lw = fst.prefSize $ blabel
        lh = snd.prefSize $ blabel

lighter :: Color4 GLfloat -> Color4 GLfloat
lighter (Color4 r g b a) = Color4 (lc r) (lc g) (lc b) a
    where
        lc c = c+((1.0-c)*0.25)

darker :: Color4 GLfloat -> Color4 GLfloat
darker (Color4 r g b a) = Color4 (r*0.75) (g*0.75) (b*0.75) a
