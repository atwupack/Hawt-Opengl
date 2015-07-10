-----------------------------------------------------------------------------
--
-- Module      :  Graphics.UI.Hawt.Widget.Label
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
module Graphics.UI.Hawt.Widget.Label (
    label
) where

import Graphics.Rendering.OpenGL.GL
import Graphics.Rendering.FTGL as FTGL
import Graphics.UI.Hawt.Widget
import Control.Lens

data Label = Label { _text :: String, _font :: String, _lcolor :: Color4 GLfloat}
makeLenses ''Label

instance IsWidgetState Label where
    renderState = renderLabel
    prefStateSize _ = (100,100)
    notifyState l event = l

label :: String -> String -> Color4 GLfloat -> Widget
label text font c = makeStateWidget $ Label text font c

renderLabel :: Label -> GLfloat -> GLfloat -> IO ()
renderLabel l width height = do
    lighting $= Disabled
    preservingMatrix $ do
        color (l^.lcolor)
        FTGL.createSimpleLayout
        --font <- createTextureFont $ "ddjkf"
        currentRasterPosition $= Vertex4 0.0 (height) 0.0 1.0
        --setFontFaceSize font 24 72
        --renderFont font "Hello world!" FTGL.Front
