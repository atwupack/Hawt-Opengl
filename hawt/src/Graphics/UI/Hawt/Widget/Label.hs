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
{-# LANGUAGE NamedFieldPuns #-}
module Graphics.UI.Hawt.Widget.Label (
    label
) where

import Graphics.Rendering.OpenGL.GL
import Graphics.UI.Hawt.Widget
import Graphics.UI.Hawt.Drawing
import Graphics.Rendering.FTGL as FTGL

data Label = Label { text :: String, fontpath :: String, lcolor :: Color4 GLfloat}
                | InitLabel { text :: String, font :: Font, lcolor :: Color4 GLfloat, size :: (GLfloat, GLfloat) }

instance IsWidgetState Label where
    renderState = renderLabel
    prefStateSize Label {} = (0,0)
    prefStateSize InitLabel {size} = size
    notifyState l event = l
    initState = initLabel

label :: String -> String -> Color4 GLfloat -> Widget
label text font c = makeStateWidget $ Label text font c


initLabel :: Label -> IO Label
initLabel (Label text font lcolor) = do
    --loadedfont <- createTextureFont font
    loadedfont <- createPixmapFont font
    setFontFaceSize loadedfont 14 72
    [llx,lly,llz,urx,ury,urz] <- getFontBBox loadedfont text
    let
        asc = realToFrac $ getFontAscender loadedfont
        desc = realToFrac $ getFontDescender loadedfont
    return $ InitLabel text loadedfont lcolor (realToFrac urx, asc+desc)
initLabel l@InitLabel {} = return l

renderLabel :: Label -> GLfloat -> GLfloat -> RenderC
renderLabel (InitLabel t f c s) width height = do
    lighting $= Disabled
    gl $ preservingMatrix $ do
        color c
        let
            asc = getFontAscender f
            desc = getFontDescender f
        rasterPos $ Vertex2 0.0 (height-realToFrac asc)
        renderFont f t FTGL.All
renderLabel (Label t f c) width height = error "Not initialized font"

