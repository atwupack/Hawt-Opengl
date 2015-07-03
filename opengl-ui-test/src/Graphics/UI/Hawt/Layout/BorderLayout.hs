-----------------------------------------------------------------------------
--
-- Module      :  Graphics.UI.Hawt.Layout.BorderLayout
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
module Graphics.UI.Hawt.Layout.BorderLayout (
    borderLayout
) where

import Graphics.UI.Hawt.Widget
import qualified Graphics.Rendering.OpenGL as GL
import Control.Lens

data BorderLayout = BorderLayout {  _center :: Widget,
                                    _top :: Widget,
                                    _bottom :: Widget,
                                    _left :: Widget,
                                    _right :: Widget }
makeLenses ''BorderLayout

borderLayout :: Widget -> Widget -> Widget -> Widget -> Widget -> Widget
borderLayout center top bottom left right =
    Widget (renderLayout (BorderLayout center top bottom left right)) (preferredWidth, preferredHeight)
    where
        preferredWidth = (center^.preferredSize^._1) + (left^.preferredSize._1) + (right^.preferredSize._1)
        preferredHeight = (center^.preferredSize._2) + (top^.preferredSize._2) + (bottom^.preferredSize._2)


renderLayout :: BorderLayout -> GL.GLfloat -> GL.GLfloat -> IO Widget
renderLayout bl width height = do
    GL.preservingMatrix $ do
        _render (bl^.bottom) width hBottom
    GL.preservingMatrix $ do
        GL.translate $ GL.Vector3 0.0 (height - hTop) 0.0
        _render (bl^.top) width hTop
    GL.preservingMatrix $ do
        GL.translate $ GL.Vector3 0.0 hBottom 0.0
        _render (bl^.left) wLeft (height - hBottom - hTop)
    GL.preservingMatrix $ do
        GL.translate $ GL.Vector3 (width - wRight) hBottom 0.0
        _render (bl^.right) wRight (height - hBottom - hTop)
    GL.preservingMatrix $ do
        GL.translate $ GL.Vector3 wLeft hBottom 0.0
        _render (bl^.center) (width - wLeft - wRight) (height - hBottom - hTop)
    where
        hBottom = bl^.bottom^.preferredSize._2
        hTop = bl^.top^.preferredSize._2
        wLeft = bl^.left^.preferredSize._1
        wRight = bl^.right^.preferredSize._1
