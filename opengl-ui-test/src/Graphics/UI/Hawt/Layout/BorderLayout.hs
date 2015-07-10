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

instance IsWidgetState BorderLayout where
    prefStateSize state = (preferredWidth, preferredHeight)
        where
            preferredWidth = (state^.center^.preferredSize^._1) + (state^.left^.preferredSize._1) + (state^.right^.preferredSize._1)
            preferredHeight = (state^.center^.preferredSize._2) + (state^.top^.preferredSize._2) + (state^.bottom^.preferredSize._2)
    renderState state = renderLayout state
    notifyState panel event = panel

borderLayout :: Widget -> Widget -> Widget -> Widget -> Widget -> Widget
borderLayout center top bottom left right =
    makeStateWidget $ BorderLayout center top bottom left right


renderLayout :: BorderLayout -> GL.GLfloat -> GL.GLfloat -> IO ()
renderLayout bl width height = do
    renderChild (bl^.bottom) 0.0 0.0 width hBottom
    renderChild (bl^.top) 0.0 (height - hTop) width hTop
    renderChild (bl^.left) 0.0 hBottom wLeft (height - hBottom - hTop)
    renderChild (bl^.right) (width - wRight) hBottom wRight (height - hBottom - hTop)
    renderChild (bl^.center) wLeft hBottom (width - wLeft - wRight) (height - hBottom - hTop)
    where
        hBottom = bl^.bottom^.preferredSize._2
        hTop = bl^.top^.preferredSize._2
        wLeft = bl^.left^.preferredSize._1
        wRight = bl^.right^.preferredSize._1
