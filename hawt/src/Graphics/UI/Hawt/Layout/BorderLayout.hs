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
{-# LANGUAGE NamedFieldPuns, TypeFamilies #-}
module Graphics.UI.Hawt.Layout.BorderLayout (
    borderLayout, BorderLayout, BLPosition(..)
) where

import Control.Applicative
import Graphics.UI.Hawt.Widget
import Graphics.UI.Hawt.Drawing
import qualified Graphics.Rendering.OpenGL as GL
import Prelude hiding (init, Left, Right)

data BorderLayout = BorderLayout {  center :: Widget,
                                    top :: Widget,
                                    bottom :: Widget,
                                    left :: Widget,
                                    right :: Widget }

data BLPosition = Center | Left | Right | Top | Bottom

instance IsWidgetState BorderLayout where
    prefStateSize state = (preferredWidth, preferredHeight)
        where
            preferredWidth = (fst.prefSize.center) state + (fst.prefSize.left) state + (fst.prefSize.right) state
            preferredHeight = (snd.prefSize.center) state + (snd.prefSize.top) state + (snd.prefSize.bottom) state
    renderState = renderLayout
    notifyState bl event = bl
    initState = initLayout

instance IsContainerState BorderLayout where
    type ChildType BorderLayout = BLPosition
    addChild bl Center child = bl {center=child}
    addChild bl Top child = bl {top=child}
    addChild bl Bottom child = bl {bottom=child}
    addChild bl Left child = bl {left=child}
    addChild bl Right child = bl {right=child}

borderLayout ::  UI BorderLayout
borderLayout = widget $ BorderLayout emptyWidget emptyWidget emptyWidget emptyWidget emptyWidget

initLayout :: BorderLayout -> IO BorderLayout
initLayout (BorderLayout c t b l r) = BorderLayout <$> init c <*> init t <*> init b <*> init l <*> init r

renderLayout :: BorderLayout -> GL.GLfloat -> GL.GLfloat -> RenderC
renderLayout bl width height = do
    renderChild (bottom bl) 0.0 0.0 width hBottom
    renderChild (top bl) 0.0 (height - hTop) width hTop
    renderChild (left bl) 0.0 hBottom wLeft (height - hBottom - hTop)
    renderChild (right bl) (width - wRight) hBottom wRight (height - hBottom - hTop)
    renderChild (center bl) wLeft hBottom (width - wLeft - wRight) (height - hBottom - hTop)
    where
        hBottom =  snd.prefSize.bottom $ bl
        hTop = snd.prefSize.top $ bl
        wLeft = fst.prefSize.left $ bl
        wRight = fst.prefSize.right $ bl
