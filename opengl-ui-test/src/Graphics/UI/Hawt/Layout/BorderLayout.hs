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

module Graphics.UI.Hawt.Layout.BorderLayout (
    borderLayout
) where

import Graphics.UI.Hawt.Widget
import qualified Graphics.Rendering.OpenGL as GL
import Prelude hiding (init)

data BorderLayout = BorderLayout {  center :: Widget,
                                    top :: Widget,
                                    bottom :: Widget,
                                    left :: Widget,
                                    right :: Widget }

instance IsWidgetState BorderLayout where
    prefStateSize state = (preferredWidth, preferredHeight)
        where
            preferredWidth = (fst.prefSize.center) state + (fst.prefSize.left) state + (fst.prefSize.right) state
            preferredHeight = (snd.prefSize.center) state + (snd.prefSize.top) state + (snd.prefSize.bottom) state
    renderState = renderLayout
    notifyState bl event = bl
    initState = initLayout

borderLayout :: Widget -> Widget -> Widget -> Widget -> Widget -> Widget
borderLayout center top bottom left right =
    makeStateWidget $ BorderLayout center top bottom left right

initLayout :: BorderLayout -> IO BorderLayout
initLayout (BorderLayout c t b l r) = BorderLayout <$> init c <*> init t <*> init b <*> init l <*> init r


renderLayout :: BorderLayout -> GL.GLfloat -> GL.GLfloat -> IO ()
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
