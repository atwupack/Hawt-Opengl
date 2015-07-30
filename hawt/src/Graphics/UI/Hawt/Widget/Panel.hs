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
    panel
) where

import qualified Graphics.Rendering.OpenGL as GL

import Graphics.UI.Hawt.Widget
import Graphics.UI.Hawt.Drawing
import Prelude hiding (init)
import Control.Arrow

data Panel = Panel { color :: GL.Color4 GL.GLfloat}

instance IsContainerState Panel where
    -- Nothing to initialize
    initStateC = return
    renderStateC = renderPanel
    -- Preferred size is max of all children.
    prefStateSizeC p = foldl cmax (0,0)
        where
            cmax (w,h) current = (max w *** max h)(prefSize current)
    -- No state change on event.
    notifyStateC p event = p

-- | Create a panel with the given color.
panel :: GL.Color4 GL.GLfloat -> Widget
panel color = makeStateContainer (Panel color) []


renderPanel ::  Panel -> [Widget] -> GL.GLfloat -> GL.GLfloat -> RenderC
-- Render an empty panel with its color.
renderPanel p@(Panel color) [] width height = renderBox color 0 0 width height
-- Render all the children.
renderPanel p@(Panel color) children width height = do
    renderPanel p [] width height
    mapM_ (\c -> renderChild c 0.0 0.0 width height) children

