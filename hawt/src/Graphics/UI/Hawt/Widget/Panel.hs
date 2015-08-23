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
{-# LANGUAGE NamedFieldPuns, TypeFamilies #-}
module Graphics.UI.Hawt.Widget.Panel (
    panel, PanelContent(..)
) where

import qualified Graphics.Rendering.OpenGL as GL

import Graphics.UI.Hawt
import Graphics.UI.Hawt.Widget
import Graphics.UI.Hawt.Drawing
import Prelude hiding (init)
import Control.Arrow

data Panel = Panel { color :: GL.Color4 GL.GLfloat, content :: Widget}
data PanelContent = Content

instance IsWidgetState Panel where
    -- Nothing to initialize
    initState Panel{color, content} = do
        c <- init content
        return $ Panel color c
    renderState = renderPanel
    -- Preferred size is max of all children.
    prefStateSize Panel{content} = prefSize content
    -- No state change on event.
    notifyState p event = p

instance IsContainerState Panel where
    addChild p Content child = p{content=child}
    type ChildType Panel = PanelContent

-- | Create a panel with the given color and w/o content.
panel :: GL.Color4 GL.GLfloat -> UI Panel
panel color = widget $ Panel color emptyWidget


renderPanel ::  Panel -> GL.GLfloat -> GL.GLfloat -> RenderC
renderPanel p@(Panel color content) width height = do
    renderBox color 0 0 width height
    renderChild content 0.0 0.0 width height
