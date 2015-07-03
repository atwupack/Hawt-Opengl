-----------------------------------------------------------------------------
--
-- Module      :  Graphics.UI.Hawt.Widget
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
module Graphics.UI.Hawt.Widget (

    Widget(..), render, preferredSize

) where

import Graphics.Rendering.OpenGL
import Control.Lens

data Widget = Widget { _render :: GLfloat -> GLfloat -> IO Widget, _preferredSize :: (GLfloat, GLfloat)}
makeLenses ''Widget

