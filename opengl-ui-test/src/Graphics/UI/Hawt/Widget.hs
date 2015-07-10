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

    Widget(..), render, preferredSize, notify, IsWidgetState(..),
    makeStateWidget, UIEvent(..), renderChild

) where

import Graphics.Rendering.OpenGL
import Control.Lens
import Control.Applicative

data UIEvent = MouseMoved GLfloat GLfloat


data Widget = Widget {  _render :: GLfloat -> GLfloat -> IO (),
                        _preferredSize :: (GLfloat, GLfloat),
                        _notify :: UIEvent -> Widget}
makeLenses ''Widget


class IsWidgetState a where
    renderState :: a -> GLfloat -> GLfloat -> IO ()
    prefStateSize :: a -> (GLfloat, GLfloat)
    notifyState :: a -> UIEvent -> a

makeStateWidget :: (IsWidgetState a) => a -> Widget
makeStateWidget state = Widget (renderInt state) (prefStateSize state) (notifyInt state)

renderInt :: (IsWidgetState a) => a -> GLfloat -> GLfloat -> IO ()
renderInt state width height = renderState state width height

notifyInt :: (IsWidgetState a) => a -> UIEvent -> Widget
notifyInt state event = makeStateWidget newState
    where
        newState = notifyState state event

renderChild :: Widget -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()
renderChild child x y width height = do
    preservingMatrix $ do
        translate $ Vector3 x y 0.0
        (child^.render) width height
