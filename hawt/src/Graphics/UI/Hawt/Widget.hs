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
{-# LANGUAGE NamedFieldPuns, TypeFamilies #-}
module Graphics.UI.Hawt.Widget (

    Widget, IsWidgetState(..), IsContainerState(..),
    makeStateWidget, UIEvent(..), renderChild, render,
    init, prefSize, notify, widget, UI(..), emptyWidget, (+>)

) where

import Graphics.Rendering.OpenGL
import Control.Applicative
import Prelude hiding (init)
import Graphics.UI.Hawt.Drawing
import Control.Monad.Trans.Reader
import Graphics.UI.Hawt

data UI a = UI a Widget

widget :: (IsWidgetState a) => a -> UI a
widget state = UI state $ makeStateWidget state

emptyWidget :: Widget
emptyWidget = Widget {render = renderE , prefSize=(0,0), notify=notifyE, init=return emptyWidget}
    where
        renderE w h = return ()
        notifyE e = emptyWidget

data UIEvent = MouseMoved GLfloat GLfloat


data Widget =   Widget { render :: GLfloat -> GLfloat -> RenderC,
                        prefSize :: (GLfloat, GLfloat),
                        notify :: UIEvent -> Widget,
                        init :: IO Widget}

class IsWidgetState a where
    renderState :: a -> GLfloat -> GLfloat -> RenderC
    prefStateSize :: a -> (GLfloat, GLfloat)
    notifyState :: a -> UIEvent -> a
    initState :: a -> IO a

class (IsWidgetState a) => IsContainerState a where
    addChild :: a -> ChildType a -> Widget -> a
    type ChildType a

(+>) :: (IsContainerState a, IsWidgetState b) => UI a -> (ChildType a, UI b) -> UI a
(+>) (UI p pw) (ct, (UI c cw)) = widget newParent
    where
        newParent = addChild p ct cw

makeStateWidget :: (IsWidgetState a) => a -> Widget
makeStateWidget state = Widget (renderState state) (prefStateSize state) (notifyIntW state) (initIntW state)

notifyIntW :: (IsWidgetState a) => a -> UIEvent -> Widget
notifyIntW state event = makeStateWidget newState
    where
        newState = notifyState state event

initIntW :: (IsWidgetState a) => a -> IO Widget
initIntW state = makeStateWidget <$> initState state

renderChild :: Widget -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> RenderC
renderChild child x y width height = do
    c <- ask
    gl $ preservingMatrix $ do
        translate $ Vector3 x y 0.0
        runReaderT (render child width height) c
