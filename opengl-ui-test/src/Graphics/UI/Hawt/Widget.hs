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
{-# LANGUAGE NamedFieldPuns #-}
module Graphics.UI.Hawt.Widget (

    Widget, IsWidgetState(..), IsContainerState(..),
    makeStateWidget, makeStateContainer, UIEvent(..), renderChild, (+>), render,
    init, prefSize, notify

) where

import Graphics.Rendering.OpenGL
import Control.Applicative
import Prelude hiding (init)

data UIEvent = MouseMoved GLfloat GLfloat


data Widget =   Empty
                | Widget {  renderW :: GLfloat -> GLfloat -> IO (),
                        prefSizeW :: (GLfloat, GLfloat),
                        notifyW :: UIEvent -> Widget,
                        initW :: IO Widget}
                | Container {  renderC ::  [Widget] -> GLfloat -> GLfloat -> IO (),
                        prefSizeC :: [Widget] -> (GLfloat, GLfloat),
                        notifyC :: UIEvent -> Widget,
                        initC :: IO Widget,
                        children :: [Widget]}

render :: Widget -> GLfloat -> GLfloat -> IO()
render Widget{renderW} w h = renderW w h
render Container{renderC, children} w h = renderC children w h
render Empty w h = return ()

init :: Widget -> IO Widget
init Widget{initW} = initW
init c@Container{children} = do
    newChildren <- mapM init children
    newCont <- initC c
    return newCont {children = newChildren}
init Empty = return Empty

prefSize :: Widget -> (GLfloat, GLfloat)
prefSize Widget{prefSizeW} = prefSizeW
prefSize Container{prefSizeC, children} = prefSizeC children
prefSize Empty = (0,0)

notify :: Widget -> UIEvent -> Widget
notify Widget{notifyW} event = notifyW event
notify Container{notifyC} event = notifyC event
notify Empty event = Empty

class IsWidgetState a where
    renderState :: a -> GLfloat -> GLfloat -> IO ()
    prefStateSize :: a -> (GLfloat, GLfloat)
    notifyState :: a -> UIEvent -> a
    initState :: a -> IO a

class IsContainerState a where
    renderStateC :: a -> [Widget] -> GLfloat -> GLfloat -> IO ()
    prefStateSizeC :: a -> [Widget] -> (GLfloat, GLfloat)
    notifyStateC :: a -> UIEvent -> a
    initStateC :: a -> IO a

(+>) :: Widget -> Widget -> Widget
(+>) p@Widget{} child = p
(+>) p@Container{children=oldChildren} child = p { children = child : oldChildren }
(+>) Empty child = Empty

makeStateWidget :: (IsWidgetState a) => a -> Widget
makeStateWidget state = Widget (renderState state) (prefStateSize state) (notifyInt state) (initInt state)

makeStateContainer :: (IsContainerState a) => a -> Widget
makeStateContainer state = Container (renderStateC state) (prefStateSizeC state) (notifyIntC state) (initIntC state) []

notifyIntC :: (IsContainerState a) => a -> UIEvent -> Widget
notifyIntC state event = makeStateContainer newState
    where
        newState = notifyStateC state event

initIntC :: (IsContainerState a) => a -> IO Widget
initIntC state = makeStateContainer <$> initStateC state


notifyInt :: (IsWidgetState a) => a -> UIEvent -> Widget
notifyInt state event = makeStateWidget newState
    where
        newState = notifyState state event

initInt :: (IsWidgetState a) => a -> IO Widget
initInt state = makeStateWidget <$> initState state

renderChild :: Widget -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()
renderChild child x y width height = preservingMatrix $ do
    translate $ Vector3 x y 0.0
    render child width height
