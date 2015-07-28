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
import Graphics.Rendering.OpenGL.Raw
import Control.Applicative
import Prelude hiding (init)
import Graphics.UI.Hawt.Drawing

data UIEvent = MouseMoved GLfloat GLfloat


data Widget =   Empty
                | Widget {  renderW :: GLfloat -> GLfloat -> RenderC,
                        prefSizeW :: (GLfloat, GLfloat),
                        notifyW :: UIEvent -> Widget,
                        initW :: IO Widget}
                | Container {  renderC :: [Widget] -> GLfloat -> GLfloat -> RenderC,
                        prefSizeC :: [Widget] -> (GLfloat, GLfloat),
                        notifyC :: UIEvent -> Widget,
                        initC :: IO Widget,
                        children :: [Widget]}

render :: Widget -> GLfloat -> GLfloat -> RenderC
render Empty _ _ = return ()
render Widget{renderW} w h = renderW w h
render Container{renderC, children} w h = renderC children w h

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
    renderState :: a -> GLfloat -> GLfloat -> RenderC
    prefStateSize :: a -> (GLfloat, GLfloat)
    notifyState :: a -> UIEvent -> a
    initState :: a -> IO a

class IsContainerState a where
    renderStateC :: a -> [Widget] -> GLfloat -> GLfloat -> RenderC
    prefStateSizeC :: a -> [Widget] -> (GLfloat, GLfloat)
    notifyStateC :: a -> UIEvent -> a
    initStateC :: a -> IO a

(+>) :: Widget -> Widget -> Widget
(+>) p@Widget{} child = p
(+>) p@Container{children=oldChildren} child = p { children = child : oldChildren }
(+>) Empty child = Empty

makeStateWidget :: (IsWidgetState a) => a -> Widget
makeStateWidget state = Widget (renderState state) (prefStateSize state) (notifyIntW state) (initIntW state)

makeStateContainer :: (IsContainerState a) => a -> [Widget] -> Widget
makeStateContainer state children = Container (renderStateC state) (prefStateSizeC state) (notifyIntC state children) (initIntC state children) children

notifyIntC :: (IsContainerState a) => a -> [Widget] -> UIEvent -> Widget
notifyIntC state children event = makeStateContainer newState children
    where
        newState = notifyStateC state event

initIntC :: (IsContainerState a) => a -> [Widget] -> IO Widget
initIntC state children = do
    newState <- initStateC state
    return $ makeStateContainer newState children


notifyIntW :: (IsWidgetState a) => a -> UIEvent -> Widget
notifyIntW state event = makeStateWidget newState
    where
        newState = notifyState state event

initIntW :: (IsWidgetState a) => a -> IO Widget
initIntW state = makeStateWidget <$> initState state

renderChild :: Widget -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> RenderC
renderChild child x y width height = do
        gl $ do
            glPushMatrix
            translate $ Vector3 x y 0.0
        render child width height
        gl glPopMatrix
