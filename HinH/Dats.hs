{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
module HInH.Dats
(p
,h1
,div
)where
import HInH.Types
import Prelude hiding(div) 

p :: (A t) => t -> Tag;p = makeTag "p"
h1 :: (A t) => t -> Tag;h1 = makeTag "h1"
div :: (A t) => t -> Tag;div = makeTag "div"
