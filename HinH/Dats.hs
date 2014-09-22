{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
module HInH.Dats
(p
)where
import HInH.Types

p :: (A t) => t -> Tag
p = makeTag "p"
