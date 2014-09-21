{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, GeneralizedNewtypeDeriving #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
module HInH.Types
(HTML ()
)where
import Control.Monad.Writer

import qualified Data.Map as M
newtype HTML a = H (Writer [TT] a) deriving(Functor,Monad)
data TT = Tag_ Tag | Text String
data Tag = Tag{name :: String, attr :: M.Map String String, inner :: HTML ()} 

class A a where __ :: a -> HTML ()
instance A String where __ = H . tell . (:[]) . Text
instance A TT where __ = H . tell . (:[])
instance A Tag where __ = H . tell . (:[]) . Tag_
instance A (HTML ()) where __ = id

makeTag :: (A t) => String -> t -> Tag
makeTag tagname inside = Tag{name = tagname, attr = M.empty, inner = __ inside}

p :: (A t) => t -> HTML ()
p = __ . makeTag "p"