{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, GeneralizedNewtypeDeriving #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
module HInH.Types
(HTML ()
,p
,modifyAttr
)where
import Control.Monad.Writer

import qualified Data.Map as M
newtype HTML a = H (Writer [TT] a) deriving(Functor,Monad)
data TT = Tag_ Tag | Text String
data Tag = Tag{name :: String, attr :: M.Map String String, inner :: HTML ()} 

class A a where __ :: a -> HTML ()
instance A (HTML ()) where __ = id
instance A TT where __ = H . tell . (:[])
instance A Tag where __ = H . tell . (:[]) . Tag_
instance A String where __ = H . tell . (:[]) . Text

class B a where __TT :: a -> TT
instance B TT where __TT = id
instance B Tag where __TT = Tag_
instance B String where __TT = Text

makeTag :: (A t) => String -> t -> Tag
makeTag tagname inside = Tag{name = tagname, attr = M.empty, inner = __ inside}

p :: (A t) => t -> Tag
p = makeTag "p"

modifyAttr :: (M.Map String String -> M.Map String String) -> Tag -> Tag
modifyAttr f t@Tag{attr = a} = t{attr = f a}
