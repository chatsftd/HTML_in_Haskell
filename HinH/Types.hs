{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, GeneralizedNewtypeDeriving #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
module HInH.Types
(HTML()
,modifyAttr
,A(..),B(..)
,makeTag
,TT(..)
,Tag(..)
,EmptyTag(..)
,ScriptTag(..)
,Attr
,rawHTML
)where
import Control.Monad.Writer
import HinH.NonEmpty

import qualified Data.Map as M
newtype HTML a = H (Writer TTList a) deriving(Functor,Monad)
newtype TTList = L { unL :: [TT] }
type Attr = M.Map String String
data TT = Tag_ Tag | ETag_ EmptyTag | STag_ ScriptTag | Text String 
data Tag = Tag{name :: String, attr :: Attr, inner :: HTML ()} 
data EmptyTag = ETag{nameE :: String, attrE :: Attr}
data ScriptTag = STag{nameS :: String, attrS :: Attr, innerS :: String}

rawHTML :: HTML a -> [TT]
rawHTML (H w) = unL $ execWriter w

instance Monoid TTList where
 mempty = L []
 L[] `mappend` a = a
 a `mappend` L[] = a
 L(x:xs) `mappend` L(y:ys) = L $ add x xs y ys

add :: TT -> [TT] -> TT -> [TT] -> [TT]
add x xs y ys = case (last' xs2,y) of
 (Text a,Text b) -> init' xs2 ++ Text(a ++ b) : ys
 _ -> x:xs++y:ys
 where xs2 = x :| xs 

class A a where __ :: a -> HTML ()
instance A (HTML ()) where __ = id
instance A TT where __ = H . tell . L . (:[])
instance A Tag where __ = H . tell . L . (:[]) . Tag_
instance A EmptyTag where __ = H . tell . L . (:[]) . ETag_
instance A ScriptTag where __ = H . tell . L . (:[]) . STag_
instance A String where __ = H . tell . L . (:[]) . Text

class B a where __TT :: a -> TT
instance B TT where __TT = id
instance B Tag where __TT = Tag_
instance B EmptyTag where __TT = ETag_
instance B ScriptTag where __TT = STag_
instance B String where __TT = Text

makeTag :: (A t) => String -> t -> Tag
makeTag tagname inside = Tag{name = tagname, attr = M.empty, inner = __ inside}

modifyAttr :: (M.Map String String -> M.Map String String) -> Tag -> Tag
modifyAttr f t@Tag{attr = a} = t{attr = f a}
