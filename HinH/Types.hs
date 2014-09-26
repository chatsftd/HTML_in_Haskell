{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving #-}
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
,T()
,(%)
)where
import Control.Monad.Writer
import HinH.NonEmpty
import Control.Applicative

import qualified Data.Map as M
newtype HTML a = H { unH :: H2 ()}
newtype H2 a = H2 (Writer TTList a)deriving(Functor,Applicative,Monad)

instance Functor HTML where
 fmap = liftM
 
instance Monad HTML  where
 return _ = H $ return ()
 (H m) >>= f = H $ m >> a where H a = f $ error "cannot not use <- or >>= in HTML"

instance Applicative HTML where
 pure = return 
 (<*>) = ap

newtype TTList = L { unL :: [TT] }
type Attr = M.Map String String
data TT = Tag_ Tag | ETag_ EmptyTag | STag_ ScriptTag | Text String 
data Tag = Tag{name :: String, attr :: Attr, inner :: HTML ()} 
data EmptyTag = ETag{nameE :: String, attrE :: Attr}
data ScriptTag = STag{nameS :: String, attrS :: Attr, innerS :: String}

class T a where __T :: Tag -> a
instance T Tag where __T = id
instance T TT where __T = Tag_
instance T (HTML a) where __T = H . H2 . tell . L . (:[]) . Tag_

rawHTML :: HTML a -> [TT]
rawHTML (H (H2 w)) = unL $ execWriter w

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

class A a where __ :: a -> HTML b
instance A (HTML a) where __ = H . unH 
instance A TT where __ = H  . H2 . tell . L . (:[])
instance A Tag where __ = H . H2 . tell . L . (:[]) . Tag_
instance A EmptyTag where __ = H . H2 . tell . L . (:[]) . ETag_
instance A ScriptTag where __ = H . H2 . tell . L . (:[]) . STag_
instance A String where __ = H . H2 . tell . L . (:[]) . Text

class B a where __TT :: a -> TT
instance B TT where __TT = id
instance B Tag where __TT = Tag_
instance B EmptyTag where __TT = ETag_
instance B ScriptTag where __TT = STag_
instance B String where __TT = Text

makeTag :: (A a, T t) => String -> a -> t
makeTag tagname inside = __T $ Tag{name = tagname, attr = M.empty, inner = __ inside}

modifyAttr :: (M.Map String String -> M.Map String String) -> Tag -> Tag
modifyAttr f t@Tag{attr = a} = t{attr = f a}

infixr 0 %
(%) :: (HTML a -> b) -> (HTML a -> b)
(%) = id