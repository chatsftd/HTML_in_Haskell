{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
module HinH.TypeDef
(HTML(..)
,TTList(..)
,TT(..)
,Tag(..)
,EmptyTag(..)
,ScriptTag(..)
,Attr
,Attr2(..)
,rawHTML
)where
import Control.Monad.Writer
import HinH.NonEmpty
import Control.Applicative
import qualified Data.Map as M



data HTML a = H {dat :: a, unH :: Writer TTList ()}
newtype TTList = L {unL :: [TT]}
type Attr = M.Map String String
data TT = Tag_ Tag | ETag_ EmptyTag | STag_ ScriptTag | Text String 
data Tag = Tag{name :: String, attr :: Attr, inner :: HTML ()} 
data EmptyTag = ETag{nameE :: String, attrE :: Attr}
data ScriptTag = STag{nameS :: String, attrS :: Attr, innerS :: String}

data Attr2 = String := String deriving(Show,Eq,Ord)

instance Functor HTML where fmap = liftM
instance Applicative HTML where pure = return; (<*>) = ap
instance Monad HTML where
 return a = H a $ return ()
 H{dat = a, unH = m} >>= f = H b $ m >> n where H b n = f a
 
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

rawHTML :: HTML a -> [TT]
rawHTML (H _ w) = unL $ execWriter w
