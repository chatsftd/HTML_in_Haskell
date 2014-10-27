{-# OPTIONS -Wall #-}
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
,RawText(..)
)where
import Control.Monad.Writer
import HinH.NonEmpty
import Control.Applicative
import qualified Data.Map as M



data HTML a = H {dat :: a, unH :: Writer TTList ()}
newtype TTList = L {unL :: [TT]}
type Attr = M.Map String String
data TT = Tag_ Tag | ETag_ EmptyTag | STag_ ScriptTag | Text String | Raw RawText
data Tag = Tag{name :: String, attr :: Attr, inner :: HTML ()} 
data EmptyTag = ETag{nameE :: String, attrE :: Attr}
data ScriptTag = STag{nameS :: String, attrS :: Attr, innerS :: RawText}
newtype RawText = R {unR :: String}

data Attr2 = String := String deriving(Show,Eq,Ord)

instance Functor HTML where fmap = liftM
instance Applicative HTML where pure = return; (<*>) = ap
instance Monad HTML where
 return a = H a $ return ()
 H a m >>= f  =  H b (then2 m n) where H b n = f a
 
then2 :: Writer TTList () -> Writer TTList () -> Writer TTList ()
then2 m n = m >>= \{- strictness -}() -> n

{-
return a >>= f
H a (return ()) >>= f
H b (return () `then2` n) where H b n = f a
H b n where H b n = f a

H a m >>= return
H b $ m `then2` n where H b n = return a
H b $ m `then2` n where H b n = H a (return ())
H a (m `then2` return ()) 
H a (m >>= \() -> return ()) 
H a (m >>= return) 
H a m 

(H a m >>= \a -> H b n) >>= \b -> H c o
H b (then2 m n) >>= \b -> H c o
H c (then2 (then2 m n) o) 
H c ((m `then2` n) `then2` o) 
H c (m `then2` (n `then2` o))
H c (then m (then n o))
H a m >>=  \a -> H c (then n o)
H a m >>= (\a -> H b n  >>= \b -> H c o)
-} 
 
 
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
