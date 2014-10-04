{-# OPTIONS -Wall #-}
module HinH.NonEmpty
(NonEmpty(..)
,cons
,snoc
,snoc2
,append
,last'
,nE
,init'
)where
import qualified Data.Traversable as T
import qualified Data.Foldable as T
import Control.Monad
import Control.Applicative

data NonEmpty a = (:|)a [a] deriving(Show,Eq,Ord)
 
instance T.Traversable NonEmpty where
 traverse up (x :| []    ) = (:|[]) <$> up x
 traverse up (x :| (y:ys)) = cons <$> up x <*> T.traverse up (y :| ys)

instance T.Foldable NonEmpty where
 foldMap = T.foldMapDefault 

instance Functor NonEmpty where
 fmap f (y :| ys) = f y :| map f ys

instance Applicative NonEmpty where
 pure = return
 (<*>) = ap

instance Monad NonEmpty where
 return = nE
 m >>= f = concat'(fmap f m)
 
infixr 5 :|
infixr 5 `cons`
infixr 5 `append`
infixl 5 `snoc`
infixl 5 `snoc2`

cons :: a -> NonEmpty a -> NonEmpty a
{-# INLINE cons #-}
x `cons` (y :| ys) = x :| y : ys

snoc :: NonEmpty a -> a -> NonEmpty a
(y :| ys) `snoc` x = y :| (ys ++ [x])

snoc2 :: [a] -> a -> NonEmpty a
[] `snoc2` a = a :| []
(x:xs) `snoc2` a = x :| (xs ++ [a])

append :: NonEmpty a -> NonEmpty a -> NonEmpty a
(x:|[]     ) `append` ys = x `cons` ys
(x:|(x2:xs)) `append` ys = x `cons` ((x2:|xs) `append` ys)

nE :: a -> NonEmpty a
{-# INLINE nE #-}
nE = (:|[])

last' :: NonEmpty a -> a
last' (x :| [])     = x
last' (_ :| (y:ys)) = last' (y :| ys)

init' :: NonEmpty a -> [a]
init' (_ :| [])     = []
init' (x :| (y:ys)) = x : init'(y:|ys)
 
concat' :: NonEmpty(NonEmpty a) -> NonEmpty a
concat' (xs:|[]) = xs 
concat' (xs:|(xs2:xss)) = xs `append` concat' (xs2:|xss)
