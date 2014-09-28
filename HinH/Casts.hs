{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
module HinH.Casts
(A(..),B(..),T(..)
)where
import Control.Monad.Writer
import HinH.TypeDef
import HinH.StateHTML

largeLift :: TT -> HTML a
largeLift = H (error "cannot use <- or >>= to HTML tags") . H2 . tell . L . (:[]) 

class A a where __ :: a -> HTML b
instance A (HTML a) where __ = H undefined .  unH 
instance A TT where __ = largeLift
instance A Tag where __ = largeLift . Tag_
instance A EmptyTag where __ = largeLift . ETag_
instance A ScriptTag where __ = largeLift . STag_
instance A String where __ = largeLift . Text

class B a where __TT :: a -> TT
instance B TT where __TT = id
instance B Tag where __TT = Tag_
instance B EmptyTag where __TT = ETag_
instance B ScriptTag where __TT = STag_
instance B String where __TT = Text

class T a where __T :: Tag -> a
instance T Tag where __T = id
instance T TT where __T = Tag_
instance T (HTML a) where __T = largeLift . Tag_
instance T (StateHTML_ a b) where __T = smallLift . largeLift . Tag_