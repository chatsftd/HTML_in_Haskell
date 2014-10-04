{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
module HinH.StateHTML
(StateHTML
,StateHTML_
,smallLift
,_PUT,_GET,_MOD
,runS,evalS,execS
)where
import Control.Monad.State
import HinH.TypeDef
import Control.Applicative

newtype StateHTML_ a b = S{unS :: StateT a HTML b} deriving(Functor,Applicative,Monad)
type StateHTML a = StateHTML_ a ()

smallLift :: HTML b -> StateHTML_ a b
smallLift = S . lift

_PUT :: a -> StateHTML_ a ()
_PUT = S . put

_GET :: StateHTML_ a a
_GET = S get 

_MOD :: (t -> t) -> StateHTML_ t ()
_MOD f = do
 x <- _GET
 _PUT $ f x

runS :: StateHTML a -> a -> HTML ((),a)
runS = runStateT . unS

evalS :: StateHTML a -> a -> HTML ()
evalS m = fmap fst . runS m

execS :: StateHTML a -> a -> HTML a
execS m = fmap snd . runS m
