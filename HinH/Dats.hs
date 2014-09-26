{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -pgmP cpp #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
module HInH.Dats
(p
,h1
,div
)where
import HInH.Types
import Prelude hiding(div) 

#define def(tagName) tagName :: (A a,T t) => a -> t;tagName = makeTag #tagName

def(p);
def(h1);
def(div);

{-
p :: (A a,T t) => a -> t;p = makeTag "p"
h1 :: (A a,T t) => a -> t;h1 = makeTag "h1"
div :: (A a,T t) => a -> t;div = makeTag "div"
-}