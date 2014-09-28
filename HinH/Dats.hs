{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -pgmP cpp #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
module HinH.Dats
(p
,h1
,div
)where
import HinH.Types
import Prelude hiding(div) 

#define def(tagName) tagName :: (ToHTML a,FromTag t) => a -> t;tagName = makeTag #tagName

def(p);
def(h1);
def(div);
