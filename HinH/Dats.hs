{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -pgmP cpp #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
module HinH.Dats
(p
,h1
,div
,br
,img
)where
import HinH.Types
import HinH.Casts
import Prelude hiding(div) 

#define def(tagName) tagName :: (ToHTML a,FromTag t) => a -> t; tagName = makeTag #tagName
#define defE(eTagName) eTagName :: FromETag t => t; eTagName = makeETag #eTagName

def(p);
def(h1);
def(div);

defE(br);
defE(img);