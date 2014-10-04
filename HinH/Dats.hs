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
import HinH.TypeDef
import HinH.Casts
import qualified Data.Map as M
import Prelude hiding(div) 


makeTag :: (ToHTML a, FromTag t) => String -> a -> t
makeTag tagname inside = __T Tag{name = tagname, attr = M.empty, inner = __ inside}

makeETag :: FromETag t => String -> t
makeETag tagname = __E ETag{nameE = tagname, attrE = M.empty}

#define def(tagName) tagName :: (ToHTML a,FromTag t) => a -> t; tagName = makeTag #tagName
#define defE(eTagName) eTagName :: FromETag t => t; eTagName = makeETag #eTagName

def(p);
def(h1);
def(div);

defE(br);
defE(img);