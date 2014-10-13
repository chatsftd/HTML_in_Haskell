{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -pgmP cpp #-}
{-# OPTIONS -Wall #-}
module HinH.Dats
(p
,h1
,div
,html
,head
,title
,body

,br
,img
,script
,doctypeHTML5
)where
import HinH.TypeDef
import HinH.Casts
import qualified Data.Map as M
import Prelude hiding(div,head) 


doctypeHTML5 :: HTML ()
doctypeHTML5 = __ . Raw . R $ "<!DOCTYPE html>"

makeTag :: (ToHTML a, FromTag t) => String -> a -> t
makeTag tagname inside = __T Tag{name = tagname, attr = M.empty, inner = __ inside}

makeETag :: FromETag t => String -> t
makeETag tagname = __E ETag{nameE = tagname, attrE = M.empty}

makeSTag :: (FromSTag t) => String -> RawText -> t
makeSTag tagname inside = __S STag{nameS = tagname, attrS = M.empty, innerS = inside}

#define def(tagName) tagName :: (ToHTML a,FromTag t) => a -> t; tagName = makeTag #tagName
#define defE(eTagName) eTagName :: FromETag t => t; eTagName = makeETag #eTagName
#define defS(sTagName) sTagName :: FromSTag t => RawText -> t; sTagName = makeSTag #sTagName

def(p);
def(h1);
def(div);
def(html);
def(head);
def(title);
def(body);

defE(br);
defE(img);

defS(script);
