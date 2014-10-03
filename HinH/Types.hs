{-# OPTIONS -Wall #-}
module HinH.Types
(HTML()
,Attr2(..)
,ToHTML(..)
,makeTag
,makeETag
,TT(..)
,Tag(..)
,EmptyTag(..)
,ScriptTag(..)
,Attr
,rawHTML
,FromTag()
,(%)
,(%%)
)where
import Control.Monad.Writer
import HinH.TypeDef
import HinH.Casts
import qualified Data.Map as M

rawHTML :: HTML a -> [TT]
rawHTML (H _ (H2 w)) = unL $ execWriter w

makeTag :: (ToHTML a, FromTag t) => String -> a -> t
makeTag tagname inside = __T Tag{name = tagname, attr = M.empty, inner = __ inside}

makeETag :: FromETag t => String -> t
makeETag tagname = __E ETag{nameE = tagname, attrE = M.empty}


infixr 0 %
(%) :: (HTML a -> b) -> HTML a -> b
(%) = id


infixl 9 %%
(%%) :: (HTML a -> t1 -> t) -> t1 -> HTML a -> t
(%%) a b c = (a % c) b
