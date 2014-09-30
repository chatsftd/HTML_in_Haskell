{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
module HinH.Types
(HTML()
,modifyAttr
,ToHTML(..)
-- ,B(..)
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

modifyAttr :: (M.Map String String -> M.Map String String) -> Tag -> Tag
modifyAttr f t@Tag{attr = a} = t{attr = f a}

infixr 0 %
(%) :: (HTML a -> b) -> HTML a -> b
(%) = id
