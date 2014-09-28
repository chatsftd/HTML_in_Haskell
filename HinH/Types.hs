{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
module HinH.Types
(HTML()
,modifyAttr
,A(..),B(..)
,makeTag
,TT(..)
,Tag(..)
,EmptyTag(..)
,ScriptTag(..)
,Attr
,rawHTML
,T()
,(%)
)where
import Control.Monad.Writer
import HinH.TypeDef
import HinH.Casts
import qualified Data.Map as M

rawHTML :: HTML a -> [TT]
rawHTML (H _ (H2 w)) = unL $ execWriter w

makeTag :: (A a, T t) => String -> a -> t
makeTag tagname inside = __T $ Tag{name = tagname, attr = M.empty, inner = __ inside}

modifyAttr :: (M.Map String String -> M.Map String String) -> Tag -> Tag
modifyAttr f t@Tag{attr = a} = t{attr = f a}

infixr 0 %
(%) :: (HTML a -> b) -> (HTML a -> b)
(%) = id