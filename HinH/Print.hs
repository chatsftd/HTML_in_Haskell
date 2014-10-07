{-# OPTIONS -Wall #-}
module HinH.Print
(toStr
,empF
,indF
,parF
)where
import HinH.TypeDef
import Prelude hiding((++))
import qualified Data.Map as M
import qualified Data.DList as D
import Data.DList(DList)
import Data.Monoid
import Control.Applicative

data Format = Format{indentLevel :: Maybe Int, fully :: Bool} deriving(Show)
empF :: Format
empF = Format{indentLevel = Nothing, fully = False}

indF :: Format
indF = Format{indentLevel = Just 0, fully = True}

parF :: Format
parF = Format{indentLevel = Just 0, fully = False}

type Txt = DList Char

class P c where pack :: c a -> DList a
instance P [] where pack = D.fromList
instance P DList where pack = id

infixr 5 ++

(++) :: (P a,P b) => a c -> b c -> DList c
a ++ b = pack a `mappend` pack b

formHead :: Format -> Txt
formHead Format{indentLevel = Nothing} = pack   ""
formHead Format{indentLevel = Just n } = pack $ replicate n '\t'


formFoot :: Format -> Txt
formFoot Format{indentLevel = Nothing} = pack ""
formFoot Format{indentLevel = Just _ } = pack "\n"

 
up :: Format -> Format
up f@Format{indentLevel = a} = f{indentLevel = (+1) <$> a}


printETag :: Format -> EmptyTag -> Txt
printETag f ETag{nameE = n, attrE = a}  = formHead f ++ "<" ++ n ++ printAttr f a ++ "/>" ++ formFoot f

printSTag :: Format -> ScriptTag -> Txt
printSTag f STag{nameS = n, attrS = a, innerS = s} = 
 formHead f ++ "<" ++ n ++ printAttr f a ++ ">" ++ formFoot f 
 ++ s ++ 
 formHead f ++ "</" ++ n ++ ">" ++ formFoot f

printTag :: Format -> Tag -> Txt
printTag f Tag{name = n, attr = a, inner = h} = 
 formHead f ++ "<" ++ n ++ printAttr f a ++ ">" ++ printHTML2 f h ++ "</" ++ n ++ ">" ++ formFoot f

toStr :: Format -> HTML () -> String
toStr f ht = D.toList $ printHTML f ht
 
printHTML2 :: Format -> HTML () -> Txt
printHTML2 f h = case (rawHTML h,fully f) of
 ([Text t],False) 
  | length t < 60 -> pack $ esc t
 _                -> formFoot f ++ printHTML (up f) h ++ formHead f 
 
printHTML :: Format -> HTML () -> Txt
printHTML fo h = mConcatMap (printInside fo) $ rawHTML h

printInside :: Format -> TT -> Txt
printInside fo (Tag_ t) = printTag fo t 
printInside fo (ETag_ t) = printETag fo t 
printInside fo (STag_ t) = printSTag fo t 
printInside fo (Text t) = formHead fo ++ esc t ++ formFoot fo


printAttr :: Format -> Attr -> Txt
printAttr _ a  
 | M.null a  = pack ""
 | otherwise = " " ++ mConcatMap pAttr (M.toList a) 
 where
  pAttr (nam,str) = nam ++ "=\"" ++ esc str ++ "\""

esc :: String -> Txt
esc = mConcatMap (pack . e)
 where
  e '<' = "&lt;"
  e '>' = "&gt;"
  e '"' = "&quot;"
  e '&' = "&amp;"
  e x   = [x]
  
mConcatMap :: Monoid c => (a -> c) -> [a] -> c  
mConcatMap = (mconcat .) . map