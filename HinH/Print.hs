{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
module HinH.Print
(toStr
,empF
,indF
)where
import HinH.Types
import Prelude hiding((++))
import qualified Data.Map as M
import qualified Data.DList as D
import Data.DList(DList)
import Data.Monoid

data Format = Format{indentLevel :: Int} deriving(Show)
empF :: Format
empF = Format{indentLevel = -1}

indF :: Format
indF = Format{indentLevel = 0}


type Res = DList Char

class P c where pack :: c a -> DList a
instance P [] where pack = D.fromList
instance P DList where pack = id

infixr 5 ++

(++) :: (P a,P b) => a c -> b c -> DList c
a ++ b = pack a `mappend` pack b

formHead :: Format -> Res
formHead Format{indentLevel = a} = pack $ replicate a '\t'

formFoot :: Format -> Res
formFoot Format{indentLevel = a} 
 | a >= 0    = pack "\n"
 | otherwise = pack ""
 
up :: Format -> Format
up f@Format{indentLevel = a} 
 | a >= 0    = f{indentLevel = a+1}
 | otherwise = f 

printETag :: Format -> EmptyTag -> Res
printETag f ETag{nameE = n, attrE = a}  = formHead f ++ "<" ++ n ++ printAttr f a ++ "/>" ++ formFoot f

printSTag :: Format -> ScriptTag -> Res
printSTag f STag{nameS = n, attrS = a, innerS = s} = 
 formHead f ++ "<" ++ n ++ printAttr f a ++ ">" ++ formFoot f 
 ++ s ++ 
 formHead f ++ "</" ++ n ++ ">" ++ formFoot f

printTag :: Format -> Tag -> Res
printTag f Tag{name = n, attr = a, inner = h} = 
 formHead f ++ "<" ++ n ++ printAttr f a ++ ">" ++ formFoot f ++ 
 printHTML (up f) h ++ 
 formHead f ++ "</" ++ n ++ ">" ++ formFoot f

toStr :: Format -> HTML () -> String
toStr f ht = D.toList $ printHTML f ht
 
printHTML :: Format -> HTML () -> Res
printHTML fo h = mConcatMap f tts
 where 
  f(Tag_ t) = printTag fo t 
  f(ETag_ t) = printETag fo t 
  f(STag_ t) = printSTag fo t  
  f(Text t) = formHead fo ++ esc t ++ formFoot fo
  tts = rawHTML h

printAttr :: Format -> Attr -> Res
printAttr _ a  
 | M.null a  = pack ""
 | otherwise = " " ++ mConcatMap pAttr (M.toList a) ++ " "
 where
  pAttr (nam,str) = nam ++ "=\"" ++ esc str ++ "\""

esc :: String -> Res
esc = mConcatMap (pack . e)
 where
  e '<' = "&lt;"
  e '>' = "&gt;"
  e '"' = "&quot;"
  e '&' = "&amp;"
  e x   = [x]
  
mConcatMap :: Monoid c => (a -> c) -> [a] -> c  
mConcatMap = (mconcat .) . map