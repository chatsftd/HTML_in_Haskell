{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
module HinH.Print
(toStr
,empF
,indF
)where
import HinH.Types
import qualified Data.Map as M

data Format = Format{indentLevel :: Int} deriving(Show)
empF :: Format
empF = Format{indentLevel = -1}

indF :: Format
indF = Format{indentLevel = 0}

formHead :: Format -> String
formHead Format{indentLevel = a} = replicate a '\t'

formFoot :: Format -> String
formFoot Format{indentLevel = a} 
 | a >= 0    = "\n"
 | otherwise = ""
 
up :: Format -> Format
up f@Format{indentLevel = a} 
 | a >= 0    = f{indentLevel = a+1}
 | otherwise = f 

printETag :: Format -> EmptyTag -> String
printETag f ETag{nameE = n, attrE = a}  = formHead f ++ "<" ++ n ++ printAttr f a ++ "/>" ++ formFoot f

printSTag :: Format -> ScriptTag -> String
printSTag f STag{nameS = n, attrS = a, innerS = s} = 
 formHead f ++ "<" ++ n ++ printAttr f a ++ ">" ++ formFoot f 
 ++ s ++ 
 formHead f ++ "</" ++ n ++ ">" ++ formFoot f

printTag :: Format -> Tag -> String
printTag f Tag{name = n, attr = a, inner = h} = 
 formHead f ++ "<" ++ n ++ printAttr f a ++ ">" ++ formFoot f ++ 
 printHTML (up f) h ++ 
 formHead f ++ "</" ++ n ++ ">" ++ formFoot f

toStr :: Format -> HTML () -> String
toStr = printHTML 
 
printHTML :: Format -> HTML () -> String
printHTML fo h = concatMap f tts
 where 
  f(Tag_ t) = printTag fo t 
  f(ETag_ t) = printETag fo t 
  f(STag_ t) = printSTag fo t  
  f(Text t) = formHead fo ++ esc t ++ formFoot fo
  tts = rawHTML h

printAttr :: Format -> Attr -> String
printAttr _ a  
 | M.null a  = ""
 | otherwise = " " ++ concatMap pAttr (M.toList a) ++ " "
 where
  pAttr (nam,str) = nam ++ "=\"" ++ esc str ++ "\""

esc :: String -> String
esc = concatMap e
 where
  e '<' = "&lt;"
  e '>' = "&gt;"
  e '"' = "&quot;"
  e '&' = "&amp;"
  e x   = [x]