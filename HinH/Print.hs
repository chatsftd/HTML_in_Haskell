{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
module HinH.Print
(printETag
,printSTag
,printTag
,printHTML
)where
import HinH.Types
import qualified Data.Map as M

printETag :: EmptyTag -> String
printETag ETag{nameE = n, attrE = a} = "<" ++ n ++ printAttr a ++ "/>"

printSTag :: ScriptTag -> String
printSTag STag{nameS = n, attrS = a, innerS = s} = "<" ++ n ++ printAttr a ++ ">" ++ s ++ "</" ++ n ++ ">"

printTag :: Tag -> String
printTag Tag{name = n, attr = a, inner = h} = "<" ++ n ++ printAttr a ++ ">" ++ printHTML h ++ "</" ++ n ++ ">"

printHTML :: HTML () -> String
printHTML h = concatMap f tts
 where 
  f(Tag_ t) = printTag t
  f(ETag_ t) = printETag t
  f(STag_ t) = printSTag t 
  f(Text t) = esc t
  tts = rawHTML h

printAttr :: Attr -> String
printAttr a 
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