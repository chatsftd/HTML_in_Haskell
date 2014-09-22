{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
module HInH.Print
(printETag
,printSTag
,printTag
,printHTML
)where
import HInH.Types
import qualified Data.Map as M
{-
import qualified Data.Map as M
newtype HTML a = H (Writer TTList a) deriving(Functor,Monad)
newtype TTList = L ([TT]) 
data TT = Tag_ Tag | Text String
data Tag = Tag{name :: String, attr :: M.Map String String, inner :: HTML ()} 
-}

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
 | otherwise = " " ++ concatMap nazo (M.toList a) ++ " "

nazo :: (String,String) -> String
nazo (nam,str) = nam ++ "=\"" ++ esc str ++ "\""

esc :: String -> String
esc = concatMap e
 where
  e '<' = "&lt;"
  e '>' = "&gt;"
  e '"' = "&quot;"
  e '&' = "&amp;"
  e x   = [x]