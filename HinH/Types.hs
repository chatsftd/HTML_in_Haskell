{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
module HInH.Types
(HTML
)where
import qualified Data.Map as M
data HTML = H [TT]
data TT = Tag_ Tag | Text String
data Tag = Tag{name :: String, attr :: M.Map String String, inner :: HTML} 


appendHTML :: HTML -> HTML -> HTML
appendHTML (H a) (H b) = H (a++b)

class A a where __ :: a -> HTML
instance A String where __ = H . (:[]) . Text
instance A TT where __ = H . (:[])
instance A Tag where __ = H . (:[]) . Tag_
 
instance A HTML where
 __ = id

makeTag :: (A t) => String -> t -> Tag
makeTag tagname inside = Tag{name = tagname, attr = M.empty, inner = __ inside}

p :: (A t) => t -> HTML
p = __ . makeTag "p"