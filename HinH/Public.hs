{-# OPTIONS -Wall #-}
module HinH.Public
(StateHTML
,Attr2(..)
,(%)
,(%%)
,HTML()
,toStr
,empF,indF,parF
,evalS,execS,runS
,_PUT,_GET,_MOD
)where
import Prelude hiding(div)
import HinH.TypeDef
import HinH.StateHTML
import HinH.Print

infixl 9 %
(%) :: (HTML a -> b) -> HTML a -> b
(%) = id

infixl 9 %%
(%%) :: (HTML a -> t1 -> t) -> t1 -> HTML a -> t
(%%) = flip
