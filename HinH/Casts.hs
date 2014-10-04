{-# OPTIONS -Wall #-}
module HinH.Casts
(ToHTML(..)
,Attr2(..)
,FromTag(..)
,FromETag(..)
,FromSTag(..)
)where
import Control.Monad.Writer(tell)
import HinH.TypeDef
import HinH.StateHTML
import qualified Data.Map as M

largeLift :: TT -> HTML a
largeLift = H (error "cannot use <- or >>= to HTML tags") . tell . L . (:[])

class Void a where vvv :: [a] -> HTML b
instance Void Char where vvv = largeLift . Text

class ToHTML a where __ :: a -> HTML b
instance ToHTML (HTML a) where __ = H undefined .  unH 
instance ToHTML TT where __ = largeLift
instance ToHTML Tag where __ = largeLift . Tag_
instance ToHTML EmptyTag where __ = largeLift . ETag_
instance ToHTML ScriptTag where __ = largeLift . STag_
instance (Void a) => ToHTML [a] where __ = vvv

class ModifyAttr a where modifyAttr :: (M.Map String String -> M.Map String String) -> a -> a
instance ModifyAttr Tag where modifyAttr f t@Tag{attr = a} = t{attr = f a}
instance ModifyAttr EmptyTag where modifyAttr f t@ETag{attrE = a} = t{attrE = f a}
instance ModifyAttr ScriptTag where modifyAttr f t@STag{attrS = a} = t{attrS = f a}

class FromTag a where __T :: Tag -> a
instance FromTag Tag where __T = id
instance FromTag TT where __T = Tag_
instance FromTag (HTML a) where __T = largeLift . Tag_
instance FromTag (StateHTML_ a b) where __T = smallLift . largeLift . Tag_
instance (Modifier c,FromTag a) => FromTag (c -> a) where __T et c =  __T $ modif c et

class FromETag a where __E :: EmptyTag -> a
instance FromETag EmptyTag where __E = id
instance FromETag TT where __E = ETag_
instance FromETag (HTML a) where __E = largeLift . ETag_
instance FromETag (StateHTML_ a b) where __E = smallLift . largeLift . ETag_
instance (Modifier c,FromETag a) => FromETag (c -> a) where __E et c =  __E $ modif c et

class FromSTag a where __S :: ScriptTag -> a
instance FromSTag ScriptTag where __S = id
instance FromSTag TT where __S = STag_
instance FromSTag (HTML a) where __S = largeLift . STag_
instance FromSTag (StateHTML_ a b) where __S = smallLift . largeLift . STag_
instance (Modifier c,FromSTag a) => FromSTag (c -> a) where __S et c =  __S $ modif c et

class Modifier c where modif :: (Modifier c,ModifyAttr a) => c -> a -> a
instance Modifier Attr2 where modif (a := b) = modifyAttr $ M.insert a b 
instance (Modifier c) => Modifier [c] where modif ms a = foldr modif a ms
