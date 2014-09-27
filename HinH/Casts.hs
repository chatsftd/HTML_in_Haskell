{-# OPTIONS -Wall #-}
module HinH.Casts
(ToHTML(..)
-- ,B(..)
,FromTag(..)
,FromETag(..)
,FromSTag(..)
)where
import Control.Monad.Writer
import HinH.TypeDef
import HinH.StateHTML

largeLift :: TT -> HTML a
largeLift = H (error "cannot use <- or >>= to HTML tags") . H2 . tell . L . (:[]) 

class Void a where vvv :: [a] -> HTML b
instance Void Char where vvv = largeLift . Text

class ToHTML a where __ :: a -> HTML b
instance ToHTML (HTML a) where __ = H undefined .  unH 
instance ToHTML TT where __ = largeLift
instance ToHTML Tag where __ = largeLift . Tag_
instance ToHTML EmptyTag where __ = largeLift . ETag_
instance ToHTML ScriptTag where __ = largeLift . STag_
instance (Void a) => ToHTML [a] where __ = vvv

-- class B a where __TT :: a -> TT
-- instance B TT where __TT = id
-- instance B Tag where __TT = Tag_
-- instance B EmptyTag where __TT = ETag_
-- instance B ScriptTag where __TT = STag_
-- instance B String where __TT = Text

class FromTag a where __T :: Tag -> a
instance FromTag Tag where __T = id
instance FromTag TT where __T = Tag_
instance FromTag (HTML a) where __T = largeLift . Tag_
instance FromTag (StateHTML_ a b) where __T = smallLift . largeLift . Tag_

class FromETag a where __E :: EmptyTag -> a
instance FromETag EmptyTag where __E = id
instance FromETag TT where __E = ETag_
instance FromETag (HTML a) where __E = largeLift . ETag_
instance FromETag (StateHTML_ a b) where __E = smallLift . largeLift . ETag_

class FromSTag a where __S :: ScriptTag -> a
instance FromSTag ScriptTag where __S = id
instance FromSTag TT where __S = STag_
instance FromSTag (HTML a) where __S = largeLift . STag_
instance FromSTag (StateHTML_ a b) where __S = smallLift . largeLift . STag_

