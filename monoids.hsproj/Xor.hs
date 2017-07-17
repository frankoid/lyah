module Xor where
  
-- a new instance of monoid for Xor on booleans

--import Data.Boolean
import Data.Monoid

{-
newtype Any = Any { getAny :: Bool }
        deriving (Eq, Ord, Read, Show, Bounded)
-}

newtype Xor = Xor { getXor :: Bool }
        deriving (Eq, Ord, Read, Show, Bounded)


{-
instance Monoid Any where
        mempty = Any False
        Any x `mappend` Any y = Any (x || y)
-}

xor :: Bool -> Bool -> Bool
xor True a = not a
xor False a = a

instance Monoid Xor where
         mempty = Xor False -- really?
         Xor x `mappend` Xor y = Xor $ x `xor` y
  