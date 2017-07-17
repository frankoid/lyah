import Data.Monoid
import Control.Monad

isBigGang :: Int -> (Bool, String)
isBigGang x = (x > 9, "Compared gang size to 9.")

isBigGangL :: Int -> (Bool, [String])
isBigGangL x = (x > 9, ["Compared gang size to 9."])

applyLog :: (Monoid m) => (a,m) -> (a -> (b,m)) -> (b,m)
applyLog (x, log) f =
  let (y, newLog) = f x
  in (y, log `mappend` newLog)

type Food = String
type Price = Sum Int

addDrink :: Food -> (Food,Price)
addDrink "beans" = ("milk", Sum 25)
addDrink "jerky" = ("whiskey", Sum 99)
addDrink _ = ("beer", Sum 30)

newtype Writer w a = Writer { runWriter :: (a, w) }

-- https://stackoverflow.com/questions/32929252/can-ghc-derive-functor-and-applicative-instances-for-a-monad-transformer
-- https://wiki.haskell.org/Functor-Applicative-Monad_Proposal#Missing_superclasses
instance (Monoid w) => Applicative (Writer w) where
  pure = return
  (<*>) = ap 

instance (Monoid w) => Functor (Writer w) where
  fmap = liftM
    
instance (Monoid w) => Monad (Writer w) where
  return x = Writer (x, mempty)
  (Writer (x,v)) >>= f =
    let (Writer (y, v')) = f x
    in Writer (y, v `mappend` v')
