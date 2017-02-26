import Data.Monoid

isBigGang :: Int -> (Bool, String)
isBigGang x = (x > 9, "Compared gang size to 9.")

applyLog :: (Monoid m) => (a,m) -> (a -> (b,m)) -> (b,m)
applyLog (x,log) f = let (y,newLog) = f x in (y,log `mappend` newLog)

type Food = String
type Price = Sum Int

addDrink :: Food -> (Food,Price)
addDrink "beans" = ("milk", Sum 25)
addDrink "jerky" = ("whiskey", Sum 99)
addDrink _ = ("beer", Sum 30)

newtype Writer w a = Writer { runWriter :: (a, w) }

-- http://stackoverflow.com/questions/32929252/can-ghc-derive-functor-and-applicative-instances-for-a-monad-transformer
instance (Monad m) => Applicative (MaybeT m) where
    pure = return
    (<*>) = ap

instance (Monad m) => Functor (MaybeT m) where
    fmap = liftM

instance (Monoid w) => Monad (Writer w) where
    return x = Writer (x, mempty)
    (Writer (x,v)) >>= f = let (Writer (y, v')) = f x in Writer (y, v `mappend` v')
