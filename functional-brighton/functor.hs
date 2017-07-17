class Functor f where
  fmap :: (a -> b) -> (f a -> f b)

instance Functor ((->) a) where
  fmap :: (a -> b) -> (((->) a) -> ((->) b))
  fmap f g = (\x -> f (g x))


  fmap :: (a -> b) -> (((->) a) -> ((->) b))
  fmap f g = (\x -> f (g x))
