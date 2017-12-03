import Control.Applicative
import Control.Monad.Trans
import Control.Monad

newtype MaybeT m a = MaybeT {
  runMaybeT :: m (Maybe a)
  }

bindMT :: (Monad m) => MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
x `bindMT` f = MaybeT $ do
  unwrapped <- runMaybeT x
  case unwrapped of
    Nothing -> return Nothing
    Just y -> runMaybeT (f y)

instance (Monad m) => Functor (MaybeT m) where
  fmap f (MaybeT m) = MaybeT $ do
    unwrapped <- m
    case unwrapped of
      Nothing -> return Nothing
      Just y -> return (Just (f y))

instance (Monad m) => Applicative (MaybeT m) where
  pure = MaybeT . return . Just
  x <*> y = MaybeT $ do
    unwrappedFunc <- runMaybeT x
    unwrappedVal <- runMaybeT y
    return (unwrappedFunc <*> unwrappedVal)
      
instance (Monad m) => Monad (MaybeT m) where
  return a = MaybeT $ return (Just a)
  (>>=) = bindMT

instance MonadTrans MaybeT where
  lift m = MaybeT (liftM Just m)
