import Control.Monad
import Control.Applicative
import Control.Monad.Trans

newtype EitherT e m a = EitherT {
    runEitherT :: m (Either e a)
    }

bindET :: (Monad m) => EitherT e m a -> (a -> EitherT e m b) -> EitherT e m b
et `bindET` f = EitherT $ do
  unwrapped <- runEitherT et
  case unwrapped of
    Left err -> return (Left err)
    Right val -> runEitherT (f val)

instance (Monad m) => Monad (EitherT e m) where
  return a = EitherT $ return $ Right a
  (>>=) = bindET

instance (Monad m) => Functor (EitherT e m) where
  fmap f et = EitherT $ fmap (fmap f) (runEitherT et)

instance (Monad m) => Applicative (EitherT e m) where
  pure a = EitherT $ return $ Right a
  ef <*> et = EitherT $ do
    unwrappedFunc <- runEitherT ef
    unwrappedVal <- runEitherT et
    return (unwrappedFunc <*> unwrappedVal)

instance MonadTrans (EitherT e) where
  lift m = EitherT (Right `liftM` m)
