{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

import Control.Monad
import Control.Monad.State
import qualified Data.ByteString.Lazy.Char8 as L8
import Control.Applicative
import Control.Monad.Trans
import MaybeTParse (ParseState(ParseState, string, offset))

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

instance (MonadState s m) => MonadState s (EitherT e m) where
  get = lift get
  put = lift . put

newtype Parse a = P
  { runP :: EitherT String (State ParseState) a
  } deriving (Monad, MonadState ParseState, Functor, Applicative)

evalParse :: Parse a -> L8.ByteString -> Either String a
evalParse parser str = evalState (runEitherT (runP parser)) (ParseState str 0)

singleParse :: Parse Char
singleParse = do
  s <- get
  case L8.uncons (string s) of
    Nothing -> P $ EitherT $ return $ Left "Empty string"
    Just (c, str) -> do
      put $ ParseState str (offset s + 1)
      return c
  
  
