{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Supply
  (
    Supply
  , next
  , runSupply
  ) where

import Control.Monad.State

newtype Supply s a = S (State [s] a) deriving (Applicative, Monad, Functor)
runSupply :: Supply s a -> [s] -> (a, [s])
runSupply (S m) = runState m 

next :: Supply s (Maybe s)
next = S $ do
  st <- get
  case st of
    [] -> return Nothing
    (x:xs) -> do
      put xs
      return (Just x)

showTwo :: (Show s) => Supply s String
showTwo = do
  a <- next
  b <- next
  return ("a: " ++ show a ++ ", b: " ++ show b)
