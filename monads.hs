import System.Random
import Control.Monad

rollNDiceIO :: Int -> IO [Int]
rollNDiceIO n = replicateM n $ randomRIO (1,6)

rollDice :: StdGen -> ((Int, Int), StdGen)
rollDice gen = ((n, m), nextnextGen) where
  (n, nextGen) = randomR (1,6) gen
  (m, nextnextGen) = randomR (1,6) nextGen

newtype State' s a = State' {runState' :: s -> (a, s)}
  
instance Functor (State' s) where
  fmap f (State' processor) = State' processor' where
    processor' state = (f value, nextState) where
      (value, nextState) = processor state
    --target = State' s b or State' (s -> (b, s)) where f :: a -> b
  
instance Applicative (State' s) where
  pure value = State' $ \state -> (value, state)
  --State' s (a -> b) -> State' s a -> State' s b
  --constructor = State' (\state -> (f, nextState))
  State' processor <*> State' processor' = State' newProcessor where
    newProcessor state = (f value, nextnextState) where
      (f, nextState) = processor state
      (value, nextnextState) = processor' nextState

instance Monad (State' s) where
  return value = State' $ \state -> (value, state) --same as Applicative instance
  State' processor1 >>= f = State' $ \state0 -> let
    (value, state1) = processor1 state0 in
    runState' (f value) state1
    
