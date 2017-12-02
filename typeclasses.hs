--Applicative
instance Applicative Maybe where
  pure :: a -> Maybe a
  pure x = Just x

  Just f <*> Just x = Just (f x)
  Nothing <*> _ = Nothing
  _ <*> Nothing = Nothing

