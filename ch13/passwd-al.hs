import           Control.Monad      (when)
import           Data.List
import           System.Environment (getArgs)
import           System.Exit
import           System.IO

main = do
  args <- getArgs --[String]
  when (length args /= 2) $ do
    putStrLn "Invalid syntax"
    exitFailure

  content <- readFile (args !! 0)
  let username = findByUID content (read (args !! 1))

  case username of
    Just x  -> putStrLn x
    Nothing -> putStrLn "Could not find that UID"

findByUID :: String -> Integer -> Maybe String
findByUID content uid =
  let al = map parseline $ lines $ content in
    lookup uid al

parseline :: String -> (Integer, String)
parseline input =
  let fields = split ':' input in
    (read (fields !! 2), fields !! 0)

split :: Char -> [Char] -> [[Char]]
split delim line =
  if null line then []
  else let (first, remain) = span (/= delim) line in
    first : case remain of
      [] -> []
      _ -> (split delim (tail remain))
