import System
import Data.Vect

-- 5.2.4
(>>) : Monad m => m a -> m b -> m b
ma >> mb = ma >>= \_ => mb

guess : Nat -> Nat -> IO ()
guess guesses target = do
  putStrLn "Guess a number"
  input <- getLine
  if all isDigit (unpack $ trim input)
  then case compare (cast input) target of
    LT => putStrLn "too low" >> guess (S guesses) target
    EQ => putStrLn $ "you guessed it in " ++ show guesses ++ " guesses!"
    GT => putStrLn "too high" >> guess (S guesses) target
  else putStrLn "invalid input, enter a natural (nonnegative) number" >> guess guesses target



-- 5.3.5
isEmpty : String -> Bool
isEmpty s = case unpack s of
  []       => True
  (_ :: _) => False

readToBlank : IO (List String)
readToBlank = do
  putStrLn "enter some input. blank line to finish"
  input <- getLine
  if isEmpty input
  then return []
  else do
    rest <- readToBlank
    return $ input :: rest

readAndSave : IO ()
readAndSave = do
  input <- readToBlank
  putStrLn "enter a filename"
  filename <- getLine
  writeFile filename (unlines input)
  return ()

readVectFile : String -> IO (n ** Vect n String)
readVectFile filename = do
  Right file <- readFile filename | Left err => return (_ ** [])
  return (_ ** fromList $ lines file)
