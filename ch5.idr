import System

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

main : IO ()
main = time >>= guess 0 . toNat . (\n => n `mod` 100)
