module DataStore

import Data.Vect

-- 4.3 but skipping ahead to ch6 for record syntax -- solves 4.3.5 ex1
record DataStore where
  constructor MkData
  size : Nat
  items : Vect size String

addToStore : DataStore -> String -> DataStore
addToStore (MkData size items) s = MkData _ (items ++ [s])

data Command = Add String | Get Integer | Quit

parseCommand : (cmd : String) -> (args : String) -> Maybe Command
parseCommand "add"  str = Just (Add str)
parseCommand "get"  val = if all isDigit (unpack val)
                          then Just . Get $ cast val
                          else Nothing
parseCommand "quit" _   = Just Quit
parseCommand _      _   = Nothing

parse : String -> Maybe Command
parse input = case span (/= ' ') input of
      (cmd, args) => parseCommand cmd (ltrim args)

getEntry : Integer -> DataStore -> Maybe (String, DataStore)
getEntry pos store = case integerToFin pos (size store) of
  Nothing => Just ("Out of range\n", store)
  Just id => Just (index id (items store) ++ "\n", store)

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store userInput = case parse userInput of
  Nothing         => Just ("Invalid command\n", store)
  Just (Add item) => Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
  Just (Get pos)  => getEntry pos store
  Just Quit       => Nothing

main : IO ()
main = replWith (MkData _ []) "Command: " processInput


-- 4.3.5
search : String -> DataStore -> List (Nat, String)
search s (MkData _ items) = foldr (\(index, item), acc =>
 if s `isInfixOf` item
 then (finToNat index, item) :: acc
 else acc
) [] (zip range items)
