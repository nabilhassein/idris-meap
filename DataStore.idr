module DataStore

import Data.Vect


infixr 5 .+.
data Schema = SString | SInt | (.+.) Schema Schema

SchemaType : Schema -> Type
SchemaType SString   = String
SchemaType SInt      = Int
SchemaType (x .+. y) = (SchemaType x, SchemaType y)

record DataStore where
  constructor MkData
  schema : Schema
  size : Nat
  items : Vect size (SchemaType schema)

addToStore : (store : DataStore) -> SchemaType (schema store) -> DataStore
addToStore (MkData schema size items) newItem = MkData schema _ (items ++ [newItem])

data Command : (schema : Schema) -> Type where
  Add : SchemaType schema -> Command schema
  Get : Integer -> Command schema
  Quit : Command schema

parseCommand : (schema : Schema) -> String -> String -> Maybe (Command schema)
parseCommand schema "add"  rest = Just (Add (?parseBySchema rest))
parseCommand schema "get"  val  = if all isDigit (unpack val)
                                  then Just . Get $ cast val
                                  else Nothing
parseCommand schema "quit"  _   = Just Quit
parseCommand _       _      _   = Nothing

parse : (schema : Schema) -> String -> Maybe (Command schema)
parse schema input = case span (/= ' ') input of
      (cmd, args) => parseCommand schema cmd (ltrim args)

display : SchemaType schema -> String
display {schema = SString} str    = str
display {schema = SInt}    num    = show num
display {schema = _ .+. _} (a, b) = display a ++ ", " ++ display b

getEntry : Integer -> (store : DataStore) -> Maybe (String, DataStore)
getEntry pos store = case integerToFin pos (size store) of
  Nothing => Just ("Out of range\n", store)
  Just id => Just (display (index id $ items store) ++ "\n", store)

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store userInput = case parse (schema store) userInput of
  Nothing         => Just ("Invalid command\n", store)
  Just (Add item) => Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
  Just (Get pos)  => getEntry pos store
  Just Quit       => Nothing

main : IO ()
main = replWith (MkData SString _ []) "Command: " processInput


-- 4.3.5
search : String -> (store : DataStore) -> List (Fin (size store), String)
search s (MkData SString _ items) = foldr (\(index, item), acc =>
 if s `isInfixOf` item then (index, item) :: acc else acc
) [] (zip range items)
