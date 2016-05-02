module DataStore

import Data.Vect


infixr 5 .+.
data Schema = SChar | SString | SInt | (.+.) Schema Schema

SchemaType : Schema -> Type
SchemaType SChar     = Char
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
  GetAll : Command schema
  Quit : Command schema

parsePrefix : (schema : Schema) -> String -> Maybe (SchemaType schema, String)
parsePrefix SChar     input = (getQuoted . unpack) input
  where
    getQuoted : List Char -> Maybe (Char, String)
    getQuoted ('\'' :: c :: '\'' :: rest) = Just (c, ltrim $ pack rest)
    getQuoted _                           = Nothing
parsePrefix SString   input = (getQuoted . unpack) input
  where
    getQuoted : List Char -> Maybe (String, String)
    getQuoted ('"' :: cs) = case span (/= '"') cs of
       (quoted, '"' :: rest) => Just (pack quoted, ltrim $ pack rest)
       _                     => Nothing
    getQuoted _           = Nothing

parsePrefix SInt      input = case span isDigit input of
                              ("", rest)  => Nothing
                              (num, rest) => Just (cast num, ltrim rest)
parsePrefix (lschema .+. rschema) input = do
  (l_val, input')  <- parsePrefix lschema input
  (r_val, input'') <- parsePrefix rschema input'
  return ((l_val, r_val), input'')



parseBySchema : (schema : Schema) -> String -> Maybe (SchemaType schema)
parseBySchema schema input = case parsePrefix schema input of
                                  Just (res, "") => Just res
                                  _              => Nothing

parseCommand : (schema : Schema) -> String -> String -> Maybe (Command schema)
parseCommand schema "add"  rest = map Add $ parseBySchema schema rest
parseCommand _      "get"  ""   = Just GetAll
parseCommand _      "get"  val  = if all isDigit $ unpack val
                                  then Just . Get $ cast val
                                  else Nothing
parseCommand _      "quit"  _   = Just Quit
parseCommand _       _      _   = Nothing

parse : (schema : Schema) -> String -> Maybe (Command schema)
parse schema input = case span (/= ' ') input of
      (cmd, args) => parseCommand schema cmd (ltrim args)

display : SchemaType schema -> String
display {schema = SChar}   char   = show char
display {schema = SString} str    = "\"" ++ str ++ "\""
display {schema = SInt}    num    = show num
display {schema = _ .+. _} (a, b) = display a ++ ", " ++ display b

getEntry : Integer -> (store : DataStore) -> Maybe (String, DataStore)
getEntry pos store = case integerToFin pos (size store) of
  Nothing => Just ("Out of range\n", store)
  Just id => Just (display (index id $ items store) ++ "\n", store)

getAll : DataStore -> Maybe (String, DataStore)
getAll store = case size store of
  Z => Just ("Store is empty\n", store)
  _ => Just ((concat . intersperse "; " . map display $ items store) ++ "\n", store)

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store userInput = case parse (schema store) userInput of
  Nothing         => Just ("Invalid command\n", store)
  Just (Add item) => Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
  Just (Get pos)  => getEntry pos store
  Just GetAll     => getAll store
  Just Quit       => Nothing

main : IO ()
main = replWith (MkData (SString .+. SString .+. SInt) _ []) "Command: " processInput


-- 4.3.5
search : String -> (store : DataStore) -> List (Fin (size store), String)
search s (MkData SString _ items) = foldr (\(index, item), acc =>
 if s `isInfixOf` item then (index, item) :: acc else acc
) [] (zip range items)
