module Main where

import Args
  ( AddOptions (..),
    Args (..),
    GetOptions (..),
    SearchOptions (..),
    parseArgs,
  )
import qualified Data.List as L
import qualified Entry.DB as DB
import Entry.Entry
  ( Entry (..),
    FmtEntry (FmtEntry),
    matchedByAllQueries,
    matchedByQuery,
    entryListDecoder,
    entryListEncoder
  )
  
import Result
import System.Environment (getArgs)
import Test.SimpleTest.Mock
import Prelude hiding (print, putStrLn, readFile)
import qualified Prelude
import qualified Bencode.Parser as Ben
import qualified Bencode.Decoder as De
import Bencode.Value
import qualified Entry.Query as Q
import Entry.Query (QueryTerm (..))
import Text.Read (Lexeme(String))

usageMsg :: String
usageMsg =
  L.intercalate
    "\n"
    [ "snip - code snippet manager",
      "Usage: ",
      "snip add <filename> lang [description] [..tags]",
      "snip search [code:term] [desc:term] [tag:term] [lang:term]",
      "snip get <id>",
      "snip init"
    ]

-- | Handle the init command
handleInit :: TestableMonadIO m => m ()
handleInit = 
  Test.SimpleTest.Mock.writeFile "snippets.ben" (DB.serialize DB.empty)

-- | Handle the get command
handleGet :: TestableMonadIO m => GetOptions -> m ()
handleGet getOpts = do
  DB.load >>= myfunc 
  where
    myfunc val =
      case val of
        Success ok -> case (DB.findFirst predicate ok) of
          Just v -> putStrLn (entrySnippet v)
          Nothing -> putStrLn "nothing"
        Error er -> putStrLn "Failed to load DB"
    predicate ent = if (entryId ent) == id then True else False
    id = getOptId getOpts
    

-- | Handle the search command
handleSearch :: TestableMonadIO m => SearchOptions -> m ()
handleSearch searchOpts = do
  DB.load >>= myfunc 
  where
    myfunc val =
      let 
        showFmtEntry :: [Entry] -> String
        showFmtEntry [] = ""
        showFmtEntry (x:xs) = (show (FmtEntry x)) ++ "\n" ++ showFmtEntry xs
      in
      case val of
        Success ok -> 
          case (DB.findAll (matchedByAllQueries (searchOptTerms searchOpts)) ok) of
            [] -> putStrLn "No entries found"
            l -> putStrLn (showFmtEntry l)
        Error er -> putStrLn "Failed to load DB"
    


-- readFile' :: TestableMonadIO m => String -> m String
-- readFile' filename = do
--   contents <- readFile filename
--   return $ length contents `seq` contents

-- -- | Load the database from disk.
-- load2 :: String -> m (Result DB.LoadDBError DB.SnippetDB)
-- load2 filename= do
--   contents <- readFile' filename
--   return $ case Ben.parse contents of
--     Error pe -> Error DB.CorruptedFile
--     Success bv -> buildEntries bv
--       where
--         buildEntries :: BencodeValue -> Result DB.LoadDBError DB.SnippetDB
--         buildEntries bv =
--           case De.runDecoder entryListDecoder bv of
--             Success es -> Success (DB.SnippetDB es)
--             Error de -> Error InvalidStructure    

-- | Handle the add command
handleAdd :: TestableMonadIO m => AddOptions -> m ()
handleAdd addOpts = do
  contents <-readFile (addOptFilename addOpts)
  let makeEntry2 id2 = makeEntry id2 contents addOpts
  let funcInsert mydb = DB.insertWith makeEntry2 mydb 

  --makeEntry2 id2 = makeEntry id2 contents addOpts
  DB.modify funcInsert >>= myfunc

  where
    myfunc val =
      case val of 
        Success ok -> putStrLn "ok"
        Error _ -> putStrLn "error"

    makeEntry :: Int -> String -> AddOptions -> Entry
    makeEntry id snippet addOpts =
      Entry
        { entryId = id,
          entrySnippet = snippet,
          entryFilename = addOptFilename addOpts,
          entryLanguage = addOptLanguage addOpts,
          entryDescription = addOptDescription addOpts,
          entryTags = addOptTags addOpts
        }

-- | Dispatch the handler for each command
run :: TestableMonadIO m => Args -> m ()
run (Add addOpts) = handleAdd addOpts
run (Search searchOpts) = handleSearch searchOpts
run (Get getOpts) = handleGet getOpts
run Init = handleInit
run Help = putStrLn usageMsg

main :: IO ()
main = do
  args <- getArgs
  let parsed = parseArgs args
  case parsed of
    (Error err) -> Prelude.putStrLn usageMsg
    (Success args) -> run args
