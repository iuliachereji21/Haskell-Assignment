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
    matchedByQuery
  )

import Result
import System.Environment (getArgs)
import Test.SimpleTest.Mock
import Prelude hiding (print, putStrLn, readFile)
import qualified Prelude
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
        Success ok -> case DB.findFirst predicate ok of
          Just v -> putStrLn (entrySnippet v)
          Nothing -> putStrLn "nothing"
        Error er -> putStrLn "Failed to load DB"
    predicate ent = entryId ent == id
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
        showFmtEntry (x:xs) = show (FmtEntry x) ++ "\n" ++ showFmtEntry xs
      in
      case val of
        Success ok ->
          case DB.findAll (matchedByAllQueries (searchOptTerms searchOpts)) ok of
            [] -> putStrLn "No entries found"
            l -> putStrLn (showFmtEntry l)
        Error er -> putStrLn "Failed to load DB"

-- | Handle the add command
handleAdd :: TestableMonadIO m => AddOptions -> m ()
handleAdd addOpts = do
  contents <-readFile (addOptFilename addOpts)
  let makeEntry2 id2 = makeEntry id2 contents addOpts
  let funcInsert mydb = DB.insertWith makeEntry2 mydb
  let predicate ent = entrySnippet ent == contents
  val <- DB.load
  case val of
    Success ok ->
      case DB.findFirst predicate ok of
          Just v -> putStrLn ("Entry with this content already exists: " ++ "\n" ++ show (FmtEntry v))
          Nothing -> DB.modify funcInsert >>= myfunc
    Error _ -> putStrLn "Failed to load DB"

  where
    myfunc val =
      case val of
        Success ok -> putStrLn "ok"
        Error _ -> putStrLn "Failed to load DB"

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
