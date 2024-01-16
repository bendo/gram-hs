{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module App
    ( app
    ) where

import           Control.Applicative
import qualified Data.Text                      as T
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow
import           Options.Applicative
import           System.Directory
import           System.IO

appHead :: String
appHead = "Grammatik Aktiv SRS system"

appDesc :: String
appDesc = "Gram is spaced repetion learning system tool to help to learn german grammar " ++
    "from the book (Grammatik Aktiv - Cornelsen - daf). Book has 80 lessons."

appVersion :: String
appVersion = "gram 1.0.0 \n\
    \Copyright (C) 2023 Free Software Foundation, Inc. \n\
    \License GPLv3+: GNU GPL version 3 or later <https://gnu.org/licenses/gpl.html>. \n\
    \This is free software: you are free to change and redistribute it. \n\
    \There is NO WARRANTY, to the extent permitted by law. \n\
    \\n\
    \Written by bendo."

data Shortcut = T | A | G | M | E | B deriving Show

data Name = TODO | APPRENTICE | GURU | MASTER | ENLIGHTENED | BURNED deriving Show

data Color = RED | GREEN | ORANGE | BLUE | PINK | DARK | NONE deriving Show

data Level = Level Shortcut Name (Maybe Color) deriving Show

levelT = Level T TODO Nothing
levelA = Level A APPRENTICE (Just PINK)
levelG = Level G GURU (Just ORANGE)
levelM = Level M MASTER (Just GREEN)
levelE = Level E ENLIGHTENED (Just BLUE)
levelB = Level B BURNED (Just DARK)

getShortcut :: Level -> Shortcut
getShortcut (Level shortcut _ _) = shortcut

getName :: Level -> Name
getName (Level _ name _) = name

getColor :: Level -> Maybe Color
getColor (Level _ _ color) = color

data Options = Options
    { add   :: Maybe String
    , count :: Bool
    , todo  :: Bool
    , view  :: Bool
    }

gram :: Parser Options
gram = Options
    <$> optional (strOption $ long "add" <> short 'a' <> help "Add learned lesson")
    <*> switch (long "count" <> short 'c' <> help "Show count of lessons which should be repeated")
    <*> switch (long "todo" <> short 't' <> help "Show all lessons which should be repeated")
    <*> switch (long "overview" <> short 'o' <> help "Show overview")

versionOpts :: Parser (a -> a)
versionOpts = infoOption appVersion (long "version" <> short 'v' <> help "Show version")

app :: IO ()
app = do
    appDir <- getXdgDirectory XdgData "gram"
    createDirectoryIfMissing True appDir
    conn <- open $ appDir ++ "/gram.db"
    execute_ conn "CREATE TABLE IF NOT EXISTS test (id INTEGER PRIMARY KEY, str TEXT)"
    close conn
    prompt =<< execParser opts
  where
    opts = info (helper <*> versionOpts <*> gram)
      ( fullDesc
     <> progDesc appDesc
     <> header appHead )

prompt :: Options -> IO ()
prompt (Options lesson False False False) = add' lesson
prompt (Options _ True False False)       = count'
prompt (Options _ False True False)       = todo'
prompt (Options _ False False True)       = view'
prompt _                                  = return ()

add' :: Show a => Maybe a -> IO ()
add' lesson = case lesson of
    Just l  -> putStrLn $ "Adding lesson " ++ show l ++ "."
    Nothing -> todo'

count' :: IO ()
count' = putStrLn "2"

todo' :: IO ()
todo' = putStrLn "Todo: "

view' :: IO ()
view' = putStrLn "Overview of all lessons: "
