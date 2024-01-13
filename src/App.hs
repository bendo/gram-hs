{-#LANGUAGE ScopedTypeVariables#-}

module App
    ( app
    ) where

import           Options.Applicative

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

data Options = Options
    { add  :: Maybe String
    , todo :: Bool
    , view :: Bool
    }

gram :: Parser Options
gram = Options
    <$> optional (strOption $ long "add" <> short 'a' <> help "Add learned lesson")
    <*> switch (long "todo" <> short 't' <> help "Show all lessons which should be repeated")
    <*> switch (long "overview" <> short 'o' <> help "Show overview")

versionOpts :: Parser (a -> a)
versionOpts = infoOption appVersion (long "version" <> short 'v' <> help "Show version")

app :: IO ()
app = prompt =<< execParser opts
  where
    opts = info (helper <*> versionOpts <*> gram)
      ( fullDesc
     <> progDesc appDesc
     <> header appHead )

prompt :: Options -> IO ()
prompt (Options lesson False False) = add' lesson
prompt (Options _ True False) = todo'
prompt (Options _ False True) = view'
prompt _ = return ()

add' :: Show a => Maybe a -> IO ()
add' lesson = case lesson of
    Just l -> putStrLn $ "Adding lesson " ++ show l ++ "."
    Nothing -> todo'

todo' :: IO ()
todo' = putStrLn "Todo: "

view' :: IO ()
view' = putStrLn "Overview of all lessons: "
