{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module App
    ( app
    ) where

import           Control.Applicative
import           Data.Data                        (Typeable)
import           Data.Foldable
import qualified Data.Text                        as T
import           Data.Time
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.ToField
import           Options.Applicative
import           System.Console.ANSI
import           System.Directory
import           System.IO                        (stdout)
import           Text.Printf

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

data Lesson = Lesson T.Text Shortcut T.Text deriving Show

instance FromRow Lesson where
    fromRow = Lesson <$> field <*> field <*> field

instance ToRow Lesson where
    toRow (Lesson lesson level due_date) = toRow (lesson, level, due_date)

data Shortcut = T | A | G | M | E | B deriving (Eq, Ord, Read, Show, Typeable)

instance ToField Shortcut where
    toField = toField . show

instance FromField Shortcut where
    fromField f =
        fromField f >>= parse . reads
      where
        parse ((s,""):_) = return s
        parse _  = returnError ConversionFailed f "invalid Shortcut field"

data Name = TODO | APPRENTICE | GURU | MASTER | ENLIGHTENED | BURNED deriving Show

data Level = Level Shortcut Name SGR deriving Show

myColor = SetColor Foreground Dull

bold = SetConsoleIntensity BoldIntensity

magenta = myColor Magenta
yellow = myColor Yellow
green = myColor Green
blue = myColor Blue
black = myColor Black
nothing = Reset

getShortcut :: Level -> Shortcut
getShortcut (Level shortcut _ _) = shortcut

getName :: Level -> Name
getName (Level _ name _) = name

getColor :: Level -> SGR
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
    execute_ conn "CREATE TABLE IF NOT EXISTS lesson (lesson TEXT, level TEXT, due_date TEXT)"
    close conn
    prompt =<< execParser opts
  where
    opts = info (helper <*> versionOpts <*> gram)
      ( fullDesc
     <> progDesc appDesc
     <> header appHead )

getDBPath :: IO FilePath
getDBPath = do getXdgDirectory XdgData "gram/gram.db"

addLesson :: ToRow q => Connection -> q -> IO ()
addLesson conn = execute conn "INSERT INTO lesson (lesson, level, due_date) VALUES (?,?,?)"

prompt :: Options -> IO ()
prompt (Options lesson False False False) = add' lesson
prompt (Options _ True False False)       = count'
prompt (Options _ False True False)       = todo'
prompt (Options _ False False True)       = view'
prompt _                                  = return ()

getLesson :: Lesson -> String
getLesson (Lesson lesson _ _) = T.unpack lesson

getLevel :: Lesson -> Shortcut
getLevel (Lesson _ level _) = level

getDueDate :: Lesson -> String
getDueDate (Lesson _ _ dueDate) = T.unpack dueDate

add' :: Maybe String -> IO ()
add' lesson = case lesson of
    Just l  -> do
        let r = read l :: Int
        if r < 1 || r > 80
            then
                putStrLn "Lesson has to be in interval 1 to 80."
            else do
                putStrLn $ "Adding lesson " ++ show r ++ "."
                dbPath <- getDBPath
                conn <- open dbPath
                let lessonT = T.pack (show r) :: T.Text
                rust <- query conn "SELECT * FROM lesson WHERE lesson = ?" (Only lessonT) :: IO [Lesson]
                if null rust then do
                    newDate <- getNewDate T
                    addLesson conn (Lesson
                        (T.pack (show r))
                        (getNewLevel T)
                        (T.pack (showGregorian newDate)))
                else do
                    let selectedLesson = head rust
                    newDate <- getNewDate (getLevel selectedLesson)
                    executeNamed conn "UPDATE lesson SET level = :level, due_date = :due_date WHERE lesson = :lesson"
                        [ ":level" := (getNewLevel (getLevel selectedLesson) :: Shortcut)
                        , ":due_date" := T.pack (show newDate)
                        , ":lesson" := (T.pack (getLesson selectedLesson) :: T.Text)]
                close conn
    Nothing -> todo'

getNewDate :: Shortcut -> IO Day
getNewDate level = do
    now <- getCurrentTime
    let (year, month, day) = toGregorian (utctDay now)
    case level of
        T -> return $ addDays 4 (fromGregorian year month day)
        A -> return $ addDays 9 (fromGregorian year month day)
        G -> return $ addDays 13 (fromGregorian year month day)
        M -> return $ addDays 21 (fromGregorian year month day)
        E -> return $ addDays 34 (fromGregorian year month day)
        B -> return $ addDays 55 (fromGregorian year month day)

getNewLevel :: Shortcut -> Shortcut
getNewLevel level =
    case level of
        T -> A
        A -> G
        G -> M
        M -> E
        E -> B
        B -> B

count' :: IO ()
count' = putStrLn "2"

todo' :: IO ()
todo' = do
    stdoutSupportsANSI <- hSupportsANSI stdout
    if stdoutSupportsANSI
        then do
            dbPath <- getDBPath
            conn <- open dbPath
            now <- getCurrentTime
            todoLessons <- query conn "SELECT * FROM lesson WHERE due_date <= ?" (Only $ utctDay now) :: IO [Lesson]
            setSGR [ nothing ]
            putStr "Todo: "
            forM_ todoLessons printTODOLesson
        else
            showNotSupportedMsg

printTODOLesson :: Lesson -> IO ()
printTODOLesson lesson = do
    setColorForLevel (getLevel lesson)
    putStr $ getLesson lesson ++ " "
    setSGR [ nothing ]

setColorForLevel :: Shortcut -> IO ()
setColorForLevel level =
    case level of
        T -> setSGR [ nothing ]
        A -> setSGR [ magenta ]
        G -> setSGR [ yellow ]
        M -> setSGR [ green ]
        E -> setSGR [ blue ]
        B -> setSGR [ black ]

showNotSupportedMsg :: IO ()
showNotSupportedMsg = putStrLn "Standard output does not support 'ANSI' escape codes."

view' :: IO ()
view' = do
    setSGR [ bold ]
    putStrLn "\nLessons overview:\n"
    setSGR [ nothing ]
    forM_ [1..80 :: Int] printLesson
    putStrLn "\nAll levels\n"

printLesson :: (PrintfArg a, Integral a) => a -> IO ()
printLesson lesson = putStr $ Text.Printf.printf "%02d" lesson ++ (if mod lesson 20 == 0 then "  \n" else "  ")
