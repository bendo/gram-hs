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
import           Text.Read

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

data Options = Options (Maybe String) Bool Bool Bool deriving Show

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
prompt (Options lesson False False False) = add lesson
prompt (Options _ True False False)       = count
prompt (Options _ False True False)       = todo
prompt (Options _ False False True)       = view
prompt _                                  = return ()

getLesson :: Lesson -> String
getLesson (Lesson lesson _ _) = T.unpack lesson

getLevel :: Lesson -> Shortcut
getLevel (Lesson _ level _) = level

add :: Maybe String -> IO ()
add lesson = case lesson of
    Just l -> do
        let r = readMaybe l :: Maybe Int
        case r of
            Just ri ->
                if ri < 1 || ri > 80 then
                    putStrLn "Lesson has to be in interval 1 to 80."
                else do
                    putStrLn $ "Adding lesson " ++ show ri ++ "."
                    dbPath <- getDBPath
                    conn <- open dbPath
                    let lessonT = T.pack (show ri) :: T.Text
                    rust <- query conn "SELECT * FROM lesson WHERE lesson = ?" (Only lessonT) :: IO [Lesson]
                    if null rust then do
                        newDate <- getNewDate T
                        addLesson conn (Lesson
                            (T.pack (show ri))
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
            Nothing -> putStrLn "Lesson has to be a number in interval 1 to 80."
    Nothing -> todo

getNewDate :: Shortcut -> IO Day
getNewDate level = do
    now <- getCurrentTime
    let (year, month, day) = toGregorian (utctDay now)
    case level of
        T -> addXDays 4 year month day
        A -> addXDays 9 year month day
        G -> addXDays 13 year month day
        M -> addXDays 21 year month day
        E -> addXDays 34 year month day
        B -> addXDays 55 year month day
    where addXDays x y m d = return $ addDays x (fromGregorian y m d)

getNewLevel :: Shortcut -> Shortcut
getNewLevel level =
    case level of
        T -> A
        A -> G
        G -> M
        M -> E
        E -> B
        B -> B

count :: IO ()
count = do
    dbPath <- getDBPath
    conn <- open dbPath
    now <- getCurrentTime
    ls <- query conn "SELECT COUNT(*) FROM lesson WHERE level != 'B' AND due_date <= ?" (Only $ utctDay now) :: IO [Only Int]
    forM_ ls $ \(Only count'') -> print count''
    close conn

todo :: IO ()
todo = do
    stdoutSupportsANSI <- hSupportsANSI stdout
    if stdoutSupportsANSI then do
        dbPath <- getDBPath
        conn <- open dbPath
        now <- getCurrentTime
        todoLessons <- query conn "SELECT * FROM lesson WHERE level != 'B' AND due_date <= ?" (Only $ utctDay now) :: IO [Lesson]
        setSGR [ Reset ]
        putStr "Todo: "
        forM_ todoLessons printTODOLesson
        putStrLn ""
        close conn
    else
        showNotSupportedMsg

printTODOLesson :: Lesson -> IO ()
printTODOLesson lesson = do
    setColorForLevel (getLevel lesson)
    putStr $ getLesson lesson ++ " "
    setSGR [ Reset ]

setColorForLevel :: Shortcut -> IO ()
setColorForLevel level =
    case level of
        T -> setSGR [ Reset ]
        A -> setSGR [ SetColor Foreground Dull Magenta ]
        G -> setSGR [ SetColor Foreground Dull Yellow ]
        M -> setSGR [ SetColor Foreground Dull Green ]
        E -> setSGR [ SetColor Foreground Dull Blue ]
        B -> setSGR [ SetColor Foreground Dull Black ]

showNotSupportedMsg :: IO ()
showNotSupportedMsg = putStrLn "Standard output does not support 'ANSI' escape codes."

view :: IO ()
view = do
    dbPath <- getDBPath
    conn <- open dbPath
    lessons <- query conn "SELECT * FROM lesson" () :: IO [Lesson]
    setSGR [ SetConsoleIntensity BoldIntensity ]
    putStrLn "\nLessons overview:\n"
    setSGR [ Reset ]
    let pairs = [(getLesson x, x) | x <- lessons]
    forM_ [1..80] (\a -> do
            let result = find ((== show a).fst) pairs
            case result of
                Just r -> do
                    let lala = snd r
                    setColorForLevel (getLevel lala)
                    printLesson (read (getLesson lala) :: Integer) a
                    setSGR [ Reset ]
                Nothing ->
                    printLesson (a :: Integer) a
            )
    putStrLn ""
    mapM_ (printLevel lessons) [(T,TODO), (A,APPRENTICE), (G,GURU), (M,MASTER), (E,ENLIGHTENED), (B,BURNED)]
    putStrLn "\n"
    close conn

printLevel :: Show a => [Lesson] -> (Shortcut, a) -> IO ()
printLevel lessons level = do
    let levels = [getLevel x | x <- lessons]
    let lvlCount = case fst level of
                     T -> 80 - length (filter (T /=) levels)
                     _ -> length $ filter (fst level ==) levels
    setColorForLevel $ fst level
    putStr $ show (snd level) ++ " (" ++ show lvlCount ++ ")   "
    setSGR [ Reset ]

printLesson :: (PrintfArg t, Integral a) => t -> a -> IO ()
printLesson x a = putStr $ Text.Printf.printf "%02d" x ++ (if mod a 20 == 0 then "  \n" else "  ")
