import           Data.Char                      ( isDigit )
import           System.IO                      ( hPutStr
                                                , hPutStrLn
                                                , hClose
                                                , stderr
                                                , hGetLine
                                                )
import           System.Process                 ( createProcess
                                                , std_out
                                                , std_in
                                                , StdStream(CreatePipe)
                                                , proc
                                                )
import qualified Data.Map                      as Map
import           Data.List                      ( sortOn )
import           System.Environment             ( getArgs )
import           System.Directory               ( doesFileExist
                                                , renameFile
                                                )
import           Control.Exception              ( try )
import           GHC.IO.Exception               ( IOException(..) )

type HistMap = Map.Map String Integer

splitLineByWeightAndName :: String -> (String, String)
splitLineByWeightAndName [] = ([], [])
splitLineByWeightAndName (x : xs) | isDigit x = (x : ys, zs)
                                  | otherwise = ([], xs)
  where (ys, zs) = splitLineByWeightAndName xs

sortIoByHistMap :: HistMap -> String -> String
sortIoByHistMap histMap = unlines . sortOn (negate . getWeight) . lines
  where getWeight ioLine = Map.findWithDefault 0 ioLine histMap

readFileIfExists :: FilePath -> IO String
readFileIfExists filePath = doesFileExist filePath >>= getFileContents
 where
  getFileContents exists | exists    = readFile filePath
                         | otherwise = return ""

updateHistMap :: HistMap -> String -> HistMap
updateHistMap histMap [] = histMap
updateHistMap histMap selectedEntry =
  Map.insertWith (+) selectedEntry 1 histMap

histContentsToMap :: String -> HistMap
histContentsToMap = foldr accHistMap Map.empty . lines
 where
  accHistMap histLine = Map.insert name weight'
   where
    (weight, name) = splitLineByWeightAndName histLine
    weight'        = read weight :: Integer

histMapToContents :: HistMap -> String
histMapToContents = unlines . map mapToLine . Map.toList
  where mapToLine (name, weight) = show weight ++ " " ++ name

getArguments :: IO (String, String, [String])
getArguments = getArgs >>= validateArgs
 where
  validateArgs (histFilePath : cmd : cmdArgs) =
    return (histFilePath, cmd, cmdArgs)
  validateArgs _ =
    error
      $ "\nMissing historyFilePath or command to run.\n"
      ++ "Usage: dmenuhist <historyFilePath> <dmenuCompatibleCommand>\n"
      ++ "Example: echo -e \"Entry1\\nEntry2\\nEntry3\\nEntry4\" | dmenuhist ~/goofyHistory.txt rofi -dmenu\n"

main :: IO ()
main = do
  (histFilePath, cmd, cmdArgs)          <- getArguments
  (Just cmdStdIn, Just cmdStdOut, _, _) <- createProcess (proc cmd cmdArgs)
    { std_in  = CreatePipe
    , std_out = CreatePipe
    }
  histMap       <- histContentsToMap <$> readFileIfExists histFilePath
  sortedContent <- sortIoByHistMap histMap <$> getContents
  hPutStr cmdStdIn sortedContent
  hClose cmdStdIn
  selectedEntry <- hGetLine cmdStdOut
  let updatedHistMap = updateHistMap histMap selectedEntry
  let histMapContent = histMapToContents updatedHistMap
  try (writeToHistory histFilePath histMapContent) >>= printIfException
  putStr selectedEntry
  hClose cmdStdOut

writeToHistory :: FilePath -> String -> IO ()
writeToHistory histFilePath contents =
  writeFile tempFilePath contents >> renameFile tempFilePath histFilePath
  where tempFilePath = histFilePath ++ ".temp"

printIfException :: Either IOError a -> IO ()
printIfException (Right _) = return ()
printIfException (Left  e) = hPutStrLn stderr $ unlines
  [ "Could not write to history cache."
  , "Probably because you opened multiple instances of dmenuhist at the same time."
  , "No worries though, you only lost the last history entry."
  , "Error details:"
  , "ioe_filename = " ++ show (ioe_filename e)
  , "ioe_description = " ++ show (ioe_description e)
  , "ioe_errno = " ++ show (ioe_errno e)
  ]

