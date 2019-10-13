import Data.Char
import System.IO
import System.Process
import qualified Data.Map as Map
import Data.List (sortBy)
import Data.Function (on)
import System.Environment
import System.Directory
import Control.Exception (try)
import GHC.IO.Exception (IOException(..))

type HistMap = Map.Map String Integer

splitLineByFrequencyAndValue :: String -> (String, String)
splitLineByFrequencyAndValue [] = ([],[])
splitLineByFrequencyAndValue xs@(x:xs') 
    | Data.Char.isAlphaNum x    =  (x:ys, zs) 
    | otherwise                 =  ([], drop 1 xs)
        where (ys, zs) = splitLineByFrequencyAndValue xs'

sortIoByHistMap :: HistMap -> String -> String
sortIoByHistMap histMap
    = unlines
    . map snd
    . sortBy (flip compare `on` fst)
    . map (\ioLine -> let sortValue = Map.findWithDefault 0 ioLine histMap in (sortValue, ioLine))
    . lines

readFileIfExists :: FilePath -> IO String
readFileIfExists filePath = doesFileExist filePath >>= \exists -> if exists then readFile filePath else pure ""

updateHistMap :: HistMap -> String -> HistMap
updateHistMap histMap [] = histMap
updateHistMap histMap selectedEntry = Map.insertWith (+) (init selectedEntry) 1 histMap

histContentsToMap :: String -> HistMap
histContentsToMap
    = foldr
        (\histLine acc ->
            let (v, k)  = splitLineByFrequencyAndValue histLine
                vi      = read v :: Integer in
            Map.insert k vi acc) Map.empty
    . lines

histMapToContents :: HistMap -> String
histMapToContents
    = unlines
    . map (\(k, vi) -> show vi ++ " " ++ k)
    . Map.toList

main =
    getArgs >>= \(histFilePath:(cmd:cmdArgs)) ->
    createProcess (proc cmd cmdArgs){ std_in = CreatePipe, std_out = CreatePipe } >>= \(Just cmdStdIn, Just cmdStdOut, _, _) ->
    histContentsToMap <$> (readFileIfExists histFilePath) >>= \histMap ->
    sortIoByHistMap histMap <$> getContents >>= \sortedContent ->
    hPutStr cmdStdIn sortedContent >>
    hClose cmdStdIn >>
    hGetContents cmdStdOut >>= \selectedEntry ->
    try (writeToHistory histFilePath $ histMapToContents $ updateHistMap histMap selectedEntry) >>=
    printIfException >>
    putStr selectedEntry >>
    hClose cmdStdOut

writeToHistory histFilePath contents =
    writeFile tempFilePath contents >> renameFile tempFilePath histFilePath
    where tempFilePath = histFilePath ++ ".temp"

printIfException :: Either IOError a -> IO ()
printIfException (Right _) = return ()
printIfException (Left e) =  hPutStrLn stderr $ unlines
    ["Could not write to history cache.",
    "Probably because you opened multiple instances of dmenuhist at the same time.",
    "No worries though, you only lost the last history entry.",
    "Error details:",
    "ioe_filename = " ++ show (ioe_filename e),
    "ioe_description = " ++ show (ioe_description e),
    "ioe_errno = " ++ show (ioe_errno e)]


