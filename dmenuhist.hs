import Data.Char
import System.IO
import System.Process
import qualified Data.Map as Map
import Data.List (sortBy)
import Data.Function (on)
import System.Environment
import System.Directory

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
    . sortBy ((flip compare) `on` fst)
    . map (\ioLine -> let sortValue = Map.findWithDefault 0 ioLine histMap in (sortValue, ioLine))
    . lines

readFileIfExists :: FilePath -> IO String
readFileIfExists filePath = do
    fileExists <- doesFileExist filePath
    if fileExists
        then readFile filePath
        else return ""

updateHistMap :: HistMap -> String -> HistMap
updateHistMap histMap [] = histMap
updateHistMap histMap selectedEntry = Map.insertWith (+) (init selectedEntry) 1 histMap

histContentsToMap :: String -> HistMap
histContentsToMap
    = foldr
        (\histLine acc ->
            let (v, k) = splitLineByFrequencyAndValue histLine
                vi = read v :: Integer in
            Map.insert k vi acc) Map.empty
    . lines

histMapToContents :: HistMap -> String
histMapToContents
    = unlines
    . map (\(k, vi) -> show vi ++ " " ++ k)
    . Map.toList

main = do
    (histFilePath:(cmd:cmdArgs)) <- getArgs

    -- | TODO: issue, try opening multiple dmenuhistory without selecting anything with fzf.. it cannot read from histfile on susbsequent it seems
    (Just cmdStdIn, Just cmdStdOut, _, _) <- createProcess (proc cmd cmdArgs){ std_in = CreatePipe, std_out = CreatePipe }

    histContents <- readFileIfExists histFilePath
    let histMap = histContentsToMap histContents

    ioContents <- getContents
    let sortedIoContent = sortIoByHistMap histMap ioContents

    hPutStr cmdStdIn sortedIoContent
    hClose cmdStdIn

    selectedEntry <- hGetContents cmdStdOut
    let updatedHistMap = updateHistMap histMap selectedEntry

    writeFile histFilePath $ histMapToContents updatedHistMap
    hClose cmdStdOut
    putStr selectedEntry

