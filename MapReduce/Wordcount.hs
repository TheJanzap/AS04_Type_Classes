module MapReduce.WordCount where
import           Data.List (sortOn)
import           Data.Char (toLower)
import           Data.Monoid (Sum(..))
import           System.Directory
import           Control.Monad (filterM)
import           Data.Ord (comparing, Down(..))
import           Map.TreeMap
 
-- Mapping from String to Int. We are using the Sum Monoid wrapper.
type WordCount = Map String (Sum Int)

-- Translates all characters to lower case.
lower :: String -> String
lower = map toLower

-- Replaces punctuation with white spaces.
clear :: String -> String
clear = map remove 
  where remove c | c `elem` ".,'\"!?" = ' '
                 | otherwise         = c

-- Takes an input text, cleans it, and splits it into a list of words.
cleanWords :: String -> [String]
cleanWords = words . clear . lower

-- Takes an input text, cleans it, constructs a list of pairs ("WORD", 1), 
-- and builds the Map based on these.
-- The map creates a new tuple for every word, e.g. [("hello",1), ("world",1), ("hello",1)]
-- fromListMerge then merges the keys and uses the Sum monoid to add the values together: Sum 1 <> Sum 1 = Sum 2
wordCount:: String -> WordCount
wordCount = fromListMerge . map (\w -> (w,1)) . cleanWords

-- Takes the mapping function and uses the monoid instance of m to reduce the values.
-- In our case, folding all WordCounts from the different files (List of String) into one.
mapReduce :: Monoid m => (a -> m) -> [a] -> m
mapReduce f xs = foldMap f xs

-- Main program, loads text files and calculates the word count on them.
main :: IO ()
main = do
  elements <- listDirectory "."
  files <- filterM doesFileExist elements
  let txtFiles = filter (endsWith ".txt") files
  contents <- mapM readFile txtFiles
  -- Since we have a list of Strings from multiple files, we need to combine all WordCounts into one
  let result = mapReduce wordCount contents
  putStrLn (showResults result)

showResults :: WordCount -> String
showResults m = concat ["count: ", show (length kvs), "\ntop10:", show (take 10 topN)]
  where topN = sortOn (Down . snd) kvs
        kvs = toList m

endsWith :: String -> String -> Bool
endsWith end string = drop (length string - length end) string == end
