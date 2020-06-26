### Curate Dataset  
  - read the reviews at `reviews.txt` 
  - read the labels at `labels.txt`

> import System.Environment
> import Control.Monad
> 
> readReviews = do
>   reviews <- readFile "reviews.txt"
>   return $ lines reviews
>   
> readLabels = do
>   labels <- readFile "labels.txt"
>   return $ lines labels



> readReviews >>= print . length



> readReviews >>= print . head



> readLabels >>= print . head


### Developing a Predictive Theory
- investigate the data
- predict the influence 

> reviewLength = 80
> 
> printEachSet labels reviews prev index = let
>     labelAt n = take reviewLength (labels !! n)
>     reviewAt n = take reviewLength (reviews !! n) 
>     printLine n = print $ (labelAt n) ++ "    :    " ++ (reviewAt n) in 
>         prev >> printLine index
>        
> prettyPrint indices = do
>     reviews <- readReviews
>     labels <- readLabels 
>     Prelude.foldl (printEachSet labels reviews) (print "label            review") indices 
>         
> prettyPrint [2137, 12816, 6267, 21934, 5297, 4998]


### Quick theory validation

- total word count in reviews
- positive/negative word count in reviews

> import Data.Map as M
> import Test.QuickCheck
> 
> type Counter = Map String Int



> import Data.List.Split
> splitReviews = splitOn " "



> -- getCount
> getCount :: String -> Counter -> Int
> getCount word cnts = if (M.member word cnts) then (cnts M.! word) else 0
> 
> -- test: getCount
> testGetCount :: String -> Int -> Bool
> testGetCount word count = (getCount word $ M.fromList [(word, count)]) == (count :: Int)
> 
> print "getCount :: String -> Counter -> Int"
> quickCheck testGetCount



> -- addCount
> addCount :: Counter -> String -> Counter
> addCount counter word = M.insert word ((+) 1 $ getCount word counter) counter 
> 
> -- test: addCount'
> wordCountMap :: String -> Int -> Counter
> wordCountMap word count = M.fromList [(word, count)]
> 
> testAddCount count word = let 
>     result = (addCount (wordCountMap word count) word) 
>     in result == (M.fromList [(word, count+1)])
> 
> print "addCount :: Counter -> String -> Counter"
> quickCheck testAddCount



> data Counters = Counters { count ::Int, 
>     allWords :: Counter,
>     posWords :: Counter,
>     negWords :: Counter } deriving (Show, Eq) 
>     
> emptyCounters = Counters {count=0, allWords=M.empty, posWords=M.empty, negWords=M.empty}
> 
> -- addPosNegCount
> addPosNegCount :: String -> Counters -> String -> Counters
> addPosNegCount label reduced word =
>     let Counters index totalCount positiveCount negativeCount = reduced
>         newPositiveCount = if label == "positive" then addCount positiveCount word else positiveCount
>         newNegativeCount = if label == "negative" then addCount negativeCount word else negativeCount
>         newTotalCount = addCount totalCount word in 
>             Counters (index + 1) newTotalCount newPositiveCount newNegativeCount
> 
> 
> -- test: addPosNegCount
> testAddPosCount :: String -> Bool
> testAddPosCount word = let 
>     result = addPosNegCount "positive" emptyCounters word
>     in result == Counters {count=1, 
>         allWords=M.fromList [(word,1)], 
>         posWords=M.fromList [(word,1)], 
>         negWords=M.empty}
> 
> testAddNegCount :: String -> Bool
> testAddNegCount word = let 
>     result = addPosNegCount "negative" emptyCounters word
>     in result == Counters {count=1, 
>         allWords=M.fromList [(word,1)], 
>         posWords=M.empty, 
>         negWords=M.fromList [(word,1)]}
> 
> print "addPosNegCount"
> print "positive"
> quickCheck testAddPosCount
> print "negative"
> quickCheck testAddNegCount



> -- groupWords
> groupWords :: String -> [String] -> Counters -> Counters
> groupWords label wordList initialData = 
>     Prelude.foldl (addPosNegCount label) initialData wordList
> 
> -- test: groupWords
> testGroupPositiveWords :: [String] -> Bool
> testGroupPositiveWords words = let 
>     result = groupWords "positive" words emptyCounters
>     wordsLength = length words
>     ones = replicate wordsLength 1
>     counterWithWords = Prelude.foldl (\a one -> addCount a one) M.empty words
>     in result == Counters {count=wordsLength, allWords=counterWithWords, posWords=counterWithWords, negWords=M.empty}
> 
> testGroupNegativeWords :: [String] -> Bool
> testGroupNegativeWords words = let 
>     result = groupWords "negative" words emptyCounters
>     wordsLength = length words
>     ones = replicate wordsLength 1
>     counterWithWords = Prelude.foldl (\a one -> addCount a one) M.empty words
>     in result == Counters {count=wordsLength, allWords=counterWithWords, posWords=M.empty, negWords=counterWithWords}
> 
> print "groupWords :: positive -> [String] -> Counters -> Counters"
> quickCheck testGroupPositiveWords
> 
> print "groupWords :: negative -> [String] -> Counters -> Counters"
> quickCheck testGroupNegativeWords



> counts label review initialData = let splitted = splitReviews review in
>         groupWords label splitted initialData
> 
> inputSize = 20
> countAll readLabels readReviews = do  
>     labels <- readLabels
>     reviews <- readReviews
>     let pair = zip (take inputSize labels) (take inputSize reviews)
>         initialData = emptyCounters
>         mergeCounts = (\all -> \one -> counts (fst one) (snd one) all) in
>         return $ Prelude.foldl mergeCounts initialData pair 



> countAll readLabels readReviews >>= 
>     \counters -> 
>     let x = show $ size $ allWords counters
>         y = show $ size $ posWords counters
>         z = show $ size $ negWords counters
>         in print $ "total words: " ++ x  ++ ", positive: " ++ y ++ ", negative: " ++ z




> safeTake :: Counter -> String -> Double
> safeTake m word = if member word m then fromIntegral $ m ! word else 0
> 
> calcRatio :: Counter -> Counter -> String -> Double
> calcRatio pos neg word = let 
>     ratio = (pos `safeTake` word) / (1 + (neg `safeTake` word)) in 
>         if ratio > 0 then log ratio else  0 - log (1/(ratio + 0.01))
> calcRatio (M.fromList [("ab", 3)]) (M.fromList []) "ab"
> 
> mergeRatio :: Counter -> Counter -> Map String Double -> String -> Map String Double 
> mergeRatio pos neg ratios word = let
>     ratio = calcRatio pos neg word in 
>     M.insert word ratio ratios
> 
> mergeRatio (M.fromList [("ab", 1)]) (M.fromList []) M.empty "ab"
> mergeRatio (M.fromList []) (M.fromList [("ab", 1)]) M.empty "ab"
> 
> positiveToNegRatios = do
>     counters <- countAll readLabels readReviews
>     let merge = mergeRatio (posWords counters) (negWords counters) in 
>         return $ Prelude.foldl merge M.empty (keys $ allWords counters)
> 
> -- cutoff = 1
> -- positiveToNegRatios >>= \x -> let r = M.filter (\cnt -> cnt > -cutoff || cnt < cutoff) x in print r










