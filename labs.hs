import Data.List

-- Performs half of an autocorrelation (only positive lags)
autocorr :: [Int] -> [Int]
autocorr = (\x -> map (sum . (zipWith (*) x)) (init(tails x)))

score :: [Int] -> Int
score = sum . (map (^2)) . autocorr

-- Returns a list of all binary sequences of length n
bin_seqs :: Int -> [[Int]]
bin_seqs n = iterate add_bit [[]] !! n
    where add_bit seqs = [b : seq | seq <- seqs, b <- [-1,1]]

-- Find the set of binary sequences that has the lowest score.
labs :: Int -> (Int, [[Int]])
labs n = (min_score, min_seqs)
    where seqs = bin_seqs n
          seqs_with_scores = map (\x -> (score x, x)) seqs
          min_score = minimum (map fst seqs_with_scores)
          min_seqs = map snd $ filter ((== min_score).fst) seqs_with_scores

main :: IO ()
main = do
    putStrLn ("Min Score = " ++ (show min_score))
    putStrLn ("Min Seqs = \n" ++ (intercalate "\n" (map show min_seqs)))
    where (min_score, min_seqs) = labs 5
