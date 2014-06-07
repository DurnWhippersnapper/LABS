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
labs :: [[Int]] -> [[Int]]
labs seqs = prune seqs (score (head seqs)) []
    where prune [] _ best_seqs = best_seqs
          prune (x:xs) min best_seqs = case (compare this_score min) of
                                        LT -> prune xs this_score [x]
                                        GT -> prune xs min best_seqs
                                        EQ -> prune xs min (x:best_seqs)
                                        where this_score = score x

main :: IO ()
main = do
    putStrLn ("Min Seqs :\n" ++ show_seqs min_seqs)
    where min_seqs = labs $ bin_seqs 24
          to01 = map (\x -> (x + 1) `div` 2)
          show_seqs = (intercalate "\n") . (map show.(map to01))
