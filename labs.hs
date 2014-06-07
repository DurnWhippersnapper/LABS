import Data.List
import Control.Parallel.Strategies

-- Performs half of an autocorrelation (only positive lags)
autocorr :: [Int] -> [Int]
autocorr = (\x -> map (sum . (zipWith (*) x)) (init(tails x)))

score :: [Int] -> Int
score = sum . (map (^2)) . autocorr

-- Returns a list of all binary sequences of length n
bin_seqs :: Int -> [[Int]]
bin_seqs n = iterate add_bit [[]] !! n
    where add_bit seqs = [b : seq | seq <- seqs, b <- [-1,1]]

score_seqs :: [[Int]] -> [(Int, [Int])]
-- without parallelism
score_seqs = map (\x -> (score x, x))

-- With parallelism, but not lazy
--score_seqs = parMap rdeepseq (\x -> (score x, x))

-- With parallesism, and lazy (will not eat your ram)
--score_seqs seqs = withStrategy (parBuffer 100000 rdeepseq) (map (\x -> (score x, x)) seqs)

-- Find the set of binary sequences that has the lowest score.
labs :: [[Int]] -> [[Int]]
labs seqs = prune scored_seqs (score (head seqs)) []
    where scored_seqs = score_seqs seqs
          prune [] _ best_seqs = best_seqs
          prune ((score,x):xs) min best_seqs = case (compare score min) of
                                        LT -> prune xs score [x]
                                        GT -> prune xs min best_seqs
                                        EQ -> prune xs min (x:best_seqs)

main :: IO ()
main = do
    putStrLn ("Min Seqs :\n" ++ show_seqs min_seqs)
    where min_seqs = labs $ bin_seqs 24
          to01 = map (\x -> (x + 1) `div` 2)
          show_seqs = (intercalate "\n") . (map show.(map to01))
