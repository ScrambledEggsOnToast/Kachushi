module Kachushi.Util where 

splice :: [a] -> [Int] -> [[a]]
splice [] [] = []
splice xs [] = [xs]
splice xs (n:ns) = take n xs : splice (drop n xs) ns
