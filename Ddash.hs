module Ddash where
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set

{-

-- Finds the shortest number of transformations. 
-- Pre: dict is a dictionary filtered to only contain
--      Strings of same length as 'from' and 'to'
shortest_path:: String -> String -> [String] -> Int
shortest_path from to dict
  = sp_helper [from] to 0 []
  where 
    sp_helper :: [String] -> String -> Int -> [String] -> Int
    sp_helper currents to count visited
      | elem to currents = count
      | otherwise = sp_helper n' to (count+1) (currents++visited)
      where 
        n' = concat([one_step c dict visited| c<-currents])
-- todo: remove dup : try Data.Set


-- Given a string and a dictionary list and a visited list, 
-- return list of all posible string that a given string can 
-- be transformed to with 1 step transformation. 
-- String is not transformed into something that is already visited. 
one_step :: String -> [String] -> [String] -> [String]
one_step from dict visited
  = [d | d <- dict, compare_ex1 d from, not (elem d visited)]


-- Compare Except 1 Character
-- Returns true if 2 strings are identical except 1 character at 
-- the same position.
-- Pre: 2 strings of same length
compare_ex1 :: String -> String -> Bool
compare_ex1 s1 s2 
  = compare_ex1_acc s1 s2 0
  where
    compare_ex1_acc [] [] c
      | c == 1 = True
      | otherwise = False
    compare_ex1_acc (x:xs) (y:ys) count
      | x == y = compare_ex1_acc xs ys count
      | count < 1 = compare_ex1_acc xs ys (count+1)
      | otherwise = False

-}

--------- Optimisation --------------

---- Map Creation ----
createMap :: Int -> [String] -> Map.Map String [String]
createMap word_len dict
  = buildMap (filter (\x -> length x == word_len) dict) Map.empty
  where
    buildMap [] map = map
    buildMap (d:ds) map
      = buildMap ds (addWordToMap d (getAllKeys d) map)

addWordToMap :: String -> [String] -> Map.Map String [String] -> Map.Map String [String]
addWordToMap word [] map = map
addWordToMap word (k:keys) map
  | isNothing query = addWordToMap word keys (Map.insert k [word] map)
  | otherwise = addWordToMap word keys (Map.insert k (word:fromJust query) map)
  where
    query = Map.lookup k map 

-- pre: a non empty string
-- post:  return a list of strings replacing each 
--        character of the input string with "_"
--        eg: input "aaa", outputs ["_aa","a_a","aa_"] 
getAllKeys :: String -> [String]
getAllKeys word
  = map (replaceCharAt word) [0.. (length word)-1]
  where
    replaceCharAt word n
      = take n word ++ "_" ++ drop (n+1) word
------------------------
shortestPartOp :: String -> String -> [String] -> Int
shortestPartOp from to dict
  = expander (Set.singleton from) (Set.singleton) (createMap (length from) dict) Set.empty 0
  where
    expander :: Set.Set String -> Set.Set String -> Map.Map String [String] -> Set.Set String -> Int -> Int
    expander froms tos dict visited level
      | Set.size froms == 0 || Set.size tos == 0 = 999999
      | Set.size (Set.intersection froms tos)  /= 0 = level
      | Set.size (Set.intersection froms' tos) /= 0 = level+1
      | otherwise = expander froms' tos' dict visited'' (level+2)
      where 
        (froms',visited')  = expandOne froms dict visited
        (tos', visited'')    = expandOne tos dict visited'

expandOne :: Set.Set String -> Map.Map String [String] -> (Set.Set String, Set.Set String)
expandOne froms dict visited
  = (Set.unions [Set.difference (Set.fromList(Map.lookup k dict)) visited| f<-froms, k <- getAllKeys f],
     Set.union froms visited)
