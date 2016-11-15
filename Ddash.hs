module Ddash where
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe
import Data.List as List

---- Map Creation ----
--create map of wildcard to words relating to it 
--for example the map could contain    "_it" -> ["hit"]
--                                and  "h_t_ -> ["hit","hot"]
--                                and  "do_" -> ["dot","dog"]
--                                and so on for all words in the dictionary
--this will make looking up the dictionary for the one-step transformation easier.
createMap :: Int -> [String] -> Map.Map String [String]
createMap word_len dict
  = buildMap (filter (\x -> length x == word_len) dict) Map.empty
  where
    buildMap [] map = map
    buildMap (d:ds) map
      = buildMap ds (addWordToMap d map)

-- adding a new word into the map
addWordToMap :: String -> Map.Map String [String] -> Map.Map String [String]
addWordToMap word map 
  = addWordToMap' word (getAllKeys word) map
  where
    addWordToMap' word [] map = map
    addWordToMap' word (k:keys) map
      | isNothing query = addWordToMap' word keys (Map.insert k [word] map)
      | otherwise = addWordToMap' word keys (Map.insert k (word:fromJust query) map)
      where
        query = Map.lookup k map 

-- pre:   'word' - a non empty string
-- post:  return a list of strings replacing each 
--        character of the input string with "_"
--        eg: input "aaa", outputs ["_aa","a_a","aa_"] 
--        ie. the keys of the map that maps to value 
--            which is a list of words that has 'word'
--            as one of its element 
getAllKeys :: String -> [String]
getAllKeys word
  = map (replaceCharAt word) [0.. (length word)-1]
  where
    replaceCharAt word n
      = take n word ++ "_" ++ drop (n+1) word
------------------------
--
--Finding shortest path --
--
-- shortest path calculated by doing a two-sided BFS from 'from' and 'to'
-- in order to reduce the search space
shortestPathOp :: String -> String -> [String] -> Int
shortestPathOp from to dict
  = expander [from] [to] (createMap (length from) dict) [] 0
  where
    expander :: [String] -> [String] -> Map.Map String [String] -> [String] -> Int -> Int
    expander froms tos dict visited level
      | length froms == 0 || length tos == 0 = error "no valid path given the dictionary"
      | length (List.intersect froms tos)  /= 0 = level
      | length (List.intersect froms' tos) /= 0 = level+1
      | otherwise = expander froms' tos' dict visited'' (level+2)
      where 
        (froms',visited')  = expandOne froms dict visited
        (tos', visited'')    = expandOne tos dict visited'

-- Given 'from' list, search all possible strings that can be made by 1-step transformation
-- from the elements in the 'from' list.
-- This done by is doing BFS with 1 level forward.
-- ie. 1. for each 'f' in 'from' find the keys by getAllKeys 
--        (eg. "hot" would have keys ["_ot","h_t","ho_"])
--     2. Use those keys to lookup the 'dict' Map for and the values will be 
--        all the words that can be made from 'f' with 1-step transformations 
--
--this function returns the new expanded fringe and a new visited list as a tuple (expanded, visited)
expandOne :: [String] -> Map.Map String [String] -> [String] -> ([String],[String])
expandOne froms dict visited
  = (foldl (++) [] (rmDup [if isNothing (Map.lookup k dict) then [] 
                      else (fromJust(Map.lookup k dict) List.\\ (froms++visited))
                    | f<-froms, k <- getAllKeys f]),
     froms++visited)

-- remove duplicate from list using Data.Set library
-- O(nlogn) runtime
rmDup :: Ord a => [a] -> [a]
rmDup s
  = (Set.toList.Set.fromList) s
