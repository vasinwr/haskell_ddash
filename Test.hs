import IC.TestSuite hiding (Id)
import qualified IC.TestSuite as TS
import qualified Data.Map as Map
import qualified Data.Set as Set
import Ddash

getAllKeysTests = [("hello") ==> ["_ello","h_llo","he_lo","hel_o","hell_"],
                   ("aaa") ==> ["_aa","a_a","aa_"],
                   ("x") ==> ["_"],
                   ("y") ==> ["_"]
                  ]

addWordToMapTests = [("hit",Map.empty) ==> Map.fromList[("_it",["hit"]),("h_t",["hit"]),("hi_",["hit"])],
                     ("hit",Map.empty) ==> Map.fromList[("_it",["hit"]),("hi_",["hit"]),("h_t",["hit"])],
                     ("hot",Map.fromList[("_it",["hit"]),("hi_",["hit"]),("h_t",["hit"])])
                       ==> Map.fromList[("ho_",["hot"]),("_ot",["hot"]),("_it",["hit"]),
                                        ("hi_",["hit"]),("h_t",["hot","hit"])]
                    ]

createMapTests = [
                  (3,["aaa"]) ==> Map.fromList[("_aa",["aaa"]),("a_a",["aaa"]),("aa_",["aaa"])],
                  (3,["aaa","a"]) ==> Map.fromList[("_aa",["aaa"]),("a_a",["aaa"]),("aa_",["aaa"])],
                  (1,["aaa","a","b","c"]) ==> Map.fromList[("_",["c","b","a"])]
                  ]

expandOneTests = [
                  (["a"],Map.fromList[("_",["a","b","c","d"])],[]) ==> (["b","c","d"],["a"]),
                  (["a"],Map.fromList[("_",["a","b","c","d"])],["b","c"]) ==> (["d"],["a","b","c"]) ,
                  (["a","b"],Map.fromList[("_",["a","b","c"])],[]) ==> (["c"],["a","b"]) ,
                  (["bb"],Map.fromList[("_a",["aa"]),("a_",["aa"])],[]) ==> ([],["bb"]) 
                  ]

rmDupTests = [ 
              (["1","1","1"]) ==> ["1"],
              (["1","2","3"]) ==> ["1","2","3"],
              (["1","2","3","2","3"]) ==> ["1","2","3"],
              ([]) ==> [],
              (["1","2","2","1"]) ==> (Set.toList . Set.fromList) ["2","1"]
              ]

shortestPathOpTests = [("hit","cog",["hit","dot","dog","cog","hot","log"]) ==> 4,
                        ("hit","hit", ["hit","dot","dog","cog","hot","log"]) ==> 0,
                        ("aaaa","cccc", ["aaab","aabb","abbb","bbbb","bcbb","ccbb","ccbz","cccz","cccc"]) ==> 9
                      ]

allTestCases
  = [ testCase "getAllKeysTests" getAllKeys getAllKeysTests,
      testCase "addWordToMapTests" (uncurry addWordToMap) addWordToMapTests,
      testCase "createMapTests" (uncurry createMap) createMapTests,
      testCase "expandOneTests" (uncurry3 expandOne) expandOneTests,
      testCase "rmDupTests" rmDup rmDupTests,
      testCase "shortestPath" (uncurry3 shortestPathOp) shortestPathOpTests
    ]

runTests = mapM_ goTest allTestCases

main = runTests
