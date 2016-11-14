import IC.TestSuite hiding (Id)
import qualified IC.TestSuite as TS
import Ddash

shortest_pathTests = [("hit","cog",["hit","dot","dog","cog","hot","log"]) ==> 4]

allTestCases
  = [testCase "shortest_path" (uncurry3 shortest_path) shortest_pathTests]

runTests = mapM_ goTest allTestCases

main = runTests
