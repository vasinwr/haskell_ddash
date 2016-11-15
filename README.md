How to run
--
This code is written in haskell. To run it, you would have to

1. Load the file into GHC's interactive environment `ghci Ddash.hs`
2. In ghci, use the function `shortestPathOp` to solve the problem by 
   `shortestPathOp <start_word> <target_word> <dictionary>` 
    eg. `shortestPathOp "hit" "cog" ["hit","dot","dog","cog","hot","log"]`

Running Tests
--
Test can be run by calling `runghc Test.hs` in the commandline.
