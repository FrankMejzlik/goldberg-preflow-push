
-- Third party libs
import Text.Pretty.Simple -- USAGE: stack ghci --package pretty-simple ./tests.hs

-- My modules
import Types
import Goldberg
import TestingNetworks -- << Test networks are generated in this module

{--------------------------------------------
 ============== TEST ALGORITHM =============
---------------------------------------------}

{- >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    Separate functions for each available test network.  -}
testGoldbergNet1 = runGoldberg net1
testGoldbergNet2 = runGoldberg net2
testGoldbergNet3 = runGoldberg net3

{- >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    Runs all the ALGORITHM tests.  -}
testGoldberg = do    
    let
        testOneNet (name, net, maxFlow) = do
            let 
                resFlow = getNetworkFlow (runGoldberg net)
                expFlow = maxFlow
            putStrLn ("Testing: " ++ name)
            putStrLn ("\tExpected = " ++ show (fromRational expFlow))
            putStrLn ("\tResult = " ++ show (fromRational resFlow))
            putStr " >>> "
            print (expFlow == resFlow)

    putStrLn "Testing all the networks..."
    mapM_ testOneNet allTestNets
{- <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< -}



{--------------------------------------------
 ============ TEST FUNCTIONALITY ============ 
---------------------------------------------}

{- >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    Runs all the FUNCTIONALITY tests.  -}
runAllFuncTests = do 
    let r1 = testGetCleanQueue
        ro = testOther
        r2 = testGoldbergInitialize
        r3 = testPush
        r4 = testLiftVertex
    
    putStrLn "Running all tests..."
    return (and r1 && and r2 && and r3 && and r4 && and ro)
{- <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< -}


{--------------------------------------------
                Unit tests
---------------------------------------------}
{- 
    TEST: `filterOnlyDescNonSat` 
-}
testOther = [
    test1_filterOnlyDescNonSat, 
    test1GetOutEdges, test2GetOutEdges, test3GetOutEdges, test4GetOutEdges, test5GetOutEdges, test6GetOutEdges,
    test1GetInEdges, test2GetInEdges, test3GetInEdges, test4GetInEdges, test5GetInEdges, test6GetInEdges
    ]

{- `filterOnlyDescNonSat` -}
test1_filterOnlyDescNonSat = filterOnlyDescNonSat vs outEdges 10 == [(1,0),(1,2),(1,3)]
    where 
       net@(Network vs es s t netQ) = goldbergInitialize net1
       outEdges = getOutEdges es 1

{- `getOutEdges` -}
test1GetOutEdges = length (getOutEdges es 0) == 2 where es = network_es $ goldbergInitialize net2
test2GetOutEdges = length (getOutEdges es 1) == 3 where es = network_es $ goldbergInitialize net2
test3GetOutEdges = length (getOutEdges es 2) == 4 where es = network_es $ goldbergInitialize net2
test4GetOutEdges = length (getOutEdges es 3) == 4 where es = network_es $ goldbergInitialize net2
test5GetOutEdges = length (getOutEdges es 4) == 3 where es = network_es $ goldbergInitialize net2
test6GetOutEdges = length (getOutEdges es 5) == 2 where es = network_es $ goldbergInitialize net2

{- `getInEdges` -}
test1GetInEdges = length (getInEdges es 0) == 2 where es = network_es $ goldbergInitialize net2
test2GetInEdges = length (getInEdges es 1) == 3 where es = network_es $ goldbergInitialize net2
test3GetInEdges = length (getInEdges es 2) == 4 where es = network_es $ goldbergInitialize net2
test4GetInEdges = length (getInEdges es 3) == 4 where es = network_es $ goldbergInitialize net2
test5GetInEdges = length (getInEdges es 4) == 3 where es = network_es $ goldbergInitialize net2
test6GetInEdges = length (getInEdges es 5) == 2 where es = network_es $ goldbergInitialize net2

{- 
    TEST: `getCleanQueue` 
-}
testGetCleanQueue = [test1_getCleanQueue, test2_getCleanQueue, test3_getCleanQueue]

-- Test Queue
t_q1 = [[1, 4, 5, 6], [], [3, 2]]

-- Test1 `getCleanQueue` on net1
test1_getCleanQueue = getCleanQueue t_q1 3 1 == [[4, 5, 6], [], [2]]

-- Test2 `getCleanQueue` on net1
test2_getCleanQueue = getCleanQueue t_q1 6 2 == [[1, 4, 5], [], [3]]

-- Test3 `getCleanQueue` on net1
test3_getCleanQueue = getCleanQueue t_q1 33 33 == [[1, 4, 5, 6], [], [3, 2]]


{- 
    TEST: `goldbergInitialize` 
-}
testGoldbergInitialize = [test_initNet1, test_initNet2]

-- Network #1
test_initNet1 =
    let 
        postNet@(Network vs es s t q) = goldbergInitialize net1
    in
        -- Check post-conditions
        vertex_ex (findVertex vs 1) == 2 
        && vertex_ex (findVertex vs 2) == 4
        && ( edge_f (fst (findEdge es 0 1)) == 2 && edge_f (snd (findEdge es 0 1)) == -2)
        && ( edge_f (fst (findEdge es 0 2)) == 4 && edge_f (snd (findEdge es 0 2)) == -4)

-- Network #2
test_initNet2 =
    let 
        postNet@(Network vs es s t q) = goldbergInitialize net2
    in
        -- Check post-conditions
        vertex_ex (findVertex vs 1) == 16
        && vertex_ex (findVertex vs 2) == 13
        && ( edge_f (fst (findEdge es 0 1)) == 16 && edge_f (snd (findEdge es 0 1)) == -16)
        && ( edge_f (fst (findEdge es 0 2)) == 13 && edge_f (snd (findEdge es 0 2)) == -13)

{- 
    TEST: `push` 
-}
testPush = [test1PushInitNet1, test2PushInitNet1]

-- Push in net1 through (1, 2)
test1PushInitNet1 =
    let 
        postNet@(Network vs es s t q) = push (goldbergInitialize net1) (1, 2)
    in
        -- Check post-conditions
        vertex_ex (findVertex vs 1) == 0 
        && vertex_ex (findVertex vs 2) == 6
        && ( edge_f (fst (findEdge es 1 2)) == 2 && edge_f (snd (findEdge es 1 2)) == -2)
    
-- Push in net1 through (2, 3)
test2PushInitNet1 =
    let 
        postNet@(Network vs es s t q) = push (goldbergInitialize net1) (2, 3)
    in
        -- Check post-conditions
        vertex_ex (findVertex vs 2) == 0
        && vertex_ex (findVertex vs 3) == 4
        && ( edge_f (fst (findEdge es 2 3)) == 4 && edge_f (snd (findEdge es 2 3)) == -4)


{- 
    TEST: `liftVertex` 
-}
testLiftVertex = [test1LiftVertex, test2LiftVertex]

-- Lift1 in net1
test1LiftVertex = 
    let 
       net = goldbergInitialize net1
       outEdges = getOutEdges es 1
       liftedNet@(Network vs es _ _ _) = liftVertex net 1 5
    in 
        -- Check post-conditions
        null (vertex_des (findVertex vs 0))
        && vertex_h (findVertex vs 1) == 5 -- Must have the correct height
        && length (vertex_des (findVertex vs 1)) ==  3 -- It is now the highest

-- Lift1 in net1
test2LiftVertex = 
    let 
       net = goldbergInitialize net1
       outEdges = getOutEdges es 1
       liftedNet@(Network vs es _ _ _) = liftVertex net 1 1
    in 
         -- Check post-conditions
        null (vertex_des (findVertex vs 0))
        && vertex_h (findVertex vs 1) == 1  -- Must have the correct height
        && length (vertex_des (findVertex vs 1)) ==  2

