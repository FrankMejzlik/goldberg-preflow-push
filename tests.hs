
-- Third party libs
import Text.Pretty.Simple -- USAGE: stack ghci --package pretty-simple ./main.hs

-- My modules
import Types
import Goldberg

{--------------------------------------------
 TESTS
---------------------------------------------}
-- Test network #1
net1_V = [V 0, V 1, V 2, V 3]
net1_E = [E 0 1 2.0, E 0 2 4.0, E 1 2 3.0, E 2 3 5.0, E 1 3 1.0]
net1 = N net1_V net1_E 0 3


-- Test network #2
net2_V = [V 0, V 1, V 2, V 3, V 4, V 5]
net2_E = [
        E 0 1 16.0, 
        E 0 2 13.0, 

        E 1 2 4.0,
        E 1 3 12.0,

        E 2 3 9.0,
        E 2 4 14.0,

        E 4 3 7.0,
        E 4 5 4.0,

        E 3 5 20.0
    ]
net2 = N net2_V net2_E 0 5

-- Test Queue
t_q1 = [[1, 4, 5, 6], [], [3, 2]]

{- +++++++++++++++++++++++ -}
{-  -}
mainTest = runGoldberg net1


{- -}
{- +++++++++++++++++++++++ -}


{- Runs all the tests. -}
testAll = do 
    let r1 = test_getCleanQueue
        ro = test_other
        r2 = test_goldbergInitialize
        r3 = test_push
        r4 = test_liftVertex
    
    putStrLn "Running all tests..."
    return (and r1 && and r2 && and r3 && and r4 && and ro)

{- TEST: `filterOnlyDescNonSat` -}
test_other = [test_filterOnlyDescNonSat, test_getOutEdges, test_getInEdges]

test_filterOnlyDescNonSat = filterOnlyDescNonSat vs outEdges 10 == [(1,0),(1,2),(1,3)]
    where 
       net@(Network vs es s t netQ) = goldbergInitialize net1
       outEdges = getOutEdges es 1

{- TEST: `getOutEdges` -}
test_getOutEdges = getOutEdges es 0 == 
        [Edge {edge_fr = 0, edge_to = 1, eedge_c = 2 / 1, edge_f = 2 / 1},Edge {edge_fr = 0, edge_to = 2, eedge_c = 4 / 1, edge_f = 4 / 1}]
    where 
       net@(Network vs es s t netQ) = goldbergInitialize net1

{- TEST: `getInEdges` -}
test_getInEdges = getInEdges es 3 == 
        [Edge {edge_fr = 2, edge_to = 3, eedge_c = 5 / 1, edge_f = 0 / 1},Edge {edge_fr = 1, edge_to = 3, eedge_c = 1 / 1, edge_f = 0 / 1}]
    where 
       net@(Network vs es s t netQ) = goldbergInitialize net1

{- TEST: `getCleanQueue` -}
test_getCleanQueue = [test1_q1, test2_q1, test3_q1]

test1_q1 = getCleanQueue t_q1 3 1 == [[4, 5, 6], [], [2]]

test2_q1 = getCleanQueue t_q1 6 2 == [[1, 4, 5], [], [3]]

test3_q1 = getCleanQueue t_q1 33 33 == [[1, 4, 5, 6], [], [3, 2]]


{- TEST: `goldbergInitialize` -}
test_goldbergInitialize = [test_initNet1]

test_initNet2 =
    goldbergInitialize net2 ==
    (Network
    { network_vs =
        [ Vertex
            { vertex_name = 0
            , vertex_h = 4
            , vertex_ex = 9 / 1
            , vertex_des = []
            }
        , Vertex
            {vertex_name = 1, vertex_h = 0, vertex_ex = 2 / 1, vertex_des = []}
        , Vertex
            {vertex_name = 2, vertex_h = 0, vertex_ex = 4 / 1, vertex_des = []}
        , Vertex
            {vertex_name = 3, vertex_h = 0, vertex_ex = 0 / 1, vertex_des = []}
        ]
    , network_es =
        [ Edge {edge_fr = 0, edge_to = 1, eedge_c = 2 / 1, edge_f = 2 / 1}
        , Edge {edge_fr = 1, edge_to = 0, eedge_c = 2 / 1, edge_f = (-2) / 1}
        , Edge {edge_fr = 0, edge_to = 2, eedge_c = 4 / 1, edge_f = 4 / 1}
        , Edge {edge_fr = 2, edge_to = 0, eedge_c = 4 / 1, edge_f = (-4) / 1}
        , Edge {edge_fr = 1, edge_to = 2, eedge_c = 3 / 1, edge_f = 0 / 1}
        , Edge {edge_fr = 2, edge_to = 3, eedge_c = 5 / 1, edge_f = 0 / 1}
        , Edge {edge_fr = 1, edge_to = 3, eedge_c = 1 / 1, edge_f = 0 / 1}
        , Edge {edge_fr = 2, edge_to = 1, eedge_c = 3 / 1, edge_f = 0 / 1}
        , Edge {edge_fr = 3, edge_to = 2, eedge_c = 5 / 1, edge_f = 0 / 1}
        , Edge {edge_fr = 3, edge_to = 1, eedge_c = 1 / 1, edge_f = 0 / 1}
        ]
    , network_s = 0
    , network_t = 3
    , network_Q = [[2, 1], [], [], [], []]
    })

test_initNet1 =
    goldbergInitialize net1 ==
    (Network
    { network_vs =
        [ Vertex
            { vertex_name = 0
            , vertex_h = 4
            , vertex_ex = 9 / 1
            , vertex_des = []
            }
        , Vertex
            {vertex_name = 1, vertex_h = 0, vertex_ex = 2 / 1, vertex_des = []}
        , Vertex
            {vertex_name = 2, vertex_h = 0, vertex_ex = 4 / 1, vertex_des = []}
        , Vertex
            {vertex_name = 3, vertex_h = 0, vertex_ex = 0 / 1, vertex_des = []}
        ]
    , network_es =
        [ Edge {edge_fr = 0, edge_to = 1, eedge_c = 2 / 1, edge_f = 2 / 1}
        , Edge {edge_fr = 1, edge_to = 0, eedge_c = 2 / 1, edge_f = (-2) / 1}
        , Edge {edge_fr = 0, edge_to = 2, eedge_c = 4 / 1, edge_f = 4 / 1}
        , Edge {edge_fr = 2, edge_to = 0, eedge_c = 4 / 1, edge_f = (-4) / 1}
        , Edge {edge_fr = 1, edge_to = 2, eedge_c = 3 / 1, edge_f = 0 / 1}
        , Edge {edge_fr = 2, edge_to = 3, eedge_c = 5 / 1, edge_f = 0 / 1}
        , Edge {edge_fr = 1, edge_to = 3, eedge_c = 1 / 1, edge_f = 0 / 1}
        , Edge {edge_fr = 2, edge_to = 1, eedge_c = 3 / 1, edge_f = 0 / 1}
        , Edge {edge_fr = 3, edge_to = 2, eedge_c = 5 / 1, edge_f = 0 / 1}
        , Edge {edge_fr = 3, edge_to = 1, eedge_c = 1 / 1, edge_f = 0 / 1}
        ]
    , network_s = 0
    , network_t = 3
    , network_Q = [[2, 1], [], [], [], []]
    })

{- TEST: `push` -}
test_push = [test1_pushInitNet1]

test1_pushInitNet1 =
    push (goldbergInitialize net1) (1, 2) ==
    (Network
    { network_vs =
        [ Vertex
            {vertex_name = 1, vertex_h = 0, vertex_ex = 0 / 1, vertex_des = []}
        , Vertex
            {vertex_name = 2, vertex_h = 0, vertex_ex = 6 / 1, vertex_des = []}
        , Vertex
            { vertex_name = 0
            , vertex_h = 4
            , vertex_ex = 9 / 1
            , vertex_des = []
            }
        , Vertex
            {vertex_name = 3, vertex_h = 0, vertex_ex = 0 / 1, vertex_des = []}
        ]
    , network_es =
        [ Edge {edge_fr = 1, edge_to = 2, eedge_c = 3 / 1, edge_f = 2 / 1}
        , Edge {edge_fr = 2, edge_to = 1, eedge_c = 3 / 1, edge_f = (-2) / 1}
        , Edge {edge_fr = 0, edge_to = 1, eedge_c = 2 / 1, edge_f = 2 / 1}
        , Edge {edge_fr = 1, edge_to = 0, eedge_c = 2 / 1, edge_f = (-2) / 1}
        , Edge {edge_fr = 0, edge_to = 2, eedge_c = 4 / 1, edge_f = 4 / 1}
        , Edge {edge_fr = 2, edge_to = 0, eedge_c = 4 / 1, edge_f = (-4) / 1}
        , Edge {edge_fr = 2, edge_to = 3, eedge_c = 5 / 1, edge_f = 0 / 1}
        , Edge {edge_fr = 1, edge_to = 3, eedge_c = 1 / 1, edge_f = 0 / 1}
        , Edge {edge_fr = 3, edge_to = 2, eedge_c = 5 / 1, edge_f = 0 / 1}
        , Edge {edge_fr = 3, edge_to = 1, eedge_c = 1 / 1, edge_f = 0 / 1}
        ]
    , network_s = 0
    , network_t = 3
    , network_Q = [[2], [], [], [], []]
    })

test2_pushInitNet1 =
    push ( liftVertex (goldbergInitialize net1) 2 4) (2, 1) ==
    (Network
    { network_vs =
        [ Vertex
            { vertex_name = 2
            , vertex_h = 4
            , vertex_ex = 1 / 1
            , vertex_des = [(2, 3)]
            }
        , Vertex
            {vertex_name = 1, vertex_h = 0, vertex_ex = 5 / 1, vertex_des = []}
        , Vertex
            {vertex_name = 0, vertex_h = 4, vertex_ex = 9 / 1, vertex_des = []}
        , Vertex
            {vertex_name = 3, vertex_h = 0, vertex_ex = 0 / 1, vertex_des = []}
        ]
    , network_es =
        [ Edge {edge_fr = 2, edge_to = 1, eedge_c = 3 / 1, edge_f = 3 / 1}
        , Edge {edge_fr = 1, edge_to = 2, eedge_c = 3 / 1, edge_f = (-3) / 1}
        , Edge {edge_fr = 0, edge_to = 1, eedge_c = 2 / 1, edge_f = 2 / 1}
        , Edge {edge_fr = 1, edge_to = 0, eedge_c = 2 / 1, edge_f = (-2) / 1}
        , Edge {edge_fr = 0, edge_to = 2, eedge_c = 4 / 1, edge_f = 4 / 1}
        , Edge {edge_fr = 2, edge_to = 0, eedge_c = 4 / 1, edge_f = (-4) / 1}
        , Edge {edge_fr = 2, edge_to = 3, eedge_c = 5 / 1, edge_f = 0 / 1}
        , Edge {edge_fr = 1, edge_to = 3, eedge_c = 1 / 1, edge_f = 0 / 1}
        , Edge {edge_fr = 3, edge_to = 2, eedge_c = 5 / 1, edge_f = 0 / 1}
        , Edge {edge_fr = 3, edge_to = 1, eedge_c = 1 / 1, edge_f = 0 / 1}
        ]
    , network_s = 0
    , network_t = 3
    , network_Q = [[1], [], [], [], [2]]
    })


{- TEST: `liftVertex` -}
test_liftVertex = [test1_liftVertex, test2_liftVertex]

test1_liftVertex = 
    let 
       net@(Network vs es s t netQ) = goldbergInitialize net1
       outEdges = getOutEdges es 1
    in 
        liftVertex net 1 1 ==
        (Network
        { network_vs =
            [ Vertex
                { vertex_name = 0
                , vertex_h = 4
                , vertex_ex = 9/ 1
                , vertex_des = []
                }
            , Vertex
                { vertex_name = 1
                , vertex_h = 1
                , vertex_ex = 2/ 1
                , vertex_des = [(1, 2), (1, 3)]
                }
            , Vertex
                {vertex_name = 2, vertex_h = 0, vertex_ex = 4/ 1, vertex_des = []}
            , Vertex
                {vertex_name = 3, vertex_h = 0, vertex_ex = 0/ 1, vertex_des = []}
            ]
        , network_es =
            [ Edge {edge_fr = 0, edge_to = 1, eedge_c = 2/ 1, edge_f = 2/ 1}
            , Edge {edge_fr = 1, edge_to = 0, eedge_c = 2/ 1, edge_f = (-2)/ 1}
            , Edge {edge_fr = 0, edge_to = 2, eedge_c = 4/ 1, edge_f = 4/ 1}
            , Edge {edge_fr = 2, edge_to = 0, eedge_c = 4/ 1, edge_f = (-4)/ 1}
            , Edge {edge_fr = 1, edge_to = 2, eedge_c = 3/ 1, edge_f = 0/ 1}
            , Edge {edge_fr = 2, edge_to = 3, eedge_c = 5/ 1, edge_f = 0/ 1}
            , Edge {edge_fr = 1, edge_to = 3, eedge_c = 1/ 1, edge_f = 0/ 1}
            , Edge {edge_fr = 2, edge_to = 1, eedge_c = 3/ 1, edge_f = 0/ 1}
            , Edge {edge_fr = 3, edge_to = 2, eedge_c = 5/ 1, edge_f = 0/ 1}
            , Edge {edge_fr = 3, edge_to = 1, eedge_c = 1/ 1, edge_f = 0/ 1}
            ]
        , network_s = 0
        , network_t = 3
        , network_Q = [[2, 1], [], [], [], []]
        })

test2_liftVertex = 
    liftVertex (goldbergInitialize net1) 1 10 ==
    (Network
    { network_vs =
        [ Vertex
            { vertex_name = 0
            , vertex_h = 4
            , vertex_ex = 9 / 1
            , vertex_des = []
            }
        , Vertex
            { vertex_name = 1
            , vertex_h = 10
            , vertex_ex = 2 / 1
            , vertex_des = [(1, 0), (1, 2), (1, 3)]
            }
        , Vertex
            {vertex_name = 2, vertex_h = 0, vertex_ex = 4 / 1, vertex_des = []}
        , Vertex
            {vertex_name = 3, vertex_h = 0, vertex_ex = 0 / 1, vertex_des = []}
        ]
    , network_es =
        [ Edge {edge_fr = 0, edge_to = 1, eedge_c = 2 / 1, edge_f = 2 / 1}
        , Edge {edge_fr = 1, edge_to = 0, eedge_c = 2 / 1, edge_f = (-2) / 1}
        , Edge {edge_fr = 0, edge_to = 2, eedge_c = 4 / 1, edge_f = 4 / 1}
        , Edge {edge_fr = 2, edge_to = 0, eedge_c = 4 / 1, edge_f = (-4) / 1}
        , Edge {edge_fr = 1, edge_to = 2, eedge_c = 3 / 1, edge_f = 0 / 1}
        , Edge {edge_fr = 2, edge_to = 3, eedge_c = 5 / 1, edge_f = 0 / 1}
        , Edge {edge_fr = 1, edge_to = 3, eedge_c = 1 / 1, edge_f = 0 / 1}
        , Edge {edge_fr = 2, edge_to = 1, eedge_c = 3 / 1, edge_f = 0 / 1}
        , Edge {edge_fr = 3, edge_to = 2, eedge_c = 5 / 1, edge_f = 0 / 1}
        , Edge {edge_fr = 3, edge_to = 1, eedge_c = 1 / 1, edge_f = 0 / 1}
        ]
    , network_s = 0
    , network_t = 3
    , network_Q = [[2, 1], [], [], [], []]
    })


{- Test the whole algorithm on different flow networks. -}
test_goldberg = [test1_goldberg, test2_goldberg]
test1_goldberg = getNetworkFlow $ runGoldberg net1
test2_goldberg = getNetworkFlow $ runGoldberg net2
