{-
The main source file for highest-label push-relabel (Goldberg) algorithm.

Entry point function is `main`.

IMPLEMENTATION:
    The algorithm `goldberg(FlowNet net)` is divided into two separate phases:
        1) `initialize` - Intializes the heights and sends the first preflow (wave) through outcoming edges of the SOURCE (`s`) vertex.
        2) Until there are same ACTIVE nodes, do `processVertex(GoldNet net, Vertex v)`

DATA STRUCTURES:
    FlowNet - Flow network (G, s, t, c) where we use G as list of neighbours where each edge also holds capacity ().

-}
import Types

-- stack ghci --package pretty-simple ./main.hs
import Text.Pretty.Simple

{- Initializes the user provided network for the Goldberg algorithm -}
goldbergInitialize net@(N vs es s t) = foldr (flip push) (toComputeNetwork net) (getRawOutEdges es s)

getRawOutEdges :: [E] -> Int -> [(VertId, VertId)]
getRawOutEdges es from = map (\(E fr to _) -> (fr, to)) (filter (\(E fr to _) -> fr == from) es)

{- Converts user network to representation for the algorithm calculations -}
toComputeNetwork :: N -> Network
toComputeNetwork (N vs es s t) = Network vss ess s t (replicate (length vs + 1) [])
  where
    vss = map fv vs -- Edgetended vertices
    ess = map fe es  ++ map feRev es -- Convert edges and add their reverses
    fv v@(V name)
        | name == s = Vertex name (length vs) capSum (map (\(E fr to c) -> (fr, to)) (filter (\ (E fr _ _) -> fr == name) es)  )
        | name == t = Vertex name 0 0.0 []
        | otherwise = Vertex name 0 0.0 []
    
    fe (E fr to c) = Edge fr to c 0.0
    feRev (E fr to c) = Edge to fr c 0.0

    capSum = sum (map (\(E _ _ c) -> c) es) -- Sum of all edge capacities

{- Gets only normal vertices from the network -}
normVertices (Network vs es s t _) = filter (\(Vertex n h ex des) -> n /= s && n /= t) vs


-- goldbergStep net@(Network vs es s t netQ) vertId = if isNothing pushEdgeIds  -- If no pushable edge found
--     then push net pushEdgeIds -- We can push => push 
--     else liftVertex net vertId 1 -- Cannot push => lift
--         where
--             pushEdgeIds = getPushableEdgeIds


{- Pushes possible maximum through the provided edge -}
push :: Network -> (VertId, VertId) -> Network
push net@(Network vs es s t netQ) (fr, to) = pushedNet
  where
    pushedNet = Network (newFromVert : newToVert : restVerts) (newEdge : newEdgeRev : restEdges) s t newQ
    
    -- Amount to push 
    diff = min fromEx edgeRes
    
    -- Handle queue properly
    newQ = updateQueue netQ diff fromVert toVert s t
    
    -- Old vertices
    fromVert@(Vertex frId frH fromEx frDes) = findVertex vs fr
    toVert@(Vertex toId toH toEx toDes) = findVertex vs to
    restVerts = filter (\(Vertex name _ _ _) -> name /= fr && name /= to) vs
    
    -- New vertices 
    newFromVert = Vertex frId frH (fromEx - diff) frDes
    newToVert = Vertex toId toH (toEx + diff) toDes
    
    -- Old edges
    edgeRes = eCap - eFlow
    foundEdge@(Edge eFr eTo eCap eFlow) = findEdge es fr to
    restEdges = filter (\(Edge _fr _to _ _) -> not ( (_fr == fr || _fr == to) && (_to == fr || _to == to) ) ) es
    
    -- New edges
    newEdge = Edge eFr eTo eCap (eFlow + diff)
    newEdgeRev = Edge eTo eFr eCap (eFlow - diff)

{- Updates the bucket queue after the push based on provided arguments. -}
updateQueue :: [[VertId]] -> Rational -> Vertex -> Vertex -> VertId -> VertId -> [[VertId]]
updateQueue netQ diff vFrom@(Vertex frId frH frEx frDes) vTo@(Vertex toId toH toEx toDes) s t = newNetQ
  where
    cleanQueue = getCleanQueue netQ frId toId
    
    -- Decide if FROM vertex should belong to the Q (s,t excluded)
    fromVertexQueue = [frId | frEx - diff > 0 && frId /= s && frId /= t]
    -- Decide if TO vertex should belong to the Q (s,t excluded)
    toVertexQueue = [toId | toEx + diff > 0 && toId /= s && toId /= t]

    -- Apppend this list into the corresponding buckets
    newNetQ = appendToKthList (appendToKthList cleanQueue fromVertexQueue frH) toVertexQueue toH


{- Appends the provided list to the list at the height `h`. -}
appendToKthList list listToAppend h = result
    where
        -- Get sublist
        sublist = list !! h

        -- Update the sublist
        newSublist = sublist ++ listToAppend

        -- Insert updated item
        result = take h list ++ [newSublist] ++ drop (h + 1) list

{- Lifts the provided vertex and updates the network accordingly. -}
liftVertex :: Network -> VertId -> Int -> Network
liftVertex net@(Network vs es s t bucketQueue) verToLiftId diff = Network updatedVertices es s t bucketQueue
    where 
        -- Get vertices with updated height
        liftedVerts = [ newV | vert@(Vertex vId h ex des) <- vs , 
            let newV = if vId == verToLiftId 
                    then Vertex vId (h + diff) ex des -- Updated vertex
                    else vert ] -- Unchanged vertices

        -- Recompute also descending edges
        updatedVertices = recomputeDescLists liftedVerts es


recomputeDescLists vs es = map updateFn vs
    where
        updateFn (Vertex vId h ex des) = (Vertex vId h ex outDescEdges)
            where
                -- Outcoming edges
                outEdges = getOutEdges es vId

                -- Outcomming DESCENDING edges
                outDescEdges = filterOnlyDesc vs outEdges 0


{- Filteres out edges that are not descending after applying `diff` lift. -}
filterOnlyDesc :: [Vertex] -> [Edge] -> Int -> [(VertId, VertId)]
filterOnlyDesc vs outEdges diff = map (\ (Edge eFr eTo eC eF) -> (eFr, eTo)) (filter (
    \(Edge eFr eTo eC eF) -> 
        let 
            (Vertex _ hFr _ _)  = findVertex vs eFr
            (Vertex _ hTo _ _)  = findVertex vs eTo
        in
            hFr + diff > hTo
    ) outEdges)

{- Removes from and to vertices from active vertex queue. -}
getCleanQueue q frId toId = map (filter (\vId -> vId /= frId && vId /= toId)) q

{-- Gets edge with specified name -}
findVertex vs tarName = head $ filter (\(Vertex name _ _ _) -> name == tarName) vs

{-- Gets edge with specified from/to values -}
findEdge es fr to = head $ filter (\(Edge _fr _to _ _) -> _fr == fr && _to == to) es

{- Gets all OUTCOMING edges for the given vertex. -}
getOutEdges :: [Edge] -> VertId -> [Edge]
getOutEdges es vId = filter (\(Edge eFr _ _ _) -> eFr == vId) es

{- Gets all INCOMING edges for the given vertex. -}
getInEdges :: [Edge] -> VertId -> [Edge]
getInEdges es vId = filter (\(Edge _ eTo _ _) -> eTo == vId) es

{--------------------------------------------
 TESTS
---------------------------------------------}
-- Test network #1
net1_V = [V 0, V 1, V 2, V 3]
net1_E = [E 0 1 2.0, E 0 2 4.0, E 1 2 3.0, E 2 3 5.0, E 1 3 1.0]
net1 = N net1_V net1_E 0 3

-- Test Queue
t_q1 = [[1, 4, 5, 6], [], [3, 2]]

{- Runs all the tests. -}
testAll = do 
    let r1 = test_getCleanQueue
        ro = test_other
        r2 = test_goldbergInitialize
        r3 = test_push
        r4 = test_liftVertex
    
    putStrLn "Running all tests!"
    return (and r1 && and r2 && and r3 && and r4 && and ro)

{- TEST: `filterOnlyDesc` -}
test_other = [test_filterOnlyDesc, test_getOutEdges, test_getInEdges]

test_filterOnlyDesc = filterOnlyDesc vs outEdges 10 == [(1,0),(1,2),(1,3)]
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

test_initNet1 =
    goldbergInitialize net1 ==
    (Network
    { network_vs =
        [ Vertex
            { vertex_name = 0
            , vertex_h = 4
            , vertex_ex = 9 / 1
            , vertex_des = [(0, 1), (0, 2)]
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
            , vertex_des = [(0, 1), (0, 2)]
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
                , vertex_des = [(0, 1), (0, 2)]
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
            , vertex_des = [(0, 2)]
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