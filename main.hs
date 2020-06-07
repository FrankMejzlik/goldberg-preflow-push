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
goldInitialze net@(N vs es s t) = foldr (flip push) (toEdgetendedNetwork net) (getOutEdges es s)

getOutEdges :: [E] -> Int -> [(VertId, VertId)]
getOutEdges es from = map (\(E fr to _) -> (fr, to)) (filter (\(E fr to _) -> fr == from) es)

{- Converts user network to representation for the algorithm calculations -}
toEdgetendedNetwork :: N -> Network
toEdgetendedNetwork (N vs es s t) = Network vss ess s t (replicate (length vs + 1) [])
  where
    vss = map fv vs -- Edgetended vertices
    ess = (map fe es)  ++ (map feRev es) -- Convert edges and add their reverses
    fv v@(V name)
        | name == s = Vertex name (length vs) capSum (map (\(E fr to c) -> (fr, to)) (filter (\ (E fr _ _) -> fr == name) es)  )
        | name == t = Vertex name 0 0.0 []
        | otherwise = Vertex name 0 0.0 []
    
    fe (E fr to c) = Edge fr to c 0.0
    feRev (E fr to c) = Edge to fr c 0.0

    capSum = sum (map (\(E _ _ c) -> c) es) -- Sum of all edge capacities

{- Gets only normal vertices from the network -}
normVertices (Network vs es s t _) = filter (\(Vertex n h ex des) -> n /= s && n /= t) vs

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
    
    fromVertexQueue = [frId | frEx - diff > 0 && frId /= s && frId /= t]
    toVertexQueue = [toId | toEx + diff > 0 && toId /= s && toId /= t]

    -- Apppend this list into the corresponding buckets
    newNetQ = appendToKthList (appendToKthList cleanQueue fromVertexQueue frH) toVertexQueue toH


appendToKthList list listToAppend h = result
    where
        sublist = list !! h
        newSublist = sublist ++ listToAppend
        result = take h list ++ [newSublist] ++ drop (h + 1) list


{- Removes from and to vertices from active vertex queue. -}
getCleanQueue q frId toId = map (filter (\vId -> vId /= frId && vId /= toId)) q

{-- Gets edge with specified name -}
findVertex vs tarName = head $ filter (\(Vertex name _ _ _) -> name == tarName) vs

{-- Gets edge with specified from/to values -}
findEdge es fr to = head $ filter (\(Edge _fr _to _ _) -> _fr == fr && _to == to) es

{--------------------------------------------
 TESTS
---------------------------------------------}
-- Test network #1
net1_V = [V 0, V 1, V 2, V 3]

net1_E = [E 0 1 2.0, E 0 2 4.0, E 1 2 3.0, E 2 3 5.0, E 1 3 1.0]


-- Test Queue
t_q1 = [[1, 4, 5, 6], [], [3, 2]]



-- Checkpoints
net1 = N net1_V net1_E 0 3




testAll = do 
    putStrLn "TEST: `getCleanQueue`:"
    r1 <- return test_getCleanQueue
    putStrLn ""
    
    putStrLn "TEST: `goldInitialze`:"
    r2 <- return test_goldInitialze
    putStrLn ""
    
    putStrLn "TEST: `push`:"
    r3 <- return test_push
    putStrLn ""

    return (and r1 && and r2 && and r3)

{- TEST: `getCleanQueue` -}
test_getCleanQueue = [test1_q1, test2_q1, test3_q1]

test1_q1 = getCleanQueue t_q1 3 1 == [[4, 5, 6], [], [2]]

test2_q1 = getCleanQueue t_q1 6 2 == [[1, 4, 5], [], [3]]

test3_q1 = getCleanQueue t_q1 33 33 == [[1, 4, 5, 6], [], [3, 2]]


{- TEST: `goldInitialze` -}
test_goldInitialze = [test_initNet1]

test_initNet1 =
    goldInitialze net1 ==
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
    push (goldInitialze net1) (1, 2) ==
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