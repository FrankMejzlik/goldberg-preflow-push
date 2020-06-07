
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


-- Test network #1
net1_V = [V 0, V 1, V 2, V 3]
net1_E = [
        E   0 1   2.0, 
        E   0 2   4.0, 

        E   1 2   3.0, 

        E   2 3   5.0, 
        E   1 3   1.0
    ]

-- Checkpoints
net1 = N net1_V net1_E 0 3
net1_postInitialize = push (toEdgetendedNetwork net1) 

goldInitialze net@(N vs es s t) = foldr (flip push) (toEdgetendedNetwork net) (getOutEdges es s)

getOutEdges :: [E] -> Int -> [(VertId, VertId)]
getOutEdges es from =  map (\(E fr to _ ) -> (fr, to))  (filter (\(E fr to _ ) -> fr == from) es)


{- Converts user network to representation for the algorithm calculations -}
toEdgetendedNetwork :: N -> Network
toEdgetendedNetwork (N vs es s t) = Network vss ess s t (take ((length vs) + 1) (repeat []))
    where 
        vss = map fv vs     -- Edgetended vertices
        ess = map fe es     -- Edgetended edges

        fv v@(V name)
            | name == s    = Vertex name (length vs) capSum (map (\(E fr to c) -> (fr, to)) es)
            | name == t    = Vertex name 0 0.0 []
            | otherwise = Vertex name 0 0.0 []

        fe (E fr to c) = Edge fr to c 0.0             

        capSum = sum (map (\(E _ _ c) -> c) es) -- Sum of all edge capacities

{- Gets only normal vertices from the network -}    
normVertices (Network vs es s t _) = filter (\ (Vertex n h ex des) -> n /= s && n /= t) vs

{- Pushes possible maximum through the provided edge -}
push :: Network -> (VertId, VertId) -> Network
push net@(Network vs es s t netQ) (fr, to) = pushedNet
    where
        pushedNet = Network (newFromVert : newToVert : restVerts) (newEdge : restEdges) s t newQ

        -- Amount to push
        diff = min fromEx edgeRes

        -- Handle queue properly
        newQ = updateQueue netQ diff fromVert toVert

        -- Old vertices
        fromVert@(Vertex frName frH fromEx frDes) = findVertex vs fr
        toVert@(Vertex toName toH toEx toDes) = findVertex vs to
        restVerts = filter (\(Vertex name _ _ _) -> name /= fr && name /= to) vs

        -- New vertices
        newFromVert = Vertex frName frH (fromEx - diff) frDes
        newToVert = Vertex toName toH (toEx + diff) toDes

        -- Old edges
        edgeRes = eCap - eFlow
        foundEdge@(Edge eFr eTo eCap eFlow) = findEdge es fr to
        restEdges = findEdgeComplement es fr to

        -- New edges
        newEdge = Edge eFr eTo eCap (eFlow + diff)

{- Updates the bucket queue after the push based on provided arguments. -}
updateQueue :: [[VertId]] -> Double -> Vertex -> Vertex -> [[VertId]]
updateQueue netQ diff (Vertex frName frH frEx frDes) (Vertex toName toH toEx toDes) = netQ

{-- Gets edge with specified name -}
findVertex vs tarName = head $ filter (\(Vertex name _ _ _) -> name == tarName) vs

{-- Gets edge with specified from/to values -}
findEdge es fr to = head $ filter (\(Edge _fr _to _ _) -> _fr == fr && _to == to) es
findEdgeComplement es fr to = filter (\(Edge _fr _to _ _) -> _fr /= fr || _to /= to) es

myLen [] = 0
myLen (x : xs) = myLen xs + 1

myLen2 xs = foldr (\x acc -> acc + 1) 0 xs