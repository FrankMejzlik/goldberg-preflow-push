
module TestingNetworks where
{- 
    THIS IS GENERATED FILE! 
    To regenerate, please use the file `testing_data/JSON_networks_to_Haskell_module.py`
    that generates this file based on JSON network definitions in `testing_data/networks` directory. 
-}

-- My modules
import Types 


net1Name = "net1"
net1V = [V 0,V 1,V 2,V 3] 
net1E = [E 0 2 4 0.0,E 0 1 2 0.0,E 1 2 3 0.0,E 1 3 1 0.0,E 2 3 5 0.0] 
net1 = N net1V net1E 0 3 
net1MaxFlow = 6::Rational 
net2Name = "net2"
net2V = [V 0,V 1,V 2,V 3,V 4,V 5] 
net2E = [E 0 1 16 0.0,E 0 2 13 0.0,E 1 2 4 0.0,E 1 3 12 0.0,E 2 3 9 0.0,E 2 4 14 0.0,E 4 3 7 0.0,E 4 5 4 0.0,E 3 5 20 0.0] 
net2 = N net2V net2E 0 5 
net2MaxFlow = 24::Rational 
net3Name = "net3"
net3V = [V 0,V 1,V 2,V 3,V 4,V 5] 
net3E = [E 0 1 16 0.0,E 0 2 13 0.0,E 1 2 4 0.0,E 1 3 12 0.0,E 2 4 14 0.0,E 4 3 7 0.0,E 4 5 4 0.0,E 3 5 20 0.0,E 3 2 9 0.0] 
net3 = N net3V net3E 0 5 
net3MaxFlow = 23::Rational 
allTestNets  = [(net1Name, net1, net1MaxFlow),(net2Name, net2, net2MaxFlow),(net3Name, net3, net3MaxFlow)]
