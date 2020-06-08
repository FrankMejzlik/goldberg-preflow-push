#!/usr/bin/python

import sys
import json
import os
import glob

file_cnter = 0
path = "./networks"
out_filepath = "../testingNetworks.hs"

result_file_string = """
module TestingNetworks where
{- 
    THIS IS GENERATED FILE! 
    To regenerate, please use the file `testing_data/JSON_networks_to_Haskell_module.py`
    that generates this file based on JSON network definitions in `testing_data/networks` directory. 
-}

-- My modules
import Types 


"""

finalListContent = ""

for filename in glob.glob(os.path.join(path, '*.json')): #only process .JSON files in folder.      
    with open(filename, encoding='utf-8', mode='r') as ifs:
        file_cnter += 1
        data = json.load(ifs)

        name = "net" + str(file_cnter)

        result_file_string += name + "Name = \"" + name + "\"\n"

        # Vertices
        source = 0
        terminus = 0

        result_file_string += name + "V = ["

        for i, pos in data["vl"].items():
            result_file_string += f"V {i},"
            terminus = i


        result_file_string = result_file_string[:-1]
        result_file_string += "] \n"

        # Edges
        result_file_string += name + "E = ["

        for i, pos in data["el"].items():
            result_file_string += f'E {pos["u"]} {pos["v"]} {pos["w"]} 0.0,'

        result_file_string = result_file_string[:-1]
        result_file_string += "] \n"

        # Graph
        result_file_string += name + " = N " + name + "V " + name + "E " + str(source) + " " + str(terminus) + " \n"

        #Max flow
        result_file_string += name + "MaxFlow = " + str(data["maxFlow"]) + "::Rational \n"

        # List of it
        finalListContent += f"({name}Name, {name}, {name}MaxFlow),"


## Add final list

finalListContent = finalListContent[:-1]
result_file_string += "allTestNets  = [" + finalListContent + "]\n"

with open (out_filepath, encoding='utf-8', mode='w') as ofs:
    ofs.write(result_file_string)

print(str(file_cnter) + " network files generated to '" +out_filepath + "'")

sys.exit(0)
