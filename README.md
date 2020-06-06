# GoldbergHaskell
Haskell implementation of the Goldberg push-relabel maximum flow algorithm with always select the highest node strategy.

# Overview
It's an algorithm returning maximum flow for the given flow network `F = (V, E, s, t, c)`, where `V` is set of graph vertices, `E` are edges, `s` is special **source** vertex, `t` is a special **terminal** vertex and `c` is **capacity function** that for each edge assigns its capacity. This algorithm is thoroughly described in the book [Průvodce labyrintem algoritmů](http://pruvodce.ucw.cz/) in the chapter "Goldbergův algoritmus" by *Martin Mareš* and *Tomáš Valla*.

Our implementation will take an approach called **highest-label** where in each iteration, whenever we take the next active node, we always choose the one with the **highest label** (also called height). We achieve this by storing a list of "buckets", one for each height and we will assign nodes in their corresponding buckets.

# How to build

# How to use (User documentation)

# Testing data
There are multiple predefined graphs prepared for testing. Functions that use them to test the main `goldberg(FlowNet net)` function are:
- `test1` 
- `test2` 
- `test3` 

# Developer documentation
The main function that takes a flow network as input and produces the maximum flow (in form of list `[(Edge e, Double flow)]` is `goldberg(FlowNet net)`.


For more information, please use comments directly inside the source files.