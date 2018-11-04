# *Graphi*

## Description

Simple script written in R to create adjacency matrix and print it as a graph. It 
uses the argparse library to get command line arguments.

## Usage

You can create the matrix and use it to generate a graph. 
You can do operation to change the matrix information directly from your console.
The graph will be printed in a pdf file.

To see the commands type `./graphi.R --help`:

```
usage: ./graphi.R [-h] [-c] [-vm] [-el] [-pg] [-al] [-i IMPORT]
                  [-sn SEARCH_NODE] [-sb SET_BOW] [-rb REMOVE_BOW]
                  [-rn REMOVE_NODE] [-an ADD_NODE] [-nv NODE_VALUE]
                  [-cv CHANGE_VALUE] [-nc NODE_CONNECTION] [-bv BOW_VALUE]
                  [-bfs BREADTH_FIRST_SEARCH] [-da DIJKSTRA_ALGORITHM] [-ka]

GRAPHI - Simple program to create graph by adjacency matrix

optional arguments:
  -h, --help            show this help message and exit
  -c, --create          Create an adjacency matrix
  -vm, --view-matrix    Display matrix in use
  -el, --edge-list      Display edgelist of graph
  -pg, --plot-graph     Create a graph from the current adjacency matrix
  -al, --adjacency-list
                        Display adjacency list of graph
  -i IMPORT, --import IMPORT
                        Import adjacency matrix from CSV file
  -sn SEARCH_NODE, --search-node SEARCH_NODE
                        Search node in graph
  -sb SET_BOW, --set-bow SET_BOW
                        Set bow between nodes (-sb
                        [STR_NODE][END_NODE][VALUE])
  -rb REMOVE_BOW, --remove-bow REMOVE_BOW
                        Remove bow between nodes (-rb [STR_NODE][END_NODE])
  -rn REMOVE_NODE, --remove-node REMOVE_NODE
                        Remove node from the current adjacency matrix
  -an ADD_NODE, --add-node ADD_NODE
                        Add node to the adjacency matrix
  -nv NODE_VALUE, --node-value NODE_VALUE
                        Display the value of the node inserted
  -cv CHANGE_VALUE, --change-value CHANGE_VALUE
                        Change the value referred to node (-cv [NODE][VALUE])
  -nc NODE_CONNECTION, --node-connection NODE_CONNECTION
                        Display node(s) connected to input node
  -bv BOW_VALUE, --bow-value BOW_VALUE
                        Display bow value (-gb [START_NODE][ENDNODE])
  -bfs BREADTH_FIRST_SEARCH, --breadth-first-search BREADTH_FIRST_SEARCH
                        Display Breadth First Search algorithm (-bs
                        [STR_NODE])
  -da DIJKSTRA_ALGORITHM, --dijkstra-algorithm DIJKSTRA_ALGORITHM
                        Display Dijkstra algorithm (-da [STR_NODE])
  -ka, --kruskal-algorithm
                        Display kruskal algorithm

```
## Library

You need these libraries:

*	`library("igraph")` - to create graph from the adjacency matrix
*	`library("argparse")` - to parse command line arguments
*	`library("cgwtools")` - to save the environment

