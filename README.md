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
usage: ./graphi.R [-h] [-c] [-vm] [-ed] [-pg] [-i IMPORT] [-sn SEARCH_NODE]
                  [-sb SET_BOW] [-rb REMOVE_BOW] [-rn REMOVE_NODE]
                  [-an ADD_NODE] [-nv NODE_VALUE] [-cv CHANGE_VALUE]
                  [-nc NODE_CONNECTION] [-bv BOW_VALUE]

GRAPHI - Simple program to create adjacency matrix and plot it as graph

optional arguments:
  -h, --help            show this help message and exit
  -c, --create          Create an adjacency matrix
  -vm, --view-matrix    Display matrix in use
  -ed, --edge-list      Display edgelist of adjacency matrix
  -pg, --plot-graph     Create a graph from the current adjacency matrix
  -i IMPORT, --import IMPORT
                        Import adjacency matrix from CSV file
  -sn SEARCH_NODE, --search-node SEARCH_NODE
                        Search node in the graph
  -sb SET_BOW, --set-bow SET_BOW
                        Set bow between nodes (EXAMPLE -sb ab5)
  -rb REMOVE_BOW, --remove-bow REMOVE_BOW
                        Remove bow between nodes (EXAMPLE -rb ab)
  -rn REMOVE_NODE, --remove-node REMOVE_NODE
                        Remove node from the current adjacency matrix
  -an ADD_NODE, --add-node ADD_NODE
                        Add node to the adjacency matrix
  -nv NODE_VALUE, --node-value NODE_VALUE
                        Display the value of the node inserted
  -cv CHANGE_VALUE, --change-value CHANGE_VALUE
                        Change the value referred to node (EXAMPLE -cv a2)
  -nc NODE_CONNECTION, --node-connection NODE_CONNECTION
                        Display node(s) connected to input node
  -bv BOW_VALUE, --bow-value BOW_VALUE
                        Display bow value (example -gb ab)

```
## Library

You need these libraries:

*	`library("igraph")` - to create graph from the adjacency matrix
*	`library("argparse")` - to parse command line arguments
*	`library("cgwtools")` - to save the environment

