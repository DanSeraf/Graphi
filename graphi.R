#!/usr/bin/env Rscript
# Progetto totale Algoritmi e Strutture Dati

# Load existent data
if(file.exists("save.RData")) {
	load("save.RData")
}	

# import libraries
suppressPackageStartupMessages(library(igraph))
library("argparse")
library("igraph")
library("cgwtools")

# Import node class
source(paste(getwd(), "/node.R", sep=""))

# Graph class 
Graph = setRefClass("Graph", fields=list(n_nodes="numeric", adj_matrix="matrix", nodes_vector="vector"),

		    methods = list(
				   # Update .RData file
				   updateGraph = function() {
					resave(graph, file="save.RData")
				   	cat("[*] Data file updated\n")
				   },
					
				   # Getter for adjacency matrix
				   getAdjMatrix = function() {
				   	return(adj_matrix)
				   },
				   
				   # Show Matrix
				   showAdjMatrix = function() {
					cat("[ADJACENCY MATRIX]\n")
				   	print(adj_matrix)
				   },

				   # Adjust vector
				   adjustVector = function() {
				   	for(i in 1:length(nodes_vector)) {
						nodes_vector[[i]]$pos <<- i # Fix nodes vector positions
					}
				   	cat("[*] Nodes vector correctly updated\n")
				   },

				   # Check node, return value only if the node is present otherwise NULL
				   checkNode = function(node_name) {
					for(i in 1:length(nodes_vector)){
						if((nodes_vector[[i]]$name)==node_name){
							return(i)
							break
				   		} 
					} 
					return(NULL)
				   },
				   
				   # Add new bow
				   addBow = function(x, y, value) {
					nodeX = checkNode(x)
				   	nodeY = checkNode(y)
					if (is.null(nodeX) || is.null(nodeY)) {	
						cat("[WARN] No such node(s) in the matrix")
					} else {
						adj_matrix[x, y] <<- value
						cat("[*] Bow added\n")
						updateGraph()
						showAdjMatrix()
					}
				   },

				   # Remove bow
				   rmBow = function(x, y) {
					if (adj_matrix[x,y] != 0) {
				   		adj_matrix[x, y] <<- 0 # Set matrix position to 0 value 
				   		cat("[*] Bow removed\n")
				   		updateGraph()
						showAdjMatrix()
					} else {cat("[WARN] Bow not present")}
				   },
				   
				   # Remove node
				   rmNode = function(node_to_remove) {
				   	check_node = checkNode(node_to_remove)
				   	if(is.null(check_node)) { # Check if node is not present
						cat("[WARN] No such node in matrix")
					} else {
						if (nodes_vector[[check_node]]$pos == check_node) { # Check node position
							nodes_vector <<- nodes_vector[-check_node] # Delete Node object from vector
						}
						adj_matrix <<- adj_matrix[-check_node, -check_node] 
						cat("[*] Node removed\n")
						n_nodes <<- n_nodes - 1
						adjustVector()
						updateGraph()
						showAdjMatrix()
					}
				   },
				   
				   # Add node
				   addNode = function(node_to_fun, node_value) {
				   	check_node = checkNode(node_to_fun)
				   	if(!is.null(check_node)) {
						cat("[WARN] Node already present in matrix")
					} else {
						n_nodes <<- n_nodes + 1
						position <- n_nodes
						node <- Node$new(value=node_value, name=node_to_fun, pos=position) # Generate node object
						nodes_vector <<- c(nodes_vector, node) # Node vector update
						value <- c()
						# Create value list to store node name
						for (i in 1:n_nodes) {
							value[[i]] <- nodes_vector[[i]]$name
						}
						# Add empty field to the adj matrix
						adj_matrix <<- rbind(adj_matrix, 0)
						adj_matrix <<- cbind(adj_matrix, 0)
						# Add column and row names to the adj matrix
						colnames(adj_matrix) <<- value
						rownames(adj_matrix) <<- value
						cat("[*] Node correctly added\n")
						updateGraph()
						showAdjMatrix()
					}
				   },

				   getNodeValue = function(node_inserted) {
				   	check_node = checkNode(node_inserted)
				   	if(is.null(check_node)) {
						cat("[WARN] Node not present in matrix")
					} else {
						get_value <- nodes_vector[[check_node]]$value
						cat("[*] Value of node [", node_inserted ,"] -->", get_value)
					}
				   },

				   changeNodeValue = function(node, node_value) {
				   	check_node = checkNode(node)
				   	if(is.null(check_node)) {
						cat("[WARN] Node not present in matrix")
					} else {
						nodes_vector[[check_node]]$value <<- node_value
						cat("[*] Value of node correctly updated\n")
						updateGraph()
					}
				   },

				   verifyNodeConnected = function(node_inserted) {
					check_node <- checkNode(node_inserted)
				   	node_list <- c() # List that store all node(s) connected to node given in input
				   	for(i in 1:n_nodes) {
						if(adj_matrix[check_node, i] > 0) {
							node_list <- c(node_list, nodes_vector[[i]]$name)
						}
					}
					cat("[*] Node connected to node [", node_inserted, "] --> [", node_list, "]")
				   },
					
				   getBow = function(x, y) {
				   	value <- adj_matrix[x,y] # Take bow value from matrix
				   	if (value > 0) {
				   		cat("[*] Value of bow [", x , "] --", value , "--> [", y , "]")
					} else {cat("[*] No bow (0 value)")}
				   },
				   
				   getNode = function(node_inserted) {
				   	check_node <- checkNode(node_inserted)
				   	if(is.null(check_node)) {
						cat("Node [", node_inserted , "] not present")
					} else {cat("Node [", node_inserted , "] present")} 
				   },
					
				   getAdjacencyList = function() {
				   	cat("[ADJACENCY LIST]\n")
				   	for(i in 1:n_nodes) {
						nodes_list <- c()
						check_node <- checkNode(nodes_vector[[i]]$name)
						for(j in 1:n_nodes) {
							if(adj_matrix[check_node, j] > 0) {
								nodes_list <- c(nodes_list, nodes_vector[[j]]$name)
							}
						}
						if(length(nodes_list) != 0) {
							cat("[", nodes_vector[[i]]$name , "] --> [", nodes_list , "]\n")
						}
					}
				   	
				   }
				
				   
				)
)

# Save file check
check_save <- function() {
	if (file.exists("save.RData")) {
		cat("Save data present, do you want to delete the file [Y/n]: ")
		req <- readLines("stdin", n=1)
		if(req == "Y") {
			file.remove("save.RData")
			return(TRUE)
		} else {cat("[WARN] You must delete the save file if you want to work to another matrix")}
	} else {return(TRUE)} 
	
}

# Matrix creation function
create_matrix <- function(user_input) {
	cat("Write the numbers of vertices: ")
	user_input <- readLines("stdin", n=1)
	user_input <- as.numeric(user_input)
	print(user_input)
	if (user_input > 0) {
		# Initializating matrix
		init_matrix <- matrix(0, nrow=user_input, ncol=user_input)

		# Get node information / create node objects
		nodes_vect <- c()
		for (i in 1:user_input) {
			cat("Write the name of the node: ")
			node_name = readLines("stdin", n=1)
			cat("Write the value of the node: ")
			node_value = readLines("stdin", n=1)
			position = i
			node <- Node$new(value=node_value, name=node_name, pos=position)
			nodes_vect <- c(nodes_vect, node)	
		}
	
		# Create value list to store the name of node
		value <- c()
		for (i in 1:user_input) {
			value[[i]] <- nodes_vect[[i]]$name
		}

		# Insert letters in matrix
		rownames(init_matrix, do.NULL=TRUE, prefix="row")
		rownames(init_matrix) <- value
		colnames(init_matrix, do.NULL=TRUE, prefix="col")
		colnames(init_matrix) <- value
		cat("[MATRIX]\n")
		print(init_matrix)

		# Create graph object with user input
		graph <- Graph$new(n_nodes=user_input, adj_matrix=init_matrix, nodes_vector=nodes_vect)
		cat("[*] Saving data")
		save(graph, file="save.RData")
	}
	else {
		cat("[WARN] Number must be > 0")
	}
}

# Import matrix from CSV file
import_matrix <- function(path) {
	if(file.exists("save.RData")) {
		file.remove("save.RData")
	}
	imported_matrix <- read.csv(path)
	imported_matrix <- as.matrix(imported_matrix)
	rownames(imported_matrix) <- colnames(imported_matrix)
	names_list <- colnames(imported_matrix)
	nodes_number <- ncol(imported_matrix)	
	nodes_vect <- c()
	# Create node object
	for (i in 1:nodes_number) {
		cat("Set the value of node [", names_list[i], "]: ")
		node_value <- readLines("stdin", n=1)
		impnode <- Node$new(value=node_value, name=names_list[i], pos=i)
		nodes_vect <- c(nodes_vect, impnode) 
	}

	value <- c()
	for (i in 1:nodes_number) {
		value[[i]] <- nodes_vect[[i]]$name
	}
	
	print(imported_matrix)	
	graph <- Graph$new(n_nodes=nodes_number, adj_matrix=imported_matrix, nodes_vector=nodes_vect)
	cat("[*] Saving data")
	save(graph, file="save.RData")
}

# View current matrix function (CURRENTLY ONLY FOR DEBUG)
view_matrix <- function() {
	cat("[ADJACENCY MATRIX]\n")
	print(graph$getAdjMatrix())
}

# Search node function
search_node <- function(node_to_search) {
	graph$getNode(node_to_search)
}

# Bow set function
set_bow <- function(bow_to_set) {
	bow_to_set <- strsplit(bow_to_set, "") [[1]]
	x <- bow_to_set[1]
	y <- bow_to_set[2]
	store <- c()# Vector to store number
	for (i in 3:length(bow_to_set)) {
		store <- c(store, bow_to_set[i])
	}
	value <- as.numeric(paste(store, collapse=""))
	graph$addBow(x, y, value)
}

# Remove bow function
rm_bow <- function(bow_to_remove) {
	bow_to_remove <- strsplit(bow_to_remove, "") [[1]]
	if(length(bow_to_remove) == 2) {
		cat("[*] Removing bow\n")
		x <- bow_to_remove[1]
		y <- bow_to_remove[2]
		graph$rmBow(x, y)
	} else {cat("[WARN] Wrong insertion")}
}

# Plot graph function
plot_graph <- function() {
	app_matrix <- graph$getAdjMatrix()
	app_graph <- graph_from_adjacency_matrix(app_matrix, weighted=TRUE)
	plot(app_graph, layout=layout.fruchterman.reingold(app_graph))
	cat("[*] Graph correctly plotted to current working directory")
}

# Remove node function
remove_node <- function(node_to_remove) {
	if(length(node_to_remove) == 1) {
		graph$rmNode(node_to_remove)	
	} else {
		cat("[WARN] Wrong insertion")
	}
}

# Add node function
add_node <- function(node_to_add) {
	node_to_add <- strsplit(node_to_add, "") [[1]]
	node_to_fun <- node_to_add[1]
	store <- c()
	for(i in 2:length(node_to_add)) {
		store <- c(store, node_to_add[i])
	}
	node_value <- paste(store, collapse="")
	graph$addNode(node_to_fun, node_value)
}

# Node value getter
get_node_value <- function(node_inserted) {
	graph$getNodeValue(node_inserted)
}

# Update node value
change_node_value <- function(node_inserted) {
	node_inserted <- strsplit(node_inserted, "") [[1]]
	node <- node_inserted[1]
	store <- c()
	for(i in 2:length(node_inserted)) { 
		store <- c(store, node_inserted[i])
	}
	node_value <- paste(store, collapse="")
	graph$changeNodeValue(node, node_value)
	
}

# Node connected to node given in input
node_connection <- function(node_inserted) {
	if(length(node_inserted) == 1) {
		graph$verifyNodeConnected(node_inserted)	
	} 
}

# Get bow value from nodes input
get_bow <- function(nodes) {
	nodes <- strsplit(nodes, "") [[1]]
	if(length(nodes) == 2) {
		x <- nodes[1]
		y <- nodes[2]
		graph$getBow(x, y)
	} else {cat("[WARN] Wrong insertion")}
}

# Edge list creation
edge_list_creation <- function() {
	adj_matrix <- graph$getAdjMatrix()
	adj <- graph.adjacency(adj_matrix) # Convert the matrix to graph object
	get.edgelist(adj)

}

# Adjacency list creation
get_adjacency_list <- function() {
	graph$getAdjacencyList()
}

# main function 
main <- function() {
	# Parse object creation
	parser <- ArgumentParser(description="GRAPHI - Simple program to create adjacency matrix and plot it as graph")

	# Options
	parser$add_argument("-c", "--create", action="store_true", default=FALSE,
			    help="Create an adjacency matrix")
	parser$add_argument("-vm", "--view-matrix", action="store_true", default=FALSE,
			    help="Display matrix in use")
	parser$add_argument("-ed", "--edge-list", action="store_true", default=FALSE,
			    help="Display edgelist of graph")	
	parser$add_argument("-pg", "--plot-graph", action="store_true", default=FALSE,
			    help="Create a graph from the current adjacency matrix")
	parser$add_argument("-al", "--adjacency-list", action="store_true", default=FALSE,
			    help="Display adjacency list of graph")
	parser$add_argument("-i", "--import", type="character", 
			    help="Import adjacency matrix from CSV file")
	parser$add_argument("-sn", "--search-node", type="character", 
			    help="Search node in graph")
	parser$add_argument("-sb", "--set-bow", type="character",
			    help="Set bow between nodes (EXAMPLE -sb ab5)")
	parser$add_argument("-rb", "--remove-bow", type="character",
			    help="Remove bow between nodes (EXAMPLE -rb ab)")
	parser$add_argument("-rn", "--remove-node", type="character",
			    help="Remove node from the current adjacency matrix")
	parser$add_argument("-an", "--add-node", type="character",
			    help="Add node to the adjacency matrix")
	parser$add_argument("-nv", "--node-value", type="character", 
			    help="Display the value of the node inserted")
	parser$add_argument("-cv", "--change-value", type="character",
			    help="Change the value referred to node (EXAMPLE -cv a2)")
	parser$add_argument("-nc", "--node-connection", type="character",
			    help="Display node(s) connected to input node")
	parser$add_argument("-bv", "--bow-value", type="character", 
			    help="Display bow value (example -gb ab)")
	
	args <- parser$parse_args()
	
	# Verify presence of save file
	verify_data <- file.exists("save.RData")
	if (isTRUE(verify_data)) {
		cat("[*] Loading saved data\n")
	} else {
		cat("[*] Saved data not present in the current directory\n")
	}

	if(isTRUE(args$create)) { 
		check <- check_save()
		if(isTRUE(check)){
			cat("[*] Creating Matrix\n")
			create_matrix()
		} 
	}
	

	if(is.character(args$import)) {
			check <- check_save()
			if(isTRUE(check)) {
				cat("[*] Importing matrix\n")
				path <- as.character(args$import)
				import_matrix(path)
			}
	}
	
	if(isTRUE(args$view_matrix)) {
		if (isTRUE(verify_data)) {
			view_matrix()
		}
		else {
			cat("[WARN] You must create the matrix")
		}
	}

	if(is.character(args$search_node)) {
		node_to_search <- as.character(args$search_node)
		search_node(node_to_search)
	}
	
	if(is.character(args$set_bow)) {
		bow_to_set <- as.character(args$set_bow)
		set_bow(bow_to_set)
	}

	if(is.character(args$remove_bow)) {
		bow_to_remove <- as.character(args$remove_bow)
		rm_bow(bow_to_remove)
	}

	if(isTRUE(args$plot_graph)) {
		if(isTRUE(verify_data)) {
			plot_graph()
		} else {
			cat("[WARN] You must create the matrix")
		}
	}

	if(is.character(args$remove_node)) {
		if(isTRUE(verify_data)) {
			node_to_remove <- as.character(args$remove_node)
			remove_node(node_to_remove)
		}	    
	}

	if(is.character(args$add_node)) {
		if(isTRUE(verify_data)) {
			node_to_add <- as.character(args$add_node)
			add_node(node_to_add)
		}
	}

	if(is.character(args$node_value)) {
		if(isTRUE(verify_data)) {
			node_inserted <- as.character(args$node_value)
			get_node_value(node_inserted)
		}
	}

	if(is.character(args$change_value)) {
		if(isTRUE(verify_data)) {
			node_inserted <- as.character(args$change_value)
			change_node_value(node_inserted)
		}
	}

	if(is.character(args$node_connection)) {
		if(isTRUE(verify_data)) {
			node_inserted <- as.character(args$node_connection)
			node_connection(node_inserted)
		}
	}

	if(is.character(args$bow_value)) {
		if(isTRUE(verify_data)) {
			nodes <- as.character(args$bow_value)
			get_bow(nodes)
		}
	}

	if(isTRUE(args$edge_list)) {
		if(isTRUE(verify_data)) {
			edge_list_creation()
		}
	}

	if(isTRUE(args$adjacency_list)) {
		if(isTRUE(verify_data)) {
			get_adjacency_list()
		}
	}
}

if(!interactive()) {
	main()
}

