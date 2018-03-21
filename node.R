#!/usr/bin/env Rscript

# Creation of the Node class to store node
Node <- setRefClass("Node",
		    fields = list(value="character", name="character", pos="numeric", checked="character")
		    )
