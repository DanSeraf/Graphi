# Queue implementation

Queue = setRefClass("Queue", fields = list(data="list"),
		    methods = list(
				   # Number of items in queue
				   len = function() {
				   	return(length(data))
				   },
				   
				   # Push method
				   push = function(item) {
				   	data[[len()+1]] <<- item
				   },

				   # Pop method
				   pop = function(n = 1) {
					if(n > len()) {
						cat("[WARN] Insufficent items in queue")
					} else {
						index <- seq_len(n)
						value <- data[index]
						data[index] <<- NULL
						if (length(value) == 0) value = NULL
						if (length(value) == 1) value = value[[1]]
						return(value)	
					}

				   },
				
				   poll = function() {
				   	if(size() == 0) return(NULL)
				   	else pop()
				   }
				   )
		    )
