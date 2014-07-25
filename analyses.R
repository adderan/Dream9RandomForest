

pathway.commons.only <- function() {
	source("select-features.R")
	load("dream9.RData")

	redo1 <- FALSE
	redo2 <- FALSE
	
	if(file.exists("intermediate/neighbors-from-expression.RData")) {
		load("intermediate/neighbors-from-expression.RData")
	}
	if(redo1) {
		neighbors.from.expression <- select.neighbors.from.expression()
		save(neighbors.from.expression, file="intermediate/neighbors-from-expression.RData")
	}
	
	cat("length of neighbors: ", length(neighbors.from.expression), "\n")

	if(file.exists("intermediate/augmented-expression-features.RData") && !redo2) {
		load("intermediate/augmented-expression-features.RData")
	}
	else {
		expression.features <- augment.selected.features(neighbors.from.expression, expression, 10)
		save(expression.features, file="intermediate/augmented-expression-features.RData")
	}
	return(expression.features)
		
	
	#copynumber.base.features <- list(list())
	#copynumber.features <- augment.selected.features(copynumber.base.features, copy.number, 10)

	#data <- select.data(expression.features, copynumber.features)

	#results <- run.random.forest(data)

	#validate.results(results, expression.features, copynumber.features, 10)
	#write.gct(results, "output/pathway-commons-only-10-features.gct")
}

validate.results <- function(results, expression.features, copynumber.features, n.features) {
	n.exp <- length(expression.features)
	for(i in 1:n.exp) {
		stopifnot(length(expression.features[[i]]))
	}
}



