#library(Biobase)

select.features <- function(expression.features, copynumber.features) {
	load("dream9.RData")
	
	n.samples <- dim(essentiality)[[2]]
	n.test.samples <- dim(expression.test)[[2]]
	n.essentiality.genes <- dim(essentiality)[[1]]
	n.expression.genes <- dim(expression)[[1]]
	n.copynumber.genes <- dim(copy.number)[[1]]
	
	selected.expression <- make.normalized.data.matrix(expression, expression.features)
	selected.copynumber <- make.normalized.data.matrix(copy.number, copynumber.features)
	selected.expression.test <- make.normalized.data.matrix(expression.test, expression.features)
	selected.copynumber.test <- make.normalized.data.matrix(copy.number.test, copynumber.features)
	
	training.data <- rbind(selected.expression, selected.copynumber)
	test.data <- rbind(selected.expression.test, selected.copynumber.test)
	
	return(list(training.data = training.data, test.data = test.data, expression = selected.expression))
}
make.normalized.data.matrix <- function(data, features) {
	n.samples <- dim(data)[[2]]
	n.features <- length(features)
	selected.data <- matrix(0, n.features, n.samples)
	for(i in 1:n.features) {
		feature.i <- features[i]
		selected.data[i,] <- data[feature.i,]
	}
	selected.data <- normalize.data.matrix(selected.data)
	return(selected.data)
}

normalize.data.matrix <- function(data) {
	n.samples <- dim(data)[[2]]
	for(s in 1:n.samples) {
		norm <- max(data[,s])
		data[,s] <- data[,s]/norm
	}
	return(data)
}
top.variance <- function(data, cutoff) {
	n.test.cases <- 1000
	allowable <- (1 - cutoff)*n.test.cases
	
	selected.features <- c()
	variances <- c()
	n.features <- dim(data)[[1]]
	n.samples <- dim(data)[[2]]
	feature.index <- 1
	for(i in 1:n.features) {
		mean.for.feature <- mean(data[i,])
		variances[i] <- mean((data[i,] - mean.for.feature)^2)
	}
	test.cases <- sort(variances[1:n.test.cases], decreasing=TRUE)
	cutoff.value <- test.cases[allowable]
	print(test.cases)
	for(i in 1:length(variances)) {
		variance.i <- variances[i]
		if(variance.i > cutoff.value) {
			selected.features[feature.index] <- rownames(data)[i]
			feature.index <- feature.index + 1
		}
		
	}
	return(selected.features)
}
select.neighbors.from.expression <- function() {
	load("gene-neighbors.RData")
	load("dream9.RData")
	n.neighbors <- length(neighbors)

	feature.names <- list()
	for(i in 1:10) {
		feature.names.i <- list()

		neighbors.for.gene <- neighbors[[i]]
		index <- 1

		cat("length of neighbors = ", length(neighbors.for.gene), "\n")
		for(j in 1:length(neighbors.for.gene)) {
			n <- neighbors.for.gene[[j]]
			#cat("class of n: ", class(n), "\n")
			#cat("class of n[[1]]: ", class(n[[1]]), "\n")
			if(is.null(n)) break
			if(n %in% rownames(expression)) {
				#print("found match")
				feature.names.i[[index]] <- n
				index <- index + 1
			}
		}
		feature.names[[i]] <- feature.names.i
	}
	return(feature.names)
}

#select expression and copynumber features based on neighbors in pathway commons. If there aren't enough neighbors, take features with most variance. 
select.features.pathway.commons <- function(n.exp, n.copy) {
	load("dream9.RData")
	expression.features <- select.neighbors.from.expression()
	expression.top.variance <- top.variance(expression, 0.99)
	n.genes <- dim(essentiality)[[1]]
	n.samples <- dim(essentiality)[[2]]
	for(i in 1:n.genes) {
		n.neighbors <- length(expression.features[[i]])
		start.index <- n.neighbors + 1

		index <- 1
		for(j in start.index:n.exp) {
			expression.features[[i]][[j]] <- expression.top.variance[[i]][[index]]
			index <- index + 1
		}
	}

			



	

			
			
	
