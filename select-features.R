#library(Biobase)

select.features <- function() {
	load("../dream9/dream9.RData")
	
	n.samples <- dim(essentiality)[[2]]
	n.test.samples <- dim(expression.test)[[2]]
	n.essentiality.genes <- dim(essentiality)[[1]]
	n.expression.genes <- dim(expression)[[1]]
	n.copynumber.genes <- dim(copy.number)[[1]]
	
	
	expression.features <- top.variance(expression)
	copynumber.features <- top.variance(copy.number)
	
	n.selected.expression.genes <- length(expression.features)
	n.selected.copynumber.genes <- length(copynumber.features)
	
	selected.expression <- matrix(0, n.selected.expression.genes, n.samples)
	selected.copynumber <- matrix(0, n.selected.copynumber.genes, n.samples)
	
	selected.expression.test <- matrix(0, n.selected.expression.genes, n.test.samples)
	selected.copynumber.test <- matrix(0, n.selected.copynumber.genes, n.test.samples)
	
	for(i in 1:n.selected.expression.genes) {
		selected.expression[i,] <- expression[expression.features[i],]
		selected.expression.test[i,] <- expression.test[expression.features[i],]
	}
	for(i in 1:n.selected.copynumber.genes) {
		selected.copynumber[i,] <- copy.number[copynumber.features[i],]
		selected.copynumber.test[i,] <- copy.number.test[copynumber.features[i],]
	}
	selected.expression <- normalize.data.matrix(selected.expression)
	selected.expression.test <- normalize.data.matrix(selected.expression.test)
	selected.copynumber <- normalize.data.matrix(selected.copynumber)
	selected.copynumber.test <- normalize.data.matrix(selected.copynumber.test)
	
	training.data <- rbind(selected.expression, selected.copynumber)
	test.data <- rbind(selected.expression.test, selected.copynumber.test)
	
	return(list(training.data = training.data, test.data = test.data))
}
normalize.data.matrix <- function(data) {
	n.samples <- dim(data)[[2]]
	for(s in 1:n.samples) {
		norm <- max(data[,s])
		data[,s] <- data[,s]/norm
	}
	return(data)
}
top.variance.from.file <- function(data) {
	#expset <- new("ExpressionSet", exprs = data)
	#percent <- 0.95
	#filtered <- varFilter(expset, var.func=IQR, var.cutoff=percent, filterByQuantile=TRUE)

	filtered <- read.table("phase1.variance_filtered.top5percent.txt", sep = "\t")
	filtered.data <- filtered[2:46,]
	filtered.labels <- filtered[1,]
	#cat("length of data:", dim(filtered.data), "\n")
	#cat("length of labels:", length(filtered.labels), "\n")
	
	#filtered.features <- as.matrix(filtered.data)
	#colnames(filtered.features) <- filtered.labels
	#print(filtered.labels)
	
	#cat(dim(filtered.features))
	return(filtered.labels)
}

top.variance <- function(data) {
	cutoff <- 0.99
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
			
			
	
