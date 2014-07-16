
run.random.forest <- function() {
	library(randomForest)
	source("select-features.R")
	load("../dream9/dream9.RData")
	

	data <- select.features()
	#print(data)

	n.genes <- dim(essentiality)[[1]]
	n.samples <- dim(essentiality)[[2]]
	n.test.samples <- dim(data$test.data)[[2]]

	results <- matrix(0, n.genes, n.test.samples)

	
	cat("Dimension of data: ", dim(data$training.data), "\n")
	cat("Dimension of essentiality: ", dim(essentiality), "\n")
	
	
	training.data <- data.frame(t(data$training.data))
	test.data <- data.frame(t(data$test.data))
	training.essentiality <- data.frame(t(essentiality))

	#cat("Lenth of training data: ", length(training.data), "\n")
	#cat("Length of test data: ", dim(test.data), "\n")
	#cat("Dimension of training.essentialities:", dim(training.essentialities), "\n")
	
	cat("dimension of essentiality[1]", dim(training.essentiality[1]), "\n")
	for(i in 1:n.genes) {
		#cat("Processing gene ", i, "\n")
		essentiality.i <- training.essentiality[i][,1]
		cat("class of essentiality.i: ", class(essentiality.i), "\n")
		cat("length of essentiality.i:", length(essentiality.i), "\n")
		cat("length of training.data:", length(training.data), "\n")

		rf.for.gene <- randomForest(x=training.data, y=essentiality.i, ntree=200)
		gene.predictions <- predict(rf.for.gene, test.data)
		#gene.name <- colnames(essentiality)[[i]]
		#colnames(results)[i] <- gene.name
		results[i, ] <- gene.predictions
	}
	cat("Dimension of results: ", dim(results), "\n")
	rownames(results) <- rownames(essentiality)
	colnames(results) <- colnames(expression.test)
	return(results)

}

write.gct <- function(results, filename) {
	n.samples <- dim(results)[[2]]
	n.genes <- dim(results)[[1]]
	sink(filename)
	cat("#1.2\n")
	cat(n.genes, "\t", n.samples, "\n")
	cat("Name", "\t", "Description")
	for(i in 1:n.samples) {
		cat("\t", colnames(results)[i])
	}
	cat("\n")
	for(i in 1:n.genes) {
		cat(rownames(results)[i], "\t", rownames(results)[i])
		for(j in 1:n.samples) {
			cat("\t", results[i,j])
		}
		cat("\n")
	}
	sink()
	
}

	






