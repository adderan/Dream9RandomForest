
run.random.forest <- function(data, essentiality, test.data, test.essentiality) {
	library(randomForest)
	source("select-features.R")

	data <- select.features(data)


	n.genes <- dim(essentiality)[[1]]
	samples <- dim(essentiality)[[2]]

	results <- matrix(0, n.genes, samples)

	for(i in 1:n.genes) {
		rf.for.gene <- randomForest(data, essentiality, ntree=500)
		gene.predictions <- predict(rf, test.data)
		gene.name <- colnames(essentiality)[[i]]
		colnames(results)[i] <- gene.name
		results[i, ] <- gene.predictions
	}

}

write.gct <- function(results) {

}

	






