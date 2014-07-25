

cluster.essentialities <- function(n, iters) {
	load("dream9.RData")
	c <- kmeans(essentiality, centers=n, iter=iters)

	classes <- list()
	for(i in 1:n) {
		incluster <- which(c$cluster == i)
		incluster <- names(incluster)
		classes[[i]] <- incluster
	}

	return(classes)
}

