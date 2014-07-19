require(RCurl)
#require(XML)

get.neighbors <- function(gene) {
	baseurl <- "http://www.pathwaycommons.org/pc/webservice.do?version=3.0&cmd=get_neighbors&input_id_type=GENE_SYMBOL&output=id_list&q="
	fullurl <- paste(baseurl, gene, sep="")
	fullurl <- getURL(fullurl)
	interactions <- readLines(tc <- textConnection(fullurl)); close(tc)
	interactions <- strsplit(interactions, "\t")
	interactions <- lapply(interactions, head, n=1)
	interactions <- lapply(interactions, strsplit, split="_")
	interactions <- lapply(interactions, unlist)
	interactions <- lapply(interactions, head, n=1)
	#interactions <- interactions[[2:length(interactions)]]
	#interactons <- interactions[-1]
	return(interactions)
}
		
get.all.neighbors <- function() {
	load("dream9.RData")
	max <- 3
	neighbors <- list(list())
	n.genes <- dim(essentiality)[[1]]
	for(i in 1:n.genes) {
		Sys.sleep(1)
		neighbors[[i]] <- get.neighbors(rownames(essentiality)[i])
	}
	save(neighbors, file="gene-neighbors.RData")
}

