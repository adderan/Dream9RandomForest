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
	load("gene-neighbors.RData")

	max <- 3 
	n.genes <- dim(essentiality)[[1]]
	start <- length(neighbors)
	for(i in start:n.genes) {
		cat("Getting neighbors for: ", rownames(essentiality)[[i]], " ", i,"\n")
		Sys.sleep(0.5)
		neighbors[[i]] <- get.neighbors(rownames(essentiality)[i])
		if(i %% 10 == 0) {
			save(neighbors, file="gene-neighbors.RData")
		}
	}
	save(neighbors, file="gene-neighbors.RData")
}
read.biopax <- function() {
	biopax <- file("pathways/Pathway Commons.4.All.BIOPAX.owl")
	lines <- readLines(biopax, 100)
	return(lines)
}

