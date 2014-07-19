require(RCurl)
require(XML)
get.gene.ids <- function() {
	brca2 <- getURL("http://www.pathwaycommons.org/pc/webservice.do?version=3.0&cmd=get_neighbors&q=93298&output=binary_sif")
	brca2 <- readLines(tc <- textConnection(brca2)); close(tc)
	return(brca2)
}
get.neighbors <- function(gene) {

	baseurl <- "http://www.pathwaycommons.org/pc/webservice.do?version=3.0&cmd=get_neighbors&input_id_type=GENE_SYMBOL&output=binary_sif&q="
	fullurl <- paste(baseurl, gene, sep="")
	fullurl <- getURL(fullurl)
	interactions <- readLines(tc <- textConnection(fullurl)); close(tc)
	#interactions <- lapply(interactions, strsplit, split="\tINTERACTS_WITH\t")
	#interactions <- lapply(interactions, unlist)
	#print(interactions)
	neighbors <- c()
	neighbors.index <- 1
	for(i in 1:length(interactions)) {
		interaction.i <- unlist(strsplit(interactions[i], "\t"))
		if(length(interaction.i) != 3) break
		id1 <- interaction.i[[1]]
		id2 <- interaction.i[[3]]
		if(is.na(id1) || is.na(id2) || i > 100) {
			break
		}
		#cat("id 1 = ", id1, " id2 = ", id2, "\n")
		gene1 <- cpath.id.to.name(id1)
		gene2 <- cpath.id.to.name(id2)
		if(gene1 == gene) {
			neighbors[neighbors.index] <- gene2
			neighbors.index <- neighbors.index + 1
		}
		#else {
		#	neighbors[i] <- gene1
		#}
	}
		
	return(neighbors)
}
cpath.id.to.name <- function(id) {
	baseurl <- "http://www.pathwaycommons.org/pc/webservice.do?cmd=get_record_by_cpath_id&version=2.0&output=gsea&q="
	fullurl <- paste(baseurl, id, sep="")
	#xmldata <- xmlToList(xmlParse(fullurl))
	#gene.info <- strsplit(xmldata$protein$NAME$text, " ")[[1]]
	#gene.name <- xmldata$protein$XREF$relationshipXref$ID
	#return(gene.info[length(gene.info)])
	fullurl <- getURL(fullurl)
	gene.name <- readLines(tc <- textConnection(fullurl)); close(tc)
	gene.name <- strsplit(gene.name, "_")[[1]]
	return(gene.name[1])
}
get.all.neighbors <- function() {
	load("dream9.RData")
	neighbors <- list(c())
	n.genes <- dim(essentiality)[[1]]
	for(i in 1:5) {
		neighbors[i] <- get.neighbors(rownames(essentiality)[[i]])
	}
	save(neighbors, file = "neighbors_for_all_genes.RData")
}