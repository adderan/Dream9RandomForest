
source("select-features.R")
source("random-forest.R")
#selected.features <- select.features()
res <- run.random.forest()
write.gct(res, "output/varfilter99.gct")

	
	
