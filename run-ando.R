load("ccle.RData")
load("achilles.RData")
source("../multitask/multitask.R")
n.ccle.samples <- dim(ccle.expression)[[2]]
n.ccle.expression <- dim(ccle.expression)[[1]]
top25 <- read.table("data/top25per.features.lst")
sigGenes <- read.table("data/sigGenes.Zach2014.corrected.lst")
top25 <- top25[3:dim(top25)[[1]],1]


reduce.features <- function(featurenames1, featurenames2, data) {
	n <- length(featurenames1)
	m <- dim(data)[[2]]
	rnames <- list()
	for(i in 1:n) {
		if((featurenames1[[i]] %in% rownames(data)) && (featurenames1[[i]] %in% featurenames2)) {
			print("found feature")
			rnames[[length(rnames) + 1]] <- featurenames1[[i]]
			
		}
	}
	reduced.features <- matrix(0, length(rnames), m)
	rownames(reduced.features) <- rnames
	print(rnames)
	for(i in 1:length(rnames)) {
		name <- rnames[[i]]
		reduced.features[name, ] <- data[name, ]
	}
	colnames(reduced.features) <- colnames(data)
	return(reduced.features)
}


make.response.matrix <- function() {
	drug.names <- drug.profiles[,1]
	sample.names <- colnames(ccle.expression)
	n.points <- dim(drug.response)[[1]]
	n.drugs <- length(drug.names)

	response.matrix <- matrix(0, n.ccle.samples, n.drugs)
	
	colnames(response.matrix) <- drug.names
	rownames(response.matrix) <- sample.names
	for(i in 1:n.points) {
		drugname <- drug.response[i, "Compound"]
		samplename <- drug.response[i, "CCLE.Cell.Line.Name"]
		#print(drugname)
		#print(samplename)
		#cat("drug name: ", drugname, " sample name: ", samplename, "\n")
		response.matrix[samplename, drugname] <- drug.response[i, "Amax"]
	}
	return(response.matrix)
	
}

run.ando <- function() {
	drug.response.matrix <- make.response.matrix()
	ccle.expression.names <- rownames(ccle.expression)
	print(ccle.expression.names)

	achilles.expression.reduced <- reduce.features(top25, ccle.expression.names, expression.altnames)
	ccle.expression.reduced <- reduce.features(top25, ccle.expression.names, ccle.expression)


	achilles.expression.reduced.leaderboard <- reduce.features(top25, ccle.expression.names, expression.altnames.test)
	
	gene <- "A2M"
	drug <- "Lapatinib"

	achilles.labels <- essentiality[gene,]
	ccle.labels <- drug.response.matrix[, drugs]

	cat("Dimension of drug.response.matrix: ", dim(drug.response.matrix), "\n")
	cat("Dimension of achilles.expression.reduced: ", dim(achilles.expression.reduced, "\n"))
	cat("Dimension of ccle.expression.reduced: ", dim(ccle.expression.reduced), "\n")
	cat("length of achilles.labels:", length(achilles.labels), "\n")
	cat("length of ccle.labels: ", length(ccle.labels), "\n")

	

}

