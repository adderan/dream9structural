load("ccle.RData")
load("achilles.RData")
n.ccle.samples <- dim(ccle.expression)[[2]]
n.ccle.expression <- dim(ccle.expression)[[1]]
top25 <- read.table("data/top25per.features.lst")
sigGenes <- read.table("data/sigGenes.Zach2014.corrected.lst")
top25 <- top25[3:dim(top25)[[1]],1]


select.data <- function(features, column,  data) {
	selected.data <- NULL
	for(i in 1:length(features)) {
		name <- as.character(features[[i]])
		#print(name)
		selected.row <- data[data[column] == name,]
		selected.data <- rbind(selected.data, selected.row)
	}
	colnames(selected.data) <- colnames(data)
	return(selected.data)

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
select.data.rownames <- function(featurenames, data) {
	selected.data <- NULL
	for(i in 1:length(featurenames)) {
		name <- as.character(featurenames[[i]])
		selected.row <- data[name,]
		selected.data <- rbind(selected.data, selected.row)
	}
	rownames(selected.data) <- featurenames
	return(selected.data)
}
reduce.all.data <- function() {
	drug.response.matrix <- make.response.matrix()
	ccle.expression.names <- rownames(ccle.expression)

	#reduce by top25 list and ccle.expression
	achilles.expression.reduced <- select.data(top25, 1, expression)
	achilles.expression.reduced <- select.data(ccle.expression.names, 2, achilles.expression.reduced)
	print(dim(achilles.expression.reduced))
	
	reduced.feature.names <- achilles.expression.reduced[,2]
	ccle.expression.reduced <- select.data.rownames(reduced.feature.names, ccle.expression)
	print(dim(ccle.expression.reduced))
	reduced.feature.altnames <- achilles.expression.reduced[,1]

	achilles.expression.reduced.leaderboard <- select.data(reduced.feature.altnames, 1, expression.test)
	print(dim(achilles.expression.reduced.leaderboard))
	save(achilles.expression.reduced, ccle.expression.reduced, achilles.expression.reduced.leaderboard, drug.response.matrix, file="reduced.RData")
}
