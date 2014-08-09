read.drug.data <- function() {
	drug.response <- read.csv("data/CCLE_NP24.2009_Drug_data_2012.02.20.csv", header=TRUE)

	drug.profiles <- read.csv("data/CCLE_NP24.2009_profiling_2012.02.20.csv", header=TRUE)

	ccle.expression <- read.table("data/CCLE_Expression_Entrez_2012-09-29.tab", fill = TRUE, header=TRUE)
	ccle.expression.gene.names <- ccle.expression[,1]
	ccle.expression <- ccle.expression[,-1]
	ccle.expression <- as.matrix(ccle.expression)
	rownames(ccle.expression) <- ccle.expression.gene.names
	#ccle.copynumber <- read.table
	save(drug.response, drug.profiles, ccle.expression, file="ccle.RData")
}

parse.gct <- function(filename) {
	data <- read.table(filename, skip = 2, header = TRUE)
	gene.names <- data[,1]
	sample.names <- colnames(data)

	data <- data[,-1]
	data <- data[,-1]
	data.matrix <- as.matrix(data)
	rownames(data.matrix) <- gene.names
	return(data.matrix)
}

read.achilles.data <- function() {
	expression.altnames <- parse.gct("data/CCLE_expression_training.gct")
	copynumber <- parse.gct("data/CCLE_copynumber_training.gct")
	expression.altnames.test <- parse.gct("data/CCLE_expression_leaderboard.gct")
	copynumber.test <- parse.gct("data/CCLE_copynumber_leaderboard.gct")
	essentiality <- parse.gct("data/Achilles_v2.9_training.gct")
	save(expression.altnames, copynumber, expression.altnames.test, copynumber.test, essentiality, file="achilles.RData")
}