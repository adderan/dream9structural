read.drug.data <- function() {
	drug.response <- read.csv("data/CCLE_NP24.2009_Drug_data_2012.02.20.csv", header=TRUE)

	drug.profiles <- read.csv("data/CCLE_NP24.2009_profiling_2012.02.20.csv", header=TRUE)

	ccle.expression <- read.table("data/CCLE_Expression_Entrez_2012-09-29.tab", fill = TRUE, header=TRUE)
	#ccle.expression.gene.names <- ccle.expression[,1]
	#ccle.expression <- ccle.expression[,-1]
	#ccle.expression <- as.matrix(ccle.expression)
	#rownames(ccle.expression) <- ccle.expression.gene.names
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

parse.gct.full <- function(filename) {
	data <- read.table(filename, skip=2, header=TRUE)
	return(data)
}
read.achilles.data <- function() {
	expression <- parse.gct.full("data/CCLE_expression_training_phase2.gct")
	copynumber <- parse.gct.full("data/CCLE_copynumber_training_phase2.gct")
	expression.test <- parse.gct.full("data/CCLE_expression_leaderboard_phase2.gct")
	copynumber.test <- parse.gct.full("data/CCLE_copynumber_leaderboard_phase2.gct")
	essentiality <- parse.gct.full("data/Achilles_v2.11_training_phase2.gct")
	save(expression, copynumber, expression.test, copynumber.test, essentiality, file="achilles.RData")
}
