load("/home/alden/dream9structural/ccle.RData")
load("/home/alden/dream9structural/achilles.RData")
top25 <- read.table("/home/alden/dream9structural/data/top25per.features.lst")
top25 <- as.character(top25[,1])
n.ccle.samples <- dim(ccle.expression)[[2]]



make.name.map <- function(data) {
	map <- list()
	for(i in 1:dim(data)[[1]]) {
		key <- as.character(data[i, 1])
		value <- as.character(data[i, 2])
		map[[key]] <- value
	}
	return(map)
}


select.data <- function(feature.list, column, data) {
	n.features <- length(feature.list)
	selected.data <- NULL
	for(i in 1:n.features) {
		rname <- feature.list[[i]]
		selected.row <- data[data[column] == rname,]
		selected.data <- rbind(selected.data, selected.row)
	}
	return(selected.data)
}
select.data.rownames <- function(feature.list, data) {
	n.features <- length(feature.list)
	selected.data <- NULL
	for(i in 1:n.features) {
		rname <- feature.list[[i]]
		selected.row <- data[rname,]
		selected.data <- rbind(selected.data, selected.row)
	}
	rownames(selected.data) <- feature.list
	return(selected.data)
}

reduce.all.data <- function() {
	achilles.name.map <- make.name.map(expression)
	ccle.names <- rownames(ccle.expression)

	achilles.indexes <- intersect(names(achilles.name.map), top25)
	feature.list <- list()
	for(i in 1:length(achilles.indexes)) {
		feature.list[[i]] <- achilles.name.map[[achilles.indexes[[i]]]]
	}
	feature.list <- intersect(feature.list, ccle.names)


	reduced.ccle.expression <- select.data.rownames(feature.list, ccle.expression)
	reduced.expression <- select.data(feature.list, 2, expression)
	reduced.expression.test <- select.data(feature.list, 2, expression.test)
	save(reduced.ccle.expression, reduced.expression, reduced.expression.test, file="temp123.RData")

	#load("temp123.RData")

	reduced.expression.rownames <- reduced.expression[,2]
	reduced.expression <- reduced.expression[,-1]
	reduced.expression <- reduced.expression[,-1]
	reduced.expression <- as.matrix(reduced.expression)
	rownames(reduced.expression) <- reduced.expression.rownames

	reduced.ccle.expression <- as.matrix(reduced.ccle.expression)
	
	reduced.expression.test <- reduced.expression.test[,-1]
	reduced.expression.test <- reduced.expression.test[,-1]
	reduced.expression.test <- as.matrix(reduced.expression.test)
	rownames(reduced.expression.test) <- reduced.expression.rownames

	essentiality.rownames <- essentiality[,1]
	essentiality <- essentiality[,-1]
	essentiality <- essentiality[,-1]
	essentiality <- as.matrix(essentiality)
	rownames(essentiality) <- essentiality.rownames

	drug.response.matrix <- make.response.matrix()
	save(essentiality, reduced.expression, reduced.ccle.expression, reduced.expression.test, drug.response.matrix, file="reduced-lists.RData")
}
