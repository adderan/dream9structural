
#adds  feature from featurelist1 to reduced data matrix if it is present in featurelist2 and the data matrix
reduce.features <- function(featurenames, column, data) {
	n <- length(featurenames)
	m <- dim(data)[[2]]
	rnames <- list()
	for(i in 1:n) {
		f <- as.character(featurenames[[i]])
		data.feature.names <- data[,column]
		if(f %in% data.feature.names) {
			rnames[[length(rnames) + 1]] <- f
			#print("found match")
		}
		
	}
	reduced.features <- NULL
	for(i in 1:length(rnames)) {
		name <- rnames[[i]]
		selected.row <- data[data[column] == name,]
		#print(selected.row)
		reduced.features <- rbind(reduced.features, selected.row)
	}
	colnames(reduced.features) <- colnames(data)
	return(reduced.features)
}

