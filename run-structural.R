load("/home/alden/dream9structural/reduced-lists.RData")
source("/home/alden/multitask/multitask.R")
source("/home/alden/multitask/ando.R")
source("/home/alden/dream9tools/fix-drugnames.R")
source("/home/alden/dream9tools/write-gct.R")

n.essentiality <- dim(essentiality)[[1]]
n.samples <- dim(essentiality)[[2]]
drug.response.matrix <- fix.drugnames(drug.response.matrix)

test.structural.algorithm <- function() {
	data <- test1.data()
	ando.X <- lapply(data$X.list, t)
	ando.Y <- data$y.list
	joint.min.out <- joint.min(ando.X, ando.Y, 10, 1)
	ando.test(data, joint.min.out$W.hat, joint.min.out$V.hat, joint.min.out$Theta.hat)
}


train.structural.algorithm <- function() {
	#reduce number of features for compute time
	max.features <- 50
	reduced.expression <- reduced.expression[1:max.features,]
	reduced.expression.test <- reduced.expression.test[1:max.features,]
	reduced.ccle.expression <- reduced.ccle.expression[1:max.features,]

	drug.list <- choose.drugs()

	predictions <- matrix(0, n.essentiality, n.samples)
	for(i in 1:n.essentiality) {
		gene <- rownames(essentiality)[[i]]
		print(gene)
		drug.labels <- drug.response.matrix[, drug.list[[gene]]]
		#cat("Length of training.drug.labels: ", length(drug.labels), "\n")

		essentiality.labels <- essentiality[gene,]	


	
		#cat("dimension of ccle expression: ", dim(reduced.ccle.expression), "\n")
		#cat("dimension of achilles expression: ", dim(reduced.expression), "\n")
	

		#print(essentiality.labels)
		ando.X <- list(reduced.expression, reduced.ccle.expression)  #, ccle.expression.reduced)
		ando.Y <- list(essentiality.labels, drug.labels)  #, training.drug.labels)

		joint.min.out <- joint.min(ando.X, ando.Y, 10, 3)
		w <- joint.min.out$W.hat[,1]
		v <- joint.min.out$V.hat[,1]
		theta <- joint.min.out$Theta.hat
		predictions.for.gene <- t(w) %*% reduced.expression.test + t(v) %*% theta %*% reduced.expression.test
		predictions[i,] <- predictions.for.gene
	}
	return(predictions)

}
	
