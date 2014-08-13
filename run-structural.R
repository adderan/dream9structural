load("reduced.RData")
load("achilles.RData")


drug.correlations <- read.table("data/CCLE_vs_challenge.Amax.tab", header=TRUE)
drug.correlations.names <- drug.correlations[,1]
rownames(drug.correlations) <- drug.correlations.names
drug.correlations <- drug.correlations[,-1]

n.essentiality <- dim(essentiality)[[1]]
n.samples <- dim(essentiality)[[2]]


choose.drugs <- function() {
	
	n.genes <- dim(drug.correlations)[[1]]
	drugs.to.use <- list()
	for(i in 1:n.genes) {
		correlations.for.gene <- drug.correlations[i,]
		gene.name <- rownames(drug.correlations)[[i]]
		drug.index <- which.max(correlations.for.gene)
		drugs.to.use[[gene.name]] <- colnames(drug.correlations)[[drug.index]]
	}
	return(drugs.to.use)	
}
train.structural.algorithm <- function(gene) {
	drug.list <- choose.drugs()
	training.drug.labels <- drug.response.matrix[, drug.list[[gene]]]
	cat("Length of training.drug.labels: ", length(training.drug.labels), "\n")
	training.essentiality.labels <- essentiality[gene,]

	achilles.expression.reduced.names <- achilles.expression.reduced[,2]
	ccle.expression.reduced.names <- rownames(ccle.expression.reduced)
	

	#convert the achilles expression into a useable form
	#rownames(achilles.expression.reduced) <- achilles.expression.reduced.names
	achilles.expression.reduced <- achilles.expression.reduced[,-1]
	achilles.expression.reduced <- achilles.expression.reduced[,-1]



	ando.X <- list(achilles.expression.reduced, ccle.expression.reduced)
	ando.Y <- list(training.essentiality.labels, training.drug.labels)

	return(achilles.expression.reduced)
	

}
	
