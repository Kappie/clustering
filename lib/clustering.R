library(ape)

distances <- "distance_matrices/literature-lc-4.csv" #paste("distance_matrices/", dir_name, sep="") 

#distance_matrix <- read.table(distances, row.names=1)

distance_matrix <- as.dist( as.matrix( read.csv(distances, row.names=1 ) ) )
clustering <- hclust(distance_matrix, method="average" )

pdf("plots/literature-lc-4-average.pdf")
plot( as.phylo(clustering) )
dev.off()
