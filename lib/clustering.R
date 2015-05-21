library(ape)

distances <- "distance_matrices/24-mammals-davy.csv" #paste("distance_matrices/", dir_name, sep="") 

#distance_matrix <- read.table(distances, row.names=1)

distance_matrix <- as.dist( as.matrix( read.csv(distances, row.names=1 ) ) )
clustering <- hclust(distance_matrix, method="average" )

pdf("plots/24-mammals-davy-average.pdf")
plot( as.phylo(clustering) )
dev.off()
