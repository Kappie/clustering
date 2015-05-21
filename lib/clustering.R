library(ape)

distances <- "distance_matrices/24-mammals-2bit" #paste("distance_matrices/", dir_name, sep="") 

distance_matrix <- read.table(distances, row.names=1)
clustering <- hclust( as.dist(distance_matrix), method="average" )

pdf("plots/24-mammals-2bit-average.pdf")
plot( as.phylo(clustering) )
dev.off()
