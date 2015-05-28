library(ape)

distances <- "distance_matrices/30-source-files-no-comments.csv" #paste("distance_matrices/", dir_name, sep="") 

#distance_matrix <- read.table(distances, row.names=1)

distance_matrix <- as.dist( as.matrix( read.csv(distances, row.names=1 ) ) )
clustering <- hclust(distance_matrix, method="average" )
dendrogram <- as.dendrogram(clustering)

pdf("plots/30-source-files-no-comments-average.pdf")
#plot( cut(dendrogram, h = 0.90)[[1]])
plot( clustering, ann=FALSE, ylab=FALSE)
dev.off()
