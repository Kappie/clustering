library(ape)

distances <- "distance_matrices/latin_texts.csv" 

distance_matrix <- as.dist( as.matrix( read.csv(distances, row.names=1 ) ) )
clustering <- hclust(distance_matrix, method="complete" )
dendrogram <- as.dendrogram(clustering)

pdf("plots/latin-texts-complete")
plot( clustering, ann=FALSE, ylab=FALSE)
dev.off()
