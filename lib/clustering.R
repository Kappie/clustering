distances <- "distance_matrices/24-mammals" #paste("distance_matrices/", dir_name, sep="") 

distance_matrix <- read.table(distances, row.names=1)
clustering <- hclust( as.dist(distance_matrix), method="average" )

pdf("plots/24-mammals-average")
plot(clustering, hang=-1, axes=FALSE)
dev.off()
