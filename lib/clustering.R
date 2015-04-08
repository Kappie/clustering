dir_name <- "34-mammals"
distances <- "distance_matrices/34-mammals-xz" #paste("distance_matrices/", dir_name, sep="") 
output_location <- paste("plots/", dir_name, sep="")

distance_matrix <- read.table(distances, row.names=1)
clustering <- hclust( as.dist(distance_matrix), method="ward.D2" )

pdf("plots/34-mammals-xz.pdf")
plot(clustering)
dev.off()
