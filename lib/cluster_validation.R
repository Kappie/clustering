library(clValid)

distance_matrix <- read.table("distance_matrices/34-mammals-xz", row.names=1)
Dist <- as.dist(distance_matrix)

clustering <- hclust(Dist, method="average")

number_of_clusters <- 30
cluster <- cutree(clustering, number_of_clusters)
score <- dunn(Dist, cluster)
print(score)

