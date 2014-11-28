source("src/utils.R")
data <- loadSSLCData()
marks <- getmarks(data$allrecords)
marks <- marks[, 1:6]
names(marks)

# Elbow plot to determine the number of clusters for kmeans
# TODO: What about bisecting K-means?
sse <- c()
centers <- c()
for (k in 2:10) {
    kmeans.out <- kmeans(scale(marks), centers = k, iter.max = 20, nstart = 3)
    sse[k] <- kmeans.out$tot.withinss
    centers[k] <- k
}
plot(centers, sse)
lines(centers, sse)
# Elbow is at 4 clusters

# TODO
# silhouette coefficient : Gives a numerical measure of how good the clustering
# is. This measure is based on the cohesion within the cluster and the
# seperation amongst clusters
library(cluster)
# disse <- daisy(marks) # Not able to do this as dataset is too large

# kmeans for 4 clusters
kmeans.out <- kmeans(scale(marks), centers = 4, iter.max = 20, nstart = 5)

# How are the clusters distributed about the NRC_CLASS?
table(kmeans.out$cluster, data$allrecords$NRC_CLASS)
# Cluster 1 : FAIL and PASS
# Cluster 2 : D and 1st class
# Cluster 3 : 1st class and 2nd class
# Cluster 4 : FAIL

# Visualization of the above and characterization of the clusters in terms 
# of mean marks
mean_marks <- c()
for (cluster in 1:4) {
    mean_marks[cluster] 
    <- mean(data$allrecords$TOTAL_MARKS[kmeans.out$cluster == cluster])
}
barplot(mean_marks, #col = 1:4, 
        names.arg = c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4"),
        ylab = "Average marks(out of 625)", ylim = c(0, 650),
        main = "Performance of each cluster in the exams")
abline(h = 625 * .35)
text(4.5, 625 * .35 + 10, "PASS")
abline(h = 625 * .5)
text(4.5, 625 * .5 + 10, "2nd Class")
abline(h = 625 * .6)
text(4.5, 625 * .6 + 10, "1st Class")
abline(h = 625 * .85)
text(4.5, 625 * .85 + 10, "Distinction")
# Cluster 1 : Average is just pass
# Cluster 2 : Average is slightly missing distinction
# Cluster 3 : Average is just missing 1st class
# Cluster 4 : Definitely fail


# Visualization of clusters by reducing dimensionality to 2
marks.svd <- svd(marks)
plot(marks.svd$u[, 1:2], col = kmeans.out$cluster, xaxt = "n", yaxt = "n",
     xlab = "", ylab ="", main = "Clustering based on course marks",
     sub = "Dimensionality reduced to 2 to aid visualization")

# Characterize clusters using association rules
# This is done by generating rules which have the cluster id as the 
# consequent

# Create transactions
transactions <- data$allrecords[, c("DIST_CODE", "TALUQ_CODE", "SCHOOL_CODE", 
                                    "SCHOOL_TYPE", "URBAN_RURAL", 
                                    "NRC_CASTE_CODE", "NRC_GENDER_CODE",
                                    "NRC_MEDIUM", "NRC_PHYSICAL_CONDITION",
                                    "NRC_CLASS", "CANDIDATE_TYPE")]
transactions[, "CLUSTER_ID"] <- as.factor(kmeans.out$cluster)
names(transactions)

# Generate rules for each cluster
library("arules")
library("arulesViz")
rules <- list()
for (cluster in 1:4) {
    rules[cluster] <- apriori(as(transactions[transactions$CLUSTER_ID == cluster, ], 
                                 "transactions"),
                              appearance = list(rhs = c(paste("CLUSTER_ID=", 
                                                              cluster, 
                                                              sep = "")), 
                                                default = "lhs"),
                              parameter = list(support = 0.5))
}


# Characterization of each of the clusters

# Common attributes
# normal physical condition, fresher, general caste
for (cluster_id in 1:4) {
    inspect(sort(rules[[cluster_id]], by = "support")) # as confidence and list are 1
# Cluster 1 : kannada medium, pass, rural, boys
# Cluster 2 : 1st class, urban , english, girl
# Cluster 3 : kannada, rural, 2nd class, girl
# Cluster 4: fail, kannada, boys, urban
}
