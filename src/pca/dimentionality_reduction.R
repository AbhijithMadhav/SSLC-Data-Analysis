source("src/utils.R")

data <- loadSSLCData()
allrecords <- data$allrecords
sslc.marks = data.frame(L1 = allrecords[["L1_MARKS"]],
                        L2 = allrecords[["L2_MARKS"]],
                        L3 = allrecords[["L3_MARKS"]],
                        MATH = allrecords[["S1_MARKS"]], 
                        SCIENCE = allrecords[["S2_MARKS"]],
                        SOCIAL_SCIENCE = allrecords[["S3_MARKS"]])

plot(sslc.marks, main="Plot of covariation of marks", pch = 20, col = "blue")
options(digits = 3)

# standardize the data. Use correlation matrix as marks for L1 is out of 125
# whereas for others it is out of 100
sslc.scaled.marks = apply(sslc.marks, 2, function(x) { (x - mean(x))/sd(x)})

# Find eigen values of the covariance matrix
sslc.cov = cov(sslc.scaled.marks)
sslc.eigen = eigen(sslc.cov)
rownames(sslc.eigen$vectors) = colnames(sslc.marks)
colnames(sslc.eigen$vectors) = c("PC1","PC2", "PC3", "PC4", "PC5", "PC6")
sslc.eigen

# See how much variation each eigenvector(principle component) accounts for
pc.var <- sapply(sslc.eigen$values, function(x) { 100*round(x/sum(sslc.eigen$values),digits=2) })
pc.var

# Multiply the scaled data by the eigen vectors (principal components) to get the 
# reduced data
# How many eigen vectors should be considered? 
# Considering 3 as that will cover capture 90% of the variation in data
reduced_data = sslc.scaled.marks %*% sslc.eigen$vectors[,1:3] # U

# What kind of operations can be done on this reduced data?

# Recover original data
#recovered_data = scores %*% t(my.eigen$vectors)

#plot(recovered_data, main="Recovered data", cex = 0.9)
