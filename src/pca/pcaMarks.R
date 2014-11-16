# Purpose : To figure out the proportion of the variance in marks 
# of the students captured by the principle components

source("src/utils.R")
data <- loadSSLCData()
marks = data.frame(L1 = data$allrecords[["L1_MARKS"]],
                        L2 = data$allrecords[["L2_MARKS"]],
                        L3 = data$allrecords[["L3_MARKS"]],
                        MATH = data$allrecords[["S1_MARKS"]], 
                        SCIENCE = data$allrecords[["S2_MARKS"]],
                        SOCIAL_SCIENCE = data$allrecords[["S3_MARKS"]])

marks.prcomp = prcomp(scale(marks))

marks.prcomp

# The sum of the eigen values of the correlation matrix gives the sum of
# the variances measured along each attribute of the data set
sum(marks.prcomp$sdev ^ 2)
sum(apply(scale(marks), 2, sd))

# Thus the eigen values, being associated with eigen vectors, gives the 
# proportion of variance captured by the eigen vectors
# Note that the eigen values and eigen vectors of the correlation matrix
# are the same as the singular values and singular vectors of the scaled
# data matrix

par(mfrow = c(1, 1))
proportionOfVariance = marks.prcomp$sdev ^ 2 /sum(marks.prcomp$sdev ^ 2)
plot(proportionOfVariance, xaxt = "n", pch = 20, 
     ylab = "Proportion of variance in marks captured",
     xlab = "Principle Components", 
     main = "Variance of marks captured by PC's")
lines(proportionOfVariance)
axis(1, at = 1:6, labels = c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6"))
