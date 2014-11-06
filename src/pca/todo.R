# Why are marks.prcomp$sddev and marks.cor.eigen$values different from svd$d 
# whereas the marks.prcomp$rotation and marks.cor.eigen$vectors are the same
# as svd$v ?

# Answer : The former calculate on a correlated matrix which is scaled

source("src/loadCleanedSSLCData.R")

data <- loadSSLCData()
allrecords <- data$allrecords
marks = data.frame(L1 = allrecords[["L1_MARKS"]],
                   L2 = allrecords[["L2_MARKS"]],
                   L3 = allrecords[["L3_MARKS"]],
                   MATH = allrecords[["S1_MARKS"]], 
                   SCIENCE = allrecords[["S2_MARKS"]],
                   SOCIAL_SCIENCE = allrecords[["S3_MARKS"]])

print("singular values using eigen values of the correlation matrix")
marks.cor = cor(marks)
marks.cor.eigen = eigen(marks.cor)
sqrt(marks.cor.eigen$values)

print("singular values using prcomp")
marks.prcomp = prcomp(scale(marks))
marks.prcomp$sdev

print("singular values using the svd")
marks.svd = svd(scale(marks))
marks.svd$d / (nrow(marks) - 1)

# 

