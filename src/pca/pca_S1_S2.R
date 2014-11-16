source("src/utils.R")

data <- loadSSLCData()
allrecords <- data$allrecords
marks = data.frame(MATH = allrecords[["S1_MARKS"]], 
                        SCIENCE = allrecords[["S2_MARKS"]])

plot(marks, main="Plot of math vs physics scores", cex = 0.9, col = "blue")
options(digits = 3)

# Find eigen values of the covariance/correlation matrix
marks.cor = cor(marks)
marks.cor.eigen = eigen(marks.cor)
rownames(marks.cor.eigen$vectors)=colnames(marks)
colnames(marks.cor.eigen$vectors)=c("PC1","PC2")
marks.cor.eigen

pc1.slope = marks.cor.eigen$vectors[1,1]/marks.cor.eigen$vectors[2,1]
pc2.slope = marks.cor.eigen$vectors[1,2]/marks.cor.eigen$vectors[2,2]

abline(0,pc1.slope,col="red")
abline(0,pc2.slope,col="green")

# See how much variation each eigenvector accounts for

pc1.var = 100*round(marks.cor.eigen$values[1]/sum(marks.cor.eigen$values),digits=2)
pc2.var = 100*round(marks.cor.eigen$values[2]/sum(marks.cor.eigen$values),digits=2)
pc1.var
pc2.var
xlab=paste("PC1 - ",pc1.var," % of variation",sep="")
ylab=paste("PC2 - ",pc2.var," % of variation",sep="")

# Multiply the scaled data by the eigen vectors (principal components)
marks.scaled = scale(marks)
scores = marks.scaled %*% marks.cor.eigen$vectors # U
sd = sqrt(marks.cor.eigen$values) # Because sum of eigenvalues is the sum of the variances of the attributes

plot(scores, main="Data in terms of EigenVectors / PCs",xlab=xlab,ylab=ylab)
abline(0,0,col="red")
abline(0,90,col="green")

