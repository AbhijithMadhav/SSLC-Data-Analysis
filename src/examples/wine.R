pcaegdata <- read.csv("data/wine.data", header=FALSE)
#Normalize data
#Substract mean and divide by standard deviation for each column
pcadtnr <- pcaegdata
#Substract mean for each column
#Column 1
pcadtnr[,1] <- pcaegdata[,1]-mean(pcaegdata[,1])
sd1 <- sd(pcaegdata[,1])
pcadtnr[,1] <- pcadtnr[,1] / sd1

pcadtnr[,2] <- pcaegdata[,2]-mean(pcaegdata[,2])
sd2 <- sd(pcaegdata[,1])
pcadtnr[,2] <- pcadtnr[,2] / sd2
#Build the covariance matrix
covmat <- cov(pcadtnr)
#Find the eigen vectors and eigen values
eig <- eigen(covmat)
eig1 <- eig$vectors[,1]
#Projection in new dimension
Z <- as.matrix(pcadtnr)%*%eig1
#Recovering the data
X_rec <- Z%*%t(eig1)
#plotting raw and reduced data
par(mfrow=c(1,2))
plot(Z, main="2D plot of raw data")
plot(X_rec, main="1D plot of reduced data")