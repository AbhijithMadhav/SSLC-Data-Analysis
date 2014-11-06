library("rpart")
#Classification - decision tree using breastcancer diagnostic dataset from UCI, Load the dataset into R
bcandiag <- read.csv("data/wdbc.data", header=TRUE)
bcandiag <- bcandiag[,2:32]
#Creating the Trainning set and Test set
index <- 1:nrow(bcandiag)
testindex <- sample(index, trunc(length(index)/3))
testrecords <- bcandiag[testindex,]
traindrecords <- bcandiag[-testindex,]
#Create the decision tree model using the data
dtfit <- rpart(Diagnosis~., data = traindrecords, method="class")
plot(dtfit, uniform=TRUE, compress=TRUE)
text(dtfit, use.n=TRUE, all=TRUE, cex=.7)
#Predict the test set using the created Deciision tree model
dtpredict <- predict(dtfit,testrecords,type="class")
#Cross-tabulation of prediction against the TRUE classes
dtconfmat <- table(true = testrecords[,1], pred = dtpredict)
#Display the tabulation
dtconfmat