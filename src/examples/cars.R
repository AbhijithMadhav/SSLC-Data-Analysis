library(rpart)

#Load the dataset into R
dtcars <- read.csv("data/cars.csv",header=TRUE)

#Creating the Trainning set and Test set
index <- 1:nrow(dtcars)
testindex <- sample(index, trunc(length(index)/3))
testrecords <- dtcars[testindex,]
traindrecords <- dtcars[-testindex,]

dtfit <- rpart(carclass~buying + maint + doors + persons + lug_boot +
                   safety, data = traindrecords, method="class")
plot(dtfit, uniform=TRUE, compress=TRUE)
text(dtfit, use.n=TRUE, all=TRUE, cex=.7)

#Predict the test set using the created Decision tree model
dtpredict <- predict(dtfit,testrecords,type="class")
dtconfmat <- table(true = testrecords[,7], pred = dtpredict)

#Display the tabulation
dtconfmat

error = (sum(dtconfmat) - sum(diag(dtconfmat)))/sum(dtconfmat)
error
