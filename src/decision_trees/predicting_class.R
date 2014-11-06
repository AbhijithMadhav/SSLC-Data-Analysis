library(rpart)

source("src/loadCleanedSSLCData.R")

data <-loadSSLCData()
trainrecords = data[["trainrecords"]]
testrecords = data[["testrecords"]]

dtfit <- rpart(NRC_CLASS ~ 
                   SCHOOL_TYPE
               + URBAN_RURAL
               + NRC_CASTE_CODE 
               + NRC_GENDER_CODE
               + NRC_MEDIUM
               + NRC_PHYSICAL_CONDITION
               + CANDIDATE_TYPE, data = trainrecords, method="class")
plot(dtfit)
text(dtfit, pretty = 0)

dtpredict <- predict(dtfit, testrecords,type="class")
dtconfmat <- table(true  = testrecords$NRC_CLASS, pred = dtpredict)
dtconfmat

error = (sum(dtconfmat) - sum(diag(dtconfmat)))/sum(dtconfmat)
error

