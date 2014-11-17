source("src/utils.R")

data <- loadSSLCData()
marks <- getmarks(data$allrecords)


# Classification using decision trees
rpart6 <- my_rpart(NRC_CLASS ~ L1_CLASS + L2_CLASS + L3_CLASS + S1_CLASS 
                   + S2_CLASS + S3_CLASS,
                   data$trainrecords, data$testrecords, 
                   data$testrecords$NRC_CLASS)
rpart6$table
rpart6$error
plot(rpart6$rpart.out)
text(rpart6$rpart.out, pretty = 0)
