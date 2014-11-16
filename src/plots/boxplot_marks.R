source("src/utils.R")

data <- loadSSLCData()
allrecords <- data[["allrecords"]]

#par(mfrow = c(2, 3))

x <- allrecords$L1_MARKS
boxplot(allrecords$L1_MARKS, range = 0)
abline(h = 44)

x <- allrecords$L2_MARKS
boxplot(allrecords$L2_MARKS, range = 0)
abline(h = 35)

x <- allrecords$L3_MARKS
boxplot(allrecords$L3_MARKS, range = 0)
abline(h = 35)

x <- allrecords$S1_MARKS
boxplot(allrecords$S1_MARKS, range = 0)
abline(h = 35)

x <- allrecords$S2_MARKS
boxplot(allrecords$S2_MARKS, range = 0)
abline(h = 35)

x <- allrecords$S3_MARKS
boxplot(allrecords$S3_MARKS, range = 0)
abline(h = 35)

boxplot(allrecords$L3_MARKS ~ allrecords$SCHOOL_TYPE, data = allrecords)
abline(h = 35)
