source("src/utils.R")

data <- loadSSLCData()
allrecords <- data[["allrecords"]]

par(mfrow = c(1, 1))

x <- allrecords$L1_MARKS
hist(allrecords$L1_MARKS, xlab = "L1 Marks", col = "green", breaks = 125)
rug(x)
abline(v = 44)
abline(v = median(x), col = "magenta", lwd = 4)

x <- allrecords$L2_MARKS
hist(allrecords$L2_MARKS, xlab = "L2 Marks", col = "green", breaks = 100)
rug(x)
abline(v = 35)
abline(v = median(x), col = "magenta", lwd = 4)

x <- allrecords$L3_MARKS
hist(allrecords$L3_MARKS, xlab = "L3 Marks", col = "green", breaks = 100)
rug(x)
abline(v = 35)
abline(v = median(x), col = "magenta", lwd = 4)

x <- allrecords$S1_MARKS
hist(allrecords$S1_MARKS, xlab = "S1 Marks", col = "green", breaks = 100)
rug(x)
abline(v = 35)
abline(v = median(x), col = "magenta", lwd = 4)

x <- allrecords$S2_MARKS
hist(allrecords$S2_MARKS, xlab = "S2 Marks", col = "green", breaks = 100)
rug(x)
abline(v = 35)
abline(v = median(x), col = "magenta", lwd = 4)

x <- allrecords$S3_MARKS
hist(allrecords$S3_MARKS, xlab = "S3 Marks", col = "green", breaks = 100)
rug(x)
abline(v = 35)
abline(v = median(x), col = "magenta", lwd = 4)

par(mfrow = c(2, 1))
x <- subset(allrecords$L2_MARKS, allrecords$URBAN_RURAL == 'R')
hist(x)
abline(v = 35)
abline(v = median(x), col = "magenta", lwd = 4)
x <- subset(allrecords$L2_MARKS, allrecords$URBAN_RURAL == 'U') 
hist(x)
abline(v = 35)
abline(v = median(x), col = "magenta", lwd = 4)

par(mfrow = c(2, 1))
x <- subset(allrecords$L2_MARKS, allrecords$NRC_MEDIUM == 'E')
hist(x)
abline(v = 35)
abline(v = median(x), col = "magenta", lwd = 4)
x <- subset(allrecords$L2_MARKS, allrecords$NRC_MEDIUM != 'E')
hist(x)
abline(v = 35)
abline(v = median(x), col = "magenta", lwd = 4)

