source("src/loadCleanedSSLCData.R")

data <- loadSSLCData()
allrecords <- data$allrecords
sslc.marks = data.frame(L1 = allrecords[["L1_MARKS"]],
                        L2 = allrecords[["L2_MARKS"]],
                        L3 = allrecords[["L3_MARKS"]],
                        MATH = allrecords[["S1_MARKS"]], 
                        SCIENCE = allrecords[["S2_MARKS"]],
                        SOCIAL_SCIENCE = allrecords[["S3_MARKS"]])

library(psych)
pairs.panels(sslc.marks)
