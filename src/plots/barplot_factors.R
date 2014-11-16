source("src/utils.R")

data <- loadSSLCData()
allrecords <- data[["allrecords"]]

par(mfrow = c(1, 1))

x <- allrecords$DIST_CODE
barplot(table(x), col = "wheat", main = "Number of candidates in each district")

x <- allrecords$SCHOOL_TYPE
barplot(table(x), col = "wheat", main = "Number of candidates in each school type")

x <- allrecords$URBAN_RURAL
barplot(table(x), col = "wheat", main = "Number of candidates from urban or rural background")

x <- allrecords$NRC_CASTE_CODE
barplot(table(x), col = "wheat", main = "Number of candidates by caste")

x <- allrecords$NRC_GENDER_CODE
barplot(table(x), col = "wheat", main = "Number of candidates by gender")

x <- allrecords$NRC_MEDIUM
barplot(table(x), col = "wheat", main = "Number of candidates by medium")

x <- allrecords$NRC_PHYSICAL_CONDITION
barplot(table(x), col = "wheat", main = "Number of candidates by physical condition")

x <- allrecords$CANDIDATE_TYPE
barplot(table(x), col = "wheat", main = "Number of candidates by type")

x <- allrecords$NRC_CLASS
barplot(table(x), col = "wheat", main = "Number of candidates by result class")

x <- allrecords$NRC_RESULT
barplot(table(x), col = "wheat", main = "Number of candidates by result")