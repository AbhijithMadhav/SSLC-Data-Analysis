# correlation analysis against class obtained by student

source("src/utils.R")
source("src/correlation/correlation_helper.R")

data <- loadSSLCData()
allrecords <- data$allrecords

x = data.frame(PLACE = allrecords[["URBAN_RURAL"]],
               CASTE = allrecords[["NRC_CASTE_CODE"]],
               GENDER = allrecords[["NRC_GENDER_CODE"]],
               SCHOOL_TYPE = allrecords[["SCHOOL_TYPE"]],
               MEDIUM = allrecords[["NRC_MEDIUM"]],
               PHYSICAL_CONDITION = allrecords[["NRC_PHYSICAL_CONDITION"]],
               CANDIDATE_TYPE = allrecords[["CANDIDATE_TYPE"]],
               DISTRICT = allrecords[["DIST_CODE"]],
               #TALUK = allrecords[["TALUQ_CODE"]],
               CLASS = allrecords[["NRC_CLASS"]])

library(caret)
dmy <- dummyVars("~ .", data = x)
xTrsf <- data.frame(predict(dmy, newdata = x))
corMasterList <- flattenSquareMatrix(cor.prob(xTrsf))
corList <- corMasterList[order(-abs(corMasterList$cor)),]
selectedSub <- subset(corList, (abs(cor) > 0.1 & (j == 'CLASS.1' | j == 'CLASS.2' | j == 'CLASS.PASS' | j == 'CLASS.FAIL' | j == 'CLASS.D...') &( i != 'CLASS.1' & i != 'CLASS.2' & i != 'CLASS.PASS' & i != 'CLASS.FAIL' & i != 'CLASS.D...')))
selectedSub

#bestSub <- as.character(selectedSub$i[c(1, 2, 3, 4, 5, 6, 7)])
#library(psych)
#pairs.panels(xTrsf[c(bestSub, 'CLASS.FAIL', 'CLASS.PASS', 'CLASS.2', 'CLASS.1', 'CLASS.D...')])
