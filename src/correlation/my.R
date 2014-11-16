source("src/utils.R")

data <- loadSSLCData()
allrecords <- data$allrecords

head(allrecords, 1)
x = data.frame(PLACE = allrecords[["URBAN_RURAL"]],
                        CASTE = allrecords[["NRC_CASTE_CODE"]],
                        GENDER = allrecords[["NRC_GENDER_CODE"]],
                        CLASS = allrecords[["NRC_CLASS"]])
x = head(x, 100)
x$PLACE = ifelse(x$PLACE == 'R', 0, 1)
x$GENDER = ifelse(x$GENDER == 'F', 0, 1)

classToNumber <- function(a) {
    if (a == 'D')
        4
    else if (a == '1')
        3
    else if (a == '2')
        2
    else if (a == 'PASS')
        1
    else
        0
}
x$CLASS = sapply(x$CLASS, classToNumber)
x$CLASS <- unlist(x$CLASS)
library(psych)
pairs.panels(x)
