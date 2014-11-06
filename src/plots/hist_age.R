source("src/loadCleanedSSLCData.R")

age <- function(birthday) {
    as.integer(as.integer(Sys.Date() - birthday)/365.25)
}

data <- loadSSLCData()
allrecords <- data[["allrecords"]]
x <- sapply(allrecords$DOB, age)

hist(x, xlab = "Age of Students", col = "green", breaks = 50)
rug(x)
