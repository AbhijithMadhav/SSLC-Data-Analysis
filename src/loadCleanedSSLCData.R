library(methods)

loadSSLCData <- function() {
    # Set up a conversion class, myDate, to convert from the input date format
    # to the native Date type
    setClass('myDate')
    setAs("character","myDate", function(from) as.Date(from, format="%m/%d/%Y") )
    
    # Determine the classes of the column using the first 100 records
    # Helps read.csv be faster and occupy less memory
    classes = c(DOB = "myDate") # This triggers the setAs() conversion routine
    initial = read.csv("data/cleaned_sslc_data.csv",
                       colClasses = classes, nrows = 100)
    classes = sapply(initial, class)
    
    # Read in the data
    classes["DOB"] = "myDate" # This triggers the setAs() conversion routine
    classes["NRC_CASTE_CODE"] = "factor"
    dtsslc = read.csv("data/cleaned_sslc_data.csv",
                      colClasses = classes);
    
    # Get the training and test records(2:1)
    set.seed(2) # To ensure comparision of output against same set of records
    index <- sample(1:nrow(dtsslc))
    testindex <- sample(1:nrow(dtsslc), floor(length(index))/3)
    list(allrecords = dtsslc, trainrecords = dtsslc[-testindex, ], testrecords = dtsslc[testindex, ])
}