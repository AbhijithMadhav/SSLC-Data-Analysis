library(methods)

# Prior preperation of data using spreadsheet software
# 1. Have removed the '*'s in all of the marks column. 
# 2. Absentees have been given 0 marks in the respective subjects replacing the 888 marker
# 3. Correcting totalling errors for about 21 records
# 4. Added 'class' fields for each of the marks

# Note : The following columns have the said number of rows with NA data. Shouldn't really matter
# DOB = 1
# NRC_MOTHER_NAME = 142
# NRC_FATHER_NAME = 32
# L1_RESULT = 2
# L2_RESULT = 32
# L3_RESULT = 38

# Note : There is a district with "NA" code. This will by default be treated as <NA> by R. Need to work around this.
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
                      colClasses = classes, na.strings = "dfakjsdfhalsdfhasdfkasdhfkas"); # There is a district code = "NA"
    
    # Get the training and test records(2:1)
    set.seed(2) # To ensure comparision of output against same set of records
    index <- sample(1:nrow(dtsslc))
    testindex <- sample(1:nrow(dtsslc), floor(length(index))/3)
    list(allrecords = dtsslc, trainrecords = dtsslc[-testindex, ], testrecords = dtsslc[testindex, ])
}


getmarks <- function(records)
{
    data.frame(L1_MARKS = records[["L1_MARKS"]],
               L2_MARKS = records[["L2_MARKS"]],
               L3_MARKS = records[["L3_MARKS"]],
               S1_MARKS = records[["S1_MARKS"]], 
               S2_MARKS = records[["S2_MARKS"]],
               S3_MARKS = records[["S3_MARKS"]],
               TOTAL_MARKS = records[["TOTAL_MARKS"]])
}