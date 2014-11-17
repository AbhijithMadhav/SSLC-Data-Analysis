source("src/utils.R")

data <- loadSSLCData()
marks <- getmarks(data$allrecords)

lm.out <- lm(marks$TOTAL_MARKS ~ marks$L1_MARKS + marks$L2_MARKS 
             + marks$L3_MARKS + marks$S1_MARKS + marks$S2_MARKS 
             + marks$S3_MARKS, 
             na.action = "na.fail")

# Find the best models for different number of attributes Not using the AICc
# based ranking given by dredge() itself as that takes into account the
# complexity of the model. Manually ranking models based on sd of residuals In
# the end looks all the same as the AICc rankings :-|
library("MuMIn")
best_models <- list()
for (n in 1:5)
{
    dredge.out <- dredge(lm.out, m.max = n, m.min = n)
    min_residual_se <- .Machine$integer.max
    model <- NULL
    for( i in 1:nrow(dredge.out))
    {
        residual_se <- sd(lm(getCall(dredge.out, i))$residuals)
        if (residual_se < min_residual_se)
        {
            min_residual_se <- residual_se
            model <- getCall(dredge.out, i)
        }
    }
    best_models[length(best_models) + 1] <- list(model)
}

# Classification using knn
marks_trainrecords <- getmarks(data$trainrecords)[, -7]
marks_testrecords <-  getmarks(data$testrecords)[, -7]

# All predictors
knn6 <- my_knn(marks_trainrecords$NRC_CLASS ~ marks_trainrecords$L1_MARKS 
               + marks_trainrecords$L2_MARKS + marks_trainrecords$L3_MARKS
               + marks_trainrecords$S1_MARKS + marks_trainrecords$S2_MARKS
               + marks_trainrecords$S3_MARKS,
               10,
               marks_trainrecords, data$trainrecords$NRC_CLASS,
               marks_testrecords, data$testrecords$NRC_CLASS)

# Best 5 predictors
knn5 <- my_knn(marks_trainrecords$NRC_CLASS ~ marks_trainrecords$L1_MARKS 
               + marks_trainrecords$L2_MARKS + marks_trainrecords$L3_MARKS
               + marks_trainrecords$S1_MARKS + marks_trainrecords$S3_MARKS,
               10,
               marks_trainrecords, data$trainrecords$NRC_CLASS,
               marks_testrecords, data$testrecords$NRC_CLASS)

# Best 4 predictors
knn4 <- my_knn(marks_trainrecords$NRC_CLASS ~ marks_trainrecords$L1_MARKS 
               + marks_trainrecords$L2_MARKS + marks_trainrecords$S1_MARKS
               + marks_trainrecords$S3_MARKS,
               10,
               marks_trainrecords, data$trainrecords$NRC_CLASS,
               marks_testrecords, data$testrecords$NRC_CLASS)

# Best 3 predictors
knn3 <- my_knn(marks_trainrecords$NRC_CLASS ~ marks_trainrecords$L1_MARKS + 
                      marks_trainrecords$L2_MARKS + marks_trainrecords$S3_MARKS,
                  10,
                  marks_trainrecords, data$trainrecords$NRC_CLASS,
                  marks_testrecords, data$testrecords$NRC_CLASS)

# Best 2 predictors
knn2 <- my_knn(marks_trainrecords$NRC_CLASS ~ marks_trainrecords$L1_MARKS 
               + marks_trainrecords$L2_MARKS,
               10,
               marks_trainrecords, data$trainrecords$NRC_CLASS,
               marks_testrecords, data$testrecords$NRC_CLASS)

# Best 1 predictors
knn1 <- my_knn(marks_trainrecords$NRC_CLASS ~ marks_trainrecords$S2_MARKS,
               10,
               marks_trainrecords, data$trainrecords$NRC_CLASS,
               marks_testrecords, data$testrecords$NRC_CLASS)


# Classification using decision trees - Illustration of descretization errors

# All predictors
rpart6 <- my_rpart(NRC_CLASS ~ L1_CLASS + L2_CLASS + L3_CLASS + S1_CLASS 
                   + S2_CLASS + S3_CLASS,
                   data$trainrecords, data$testrecords, 
                   data$testrecords$NRC_CLASS)

# Best for 5 predictors
rpart5 <- my_rpart(NRC_CLASS ~ L1_CLASS + L2_CLASS + L3_CLASS + S1_CLASS 
                   + S3_CLASS,
                   data$trainrecords, data$testrecords, 
                   data$testrecords$NRC_CLASS)

# Best for 4 predictors
rpart4 <- my_rpart(NRC_CLASS ~ L1_CLASS + L2_CLASS + S1_CLASS + S3_CLASS,
                   data$trainrecords, data$testrecords, 
                   data$testrecords$NRC_CLASS)

# Best for 3 predictors
rpart3 <- my_rpart(NRC_CLASS ~ L1_CLASS + L2_CLASS + S3_CLASS,
                   data$trainrecords, data$testrecords, 
                   data$testrecords$NRC_CLASS)

# Best for 2 predictors
rpart2 <- my_rpart(NRC_CLASS ~ L1_CLASS + L2_CLASS,
                   data$trainrecords, data$testrecords, 
                   data$testrecords$NRC_CLASS)

# Best for 1 predictor
rpart1 <- my_rpart(NRC_CLASS ~ S2_CLASS,
                   data$trainrecords, data$testrecords, 
                   data$testrecords$NRC_CLASS)
