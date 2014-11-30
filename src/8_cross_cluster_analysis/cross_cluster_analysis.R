source("src/utils.R")
library("arules")
library("arulesViz")

clusterAndClassify <- function(data, k) {
    marks <- getmarks(data)
    marks <- marks[, 1:6]
    
    kmeans.out <- kmeans(scale(marks), centers = k, iter.max = 20, nstart = 5)
    
    # Characterize clusters using association rules
    # This is done by generating rules which have the cluster id as the 
    # consequent
    
    # Create transactions
    # Not including marks related attributes in the itemset as a performance based 
    # characterization has already been determined above
    transactions <- data[, c("DIST_CODE", "TALUQ_CODE", "SCHOOL_CODE", 
                             "SCHOOL_TYPE", "URBAN_RURAL", 
                             "NRC_CASTE_CODE", "NRC_GENDER_CODE",
                             "NRC_MEDIUM", "NRC_PHYSICAL_CONDITION",
                             "NRC_CLASS", "CANDIDATE_TYPE",
                             "L1_CLASS", "L2_CLASS", "L3_CLASS",
                             "S1_CLASS", "S2_CLASS", "S3_CLASS")]
    transactions[, "CLUSTER_ID"] <- as.factor(kmeans.out$cluster)
    
    # Generate rules
    cluster.rules <- list()
    cluster.mean_marks <- c()
    for (cluster_id in 1:k) {
        cluster.rules[cluster_id] <- apriori(as(transactions, "transactions"),
                             appearance = list(lhs = c(paste("CLUSTER_ID=",
                                                             cluster_id,
                                                             sep = "")), 
                                               default = "rhs"),
                             parameter = list(support = 0.01, 
                                              confidence = 0.7,
                                              minlen = 2))
        
        cluster.data = data[kmeans.out$cluster == cluster_id, ]
        cluster.mean_marks[cluster_id] <- round(mean(cluster.data$TOTAL_MARKS))
        #inspect(sort(cluster.rules[[k]], by = "lift"))
    }
    
    list(kmeans.out = kmeans.out, rules = cluster.rules, mean_marks = cluster.mean_marks)
}

plotClusterMarks <- function(data, kmeans.out) {
    # Visualization of the above and characterization of the clusters in terms 
    # of mean marks
    mean_marks <- c()
    for (cluster_id in 1:4) {
        mean_marks[cluster_id] <- mean(data$TOTAL_MARKS[kmeans.out$cluster == cluster_id])
    }
    barplot(mean_marks, #col = 1:4, 
            names.arg = c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4"),
            ylab = "Average marks(out of 625)", ylim = c(0, 650),
            main = "Performance of each cluster in the exams")
    abline(h = 625 * .35)
    text(4.5, 625 * .35 + 10, "PASS")
    abline(h = 625 * .5)
    text(4.5, 625 * .5 + 10, "2nd Class")
    abline(h = 625 * .6)
    text(4.5, 625 * .6 + 10, "1st Class")
    abline(h = 625 * .85)
    text(4.5, 625 * .85 + 10, "Distinction")
}

plotElbow <- function(data) {
    
    marks <- getmarks(data)
    marks <- marks[, 1:6]
    
    # Elbow plot to determine the number of clusters for kmeans
    sse <- c()
    centers <- c()
    for (k in 2:10) {
        kmeans.out <- kmeans(scale(marks), centers = k, iter.max = 20, nstart = 3)
        sse[k] <- kmeans.out$tot.withinss
        centers[k] <- k
    }
    plot(centers, sse)
    lines(centers, sse)
}

my_inspect <- function(rules_list, mean_marks) {
    for (cluster_id in 1:length(rules_list)) {
        print(paste("CLUSTER - ", cluster_id))
        print(paste("Mean total - ", round(mean_marks[cluster_id])))
        inspect(sort(rules_list[[cluster_id]], by = "lift"))
    }
}

students.data <- loadSSLCData();
students <- clusterAndClassify(students.data$allrecords, 4)
nrow(students.data$allrecords)
my_inspect(students$rules, students$mean_marks)
plotClusterMarks(students.data$allrecords, students$kmeans.out)

# Urban 
urban.data <- students.data$allrecords[students.data$allrecords$URBAN_RURAL == 'U', ]
urban <- clusterAndClassify(urban.data, 4)
my_inspect(urban$rules, urban$mean_marks)


# Rural
rural.data <- students.data$allrecords[students.data$allrecords$URBAN_RURAL == 'R', ]
rural <- clusterAndClassify(rural.data, 4)
my_inspect(rural$rules, rural$mean_marks)


# Boys
boys.data <- students.data$allrecords[students.data$allrecords$NRC_GENDER_CODE == 'B',]
boys <- clusterAndClassify(boys.data, 4)
my_inspect(boys$rules, boys$mean_marks)


# Girls
# girls.data <- students.data$allrecords[students.data$allrecords$NRC_GENDER_CODE == 'G',]
# girls <- clusterAndClassify(girls.data, 4)
# my_inspect(girls$rules, girls$mean_marks)

# Government schools
govt.data <- students.data$allrecords[students.data$allrecords$SCHOOL_TYPE == 'G',]
#plotElbow(govt.data)
govt <- clusterAndClassify(govt.data, 4)
#plotClusterMarks(govt.data, govt$kmeans.out)



# Aided schools
aided.data <- students.data$allrecords[students.data$allrecords$SCHOOL_TYPE == 'A',]
#plotElbow(aided.data)
aided <- clusterAndClassify(aided.data. 4)
#plotClusterMarks(aided.data, aided$kmeans.out)



# Unaided schools
unaided.data <- students.data$allrecords[students.data$allrecords$SCHOOL_TYPE == 'U',]
#plotElbow(unaided.data)
unaided <- clusterAndClassify(unaided.data, 4)
#plotClusterMarks(unaided.data, unaided$kmeans.out)


# Scheduled caste
sc.data <- students.data$allrecords[students.data$allrecords$NRC_CASTE_CODE == '1',]
#plotElbow(sc.data)
sc <- clusterAndClassify(sc.data, 4)
#plotClusterMarks(sc.data, sc$kmeans.out)


# Scheduled Tribes
st.data <- students.data$allrecords[students.data$allrecords$NRC_CASTE_CODE == '2',]
#plotElbow(st.data)
st <- clusterAndClassify(st.data. 4)
#plotClusterMarks(st.data, st$kmeans.out)


# Category 1
category1.data <- students.data$allrecords[students.data$allrecords$NRC_CASTE_CODE == '3',]
#plotElbow(category1.data)
category1 <- clusterAndClassify(category1.data, 4)
#plotClusterMarks(category1.data, category1$kmeans.out)


# General
general.data <- students.data$allrecords[students.data$allrecords$NRC_CASTE_CODE == '4',]
#plotElbow(general.data)
general <- clusterAndClassify(general.data, 4)
#plotClusterMarks(general.data, general$kmeans.out)


# Kannada
kannada.data <- students.data$allrecords[students.data$allrecords$NRC_MEDIUM == 'K',]
#plotElbow(kannada.data)
kannada <- clusterAndClassify(kannada.data, 4)
#plotClusterMarks(kannada.data, kannada$kmeans.out)


# English
english.data <- students.data$allrecords[students.data$allrecords$NRC_MEDIUM == 'E',]
#plotElbow(english.data)
english <- clusterAndClassify(english.dat, 4a)
#plotClusterMarks(english.data, english$kmeans.out)


# Marathi
marathi.data <- students.data$allrecords[students.data$allrecords$NRC_MEDIUM == 'M',]
#plotElbow(marathi.data)
marathi <- clusterAndClassify(marathi.data, 4)
#plotClusterMarks(marathi.data, marathi$kmeans.out)


# Urdu
urdu.data <- students.data$allrecords[students.data$allrecords$NRC_MEDIUM == 'U',]
#plotElbow(urdu.data)
urdu <- clusterAndClassify(urdu.data, 4)
#plotClusterMarks(urdu.data, urdu$kmeans.out)


# RF
rf.data <- students.data$allrecords[students.data$allrecords$CANDIDATE_TYPE == 'RF', ]
#plotElbow(rf.data)
rf <- clusterAndClassify(rf.data, 4)

# PF
pf.data <- students.data$allrecords[students.data$allrecords$CANDIDATE_TYPE == 'PF', ]
#plotElbow(pf.data)
pf <- clusterAndClassify(pf.data, 4)
my_inspect(pf$rules, pf$mean_marks)

# NSR
nsr.data <- students.data$allrecords[students.data$allrecords$CANDIDATE_TYPE == 'NSR', ]
#plotElbow(nsr.data) # Not even a hint of an elbow :-()
nsr <- clusterAndClassify(nsr.data, 4)
my_inspect(nsr$rules, nsr$mean_marks)

# NSPR
nspr.data <- students.data$allrecords[students.data$allrecords$CANDIDATE_TYPE == 'NSPR', ]
plotElbow(nspr.data) # Not even a hint of an elbow :-()
nspr <- clusterAndClassify(nspr.data, 2)
my_inspect(nspr$rules, nspr$mean_marks)

