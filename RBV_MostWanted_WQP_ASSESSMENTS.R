#### GET VOL MON DATA FOR RBV Most Wanted ASSESSMENTS FROM WQP 

# Load Libraries
library(stringr)

# Set WD
setwd("C:/Users/deepuser/Documents/Projects/Bioassessments")

# Set up web service call parameters
base  <- "https://www.waterqualitydata.us/data/"
actv  <- "Activity/search?"
actm  <- "ActivityMetric/search?"
org   <- "organization=CTVOLMON"
prj   <- "project=CT-VOLMON-RBV"
sdate <- "startDateLo=01-01-2016"
edate <- "startDateHi=01-01-2021"
mime  <- "mimeType=csv"
zipf  <- "zip=no"
dprof <- "dataProfile=activityAll"

act_url     <- paste0(base,actv,paste(org,prj,sdate,edate,mime,zipf,dprof,sep="&"))
act_met_url <- paste0(base,actm,paste(org,sdate,edate,mime,zipf,sep="&"))

# Read in data from WQP
activities<- read.csv(act_url,header=TRUE)
metrics   <- read.csv(act_met_url,header=TRUE)

data <- merge(activities,metrics,by="ActivityIdentifier")

# Add in data for assessment table
data$STA_SEQ    <- str_sub(data$MonitoringLocationIdentifier.x,-5)
data$CYCLE      <- 2022
data$SAMPLE     <- "BUG"
data$SAMPLE_YEAR<- str_sub(data$ActivityStartDate,1,4)
data$SAMPLE_TYPE<- "RBV"
data$DATA_USE_FLAG<- 0

# Rename column names
old_names <- c("ActivityMetricType.MetricTypeName",
               "MetricValueMeasure.MeasureValue",
               "OrganizationIdentifier.x",
               "MetricValueMeasure.MetricCommentText")
new_names <- c("METRIC","VALUE","COLLECTOR","COMMENT")

for(i in 1:length(old_names)){
  names(data)[names(data) == old_names[i]] <- new_names[i]
}

# Subset columns needed for assessments
fields <- c("STA_SEQ","CYCLE","SAMPLE","SAMPLE_YEAR","SAMPLE_TYPE","METRIC",
            "VALUE","DATA_USE_FLAG","COLLECTOR","COMMENT")

data <- data[,fields]
data <- data[data$VALUE>3,] # Get only samples > 3 Most Wanted

assess_data <- data.frame(STA_SEQ=numeric(),CYCLE=numeric(),SAMPLE=character(), 
                          SAMPLE_YEAR=numeric(),SAMPLE_TYPE=character(),
                          METRIC=character(),VALUE=numeric(),
                          DATA_USE_FLAG=numeric(),COLLECTOR=character(),
                          COMMENT=character())

assess_data <- rbind(assess_data,data)

