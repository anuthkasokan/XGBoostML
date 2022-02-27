library(xgboost)
library(tidyverse)
library(purrr)
library(tidyr)

coronaInfo <- read.csv("Desktop/R-Program/Dataset.csv", header=TRUE)
glimpse(coronaInfo)
coronaInfo <- coronaInfo[-1]
coronaInfo <- coronaInfo[-1]
coronaInfo <- coronaInfo[-1]
coronaInfo <- coronaInfo[-10]
coronaInfo <- coronaInfo[-10]
coronaInfo <- coronaInfo[-11]
coronaInfo <- coronaInfo[-11]
coronaInfo <- coronaInfo[-11]
glimpse(coronaInfo)

table(coronaInfo$`Estimated.Onset.Date`)
table(coronaInfo$`Age.Bracket`)
table(coronaInfo$`Gender`)
table(coronaInfo$`Detected.City`)

#you can see there is a shift in the columns after some rows. The estimated onset date is empty and age is filled into it
# and in age column Gender filled. We need to clean this dataset.
# replace empty values by age. bracket to unite all age values into a single column
coronaInfo$Estimated.Onset.Date <- ifelse(coronaInfo$Estimated.Onset.Date == "", coronaInfo$Age.Bracket, coronaInfo$Estimated.Onset.Date)

# filling the gender values from age column and placing it in the correct position
coronaInfo$Gender[coronaInfo$Age.Bracket =="F" ] <- "F"
coronaInfo$Gender[coronaInfo$Age.Bracket =="M" ] <- "M"

# coronaInfo$Gender <- ifelse(coronaInfo$Age.Bracket =="F", coronaInfo$Age.Bracket, coronaInfo$Gender)
# table(coronaInfo$`Gender`)
# coronaInfo$Gender <- ifelse(coronaInfo$Age.Bracket =="M", coronaInfo$Age.Bracket, coronaInfo$Gender)
# table(coronaInfo$`Gender`)

# removing redundant age bracket column and renaming estimated onset date column to age bracket
coronaInfo <- coronaInfo[-3]
coronaInfo <- coronaInfo %>%
  rename(
    Age.Bracket = Estimated.Onset.Date
  )

glimpse(coronaInfo)

#table(coronaInfo$`Gender`)
#coronaInfo <- unique(coronaInfo[, 1:11])
#glimpse(coronaInfo)

# Filtering target variable for our analysis
# filtering dataset for our analysis - taking only deceased and recovered
coronaInfo_filtered <- filter(coronaInfo, coronaInfo$Current.Status == "Deceased" | coronaInfo$Current.Status == "Recovered")

#table(coronaInfo_train$Current.Status)

glimpse(coronaInfo_filtered)

#table(coronaInfo_filtered$`Gender`)
# converting date to month for supporting our analysis
coronaInfo_filtered$Date.Announced <- format.Date(coronaInfo_filtered$Date.Announced,"%m")
glimpse(coronaInfo_filtered)

#table(coronaInfo_filtered$Age.Bracket)

table(coronaInfo_filtered$`Age.Bracket`)
# converting all non numberic value to NA in the age column for our analysis
coronaInfo_filtered$Age.Bracket <- as.numeric(as.character(coronaInfo_filtered$Age.Bracket))



library(data.table)

bins <-   c(0,30,50,70,110)
## 1-Youth,2-Working,3-Retired,4-Old 
labels <- c(1,2,3,4)
coronaInfo_filtered$Age.Bracket <-as.numeric(cut(coronaInfo_filtered$Age.Bracket, breaks=bins, right = FALSE, labels = labels)) 

barplot(table(coronaInfo_filtered$Age.Bracket),
        main = "Age Distribution ",
        xlab = "Age Group: 1-Youth,2-Working,3-Retired,4-Old",
        ylab = "Count")


#Female =1, Male= 2 -making gender column to numeric
coronaInfo_filtered$Gender[coronaInfo_filtered$Gender =="F" ] <- 1
coronaInfo_filtered$Gender[coronaInfo_filtered$Gender =="M" ] <- 2
coronaInfo_filtered$Gender <- as.numeric(as.character(coronaInfo_filtered$Gender))
barplot(table(coronaInfo_filtered$Gender),
        main = "Gender Distribution ",
        xlab = "Gender: Female =1, Male= 2",
        ylab = "Count")

# Converting Values to numeric as XGBoost only allows numeric values

str(coronaInfo_filtered)
coronaInfo_filtered$Detected.City <- as.factor(coronaInfo_filtered$Detected.City)
coronaInfo_filtered$Detected.City <- as.numeric(coronaInfo_filtered$Detected.City)

coronaInfo_filtered$Detected.District <- as.factor(coronaInfo_filtered$Detected.District)
coronaInfo_filtered$Detected.District <- as.numeric(coronaInfo_filtered$Detected.District)

coronaInfo_filtered$Detected.State <- as.factor(coronaInfo_filtered$Detected.State)
coronaInfo_filtered$Detected.State <- as.numeric(coronaInfo_filtered$Detected.State)
#table(coronaInfo_filtered$State.code)

#Trying another way to remove a column
coronaInfo_filtered <- subset(coronaInfo_filtered, select = -c(State.code))

#table(coronaInfo_filtered$Detected.City)
#coronaInfo_filtered <- na.omit(coronaInfo_filtered)


                                       
coronaInfo_filtered$Current.Status <- as.factor(coronaInfo_filtered$Current.Status)
#coronaInfo_filtered$Current.Status <- as.numeric(coronaInfo_filtered$Current.Status)
coronaInfo_filtered$Current.Status <- as.numeric(coronaInfo_filtered$Current.Status)-1

table(coronaInfo_filtered$Type.of.transmission)
coronaInfo_filtered$Type.of.transmission <- ifelse(coronaInfo_filtered$Type.of.transmission == "", "UNKNOWN", coronaInfo_filtered$Type.of.transmission)
coronaInfo_filtered$Type.of.transmission <- ifelse(coronaInfo_filtered$Type.of.transmission == "TBD", "UNKNOWN", coronaInfo_filtered$Type.of.transmission)

#1- Imported, 2- Local, 3- Unknown
table(coronaInfo_filtered$Type.of.transmission)
coronaInfo_filtered$Type.of.transmission <- as.factor(coronaInfo_filtered$Type.of.transmission)
coronaInfo_filtered$Type.of.transmission <- as.numeric(coronaInfo_filtered$Type.of.transmission)


coronaInfo_filtered$Date.Announced <- as.factor(coronaInfo_filtered$Date.Announced)
coronaInfo_filtered$Date.Announced <- as.numeric(coronaInfo_filtered$Date.Announced)

coronaInfo_filtered$Age.Bracket <- as.numeric(coronaInfo_filtered$Age.Bracket)

coronaInfo_filtered <- na.omit(coronaInfo_filtered)

str(coronaInfo_filtered)
table(coronaInfo_filtered$Current.Status)
# 0- Recovered, 1- Deceased
barplot(table(coronaInfo_filtered$Current.Status),
        main = "Status",
        xlab = "Status of Patient: 0- Recovered, 1- Deceased",
        ylab = "Count")

################Model

## shuffling the dataset
set.seed(1234)
coronaInfo_filtered <- coronaInfo_filtered[sample(1:nrow(coronaInfo_filtered)),]

## Removing the target variable
coronaInfo_noTarget <- subset(coronaInfo_filtered, select = -c(Current.Status))

## Split dataset into testing and training subsets
numberOfTrainingSamples <- round(length(coronaInfo_filtered$Current.Status) * .7)


# training data
train_data <- coronaInfo_noTarget[1:numberOfTrainingSamples,]
train_labels <- coronaInfo_filtered$Current.Status[1:numberOfTrainingSamples]

# testing data
test_data <- coronaInfo_noTarget[-(1:numberOfTrainingSamples),]
test_labels <- coronaInfo_filtered$Current.Status[-(1:numberOfTrainingSamples)]

#put our testing & training data into two seperates Dmatrixs objects
dtrain <- xgb.DMatrix(data = as.matrix(train_data), label= train_labels)
dtest <- xgb.DMatrix(data = as.matrix(test_data), label= test_labels)

# train a model using our training data
model <- xgboost(data = dtrain, # the data   
                 nround = 2, # max number of boosting iterations
                 objective = "binary:logistic")  # the objective function - check whether recover or not

# generate predictions for our held-out testing data
pred <- predict(model, dtest)

# get & print the classification error
err <- mean(as.numeric(pred > 0.5) != test_labels)
print(paste("test-error=", err))

# let's test if we increase the depth in the algorithm, will the error increase or decrease.
model_tuned <- xgboost(data = dtrain, # the data           
                       max.depth = 2, # the maximum depth of each decision tree
                       nround = 1, # max number of boosting iterations
                       objective = "binary:logistic") # the objective function 

# generate predictions for our held-out testing data
pred_tuned <- predict(model_tuned, dtest)

# get & print the classification error
err_tuned <- mean(as.numeric(pred_tuned > 0.5) != test_labels)
print(paste("test-error=", err_tuned))
# we can observe that the error increased when the data is split further. This means that
# the previous depth was optimum depth and increasing depth further will cause inconsistency in the output


#Evaluate the algorithm
conf_matrix <- table(test_labels, pred_tuned)
conf_matrix

performance <- sum(diag(conf_matrix)) / sum(conf_matrix)
performance
# 92.85714% performance
