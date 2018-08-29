# SYS 6018
# Kaggle Competition - Titanic
# mz6jv

library(readr)        # load libraries
library(tidyverse)

train_data <- read_csv("train.csv")      # read in training data

# by quick inspection of the data, it seems there are missing values
miss_age <- length(which(is.na(train_data$Age)))   #177 missing ages
miss_Cabin <- length(which(is.na(train_data$Cabin))) #687 missing cabin
miss_embark <-  length(which(is.na(train_data$Embarked))) #2 missing embarked info



# calculate the average of non-missing ages
age_tot <- 0
age_count <- 0

for (i in 1:length(train_data$Age)){
  if (!is.na(train_data$Age[i])){
    age_tot <- age_tot + train_data$Age[i]
    age_count <- age_count + 1
  }
}

mean_age <- age_tot/age_count

# fill in missing ages using the average 
for (i in 1:length(train_data$Age)){
  if (is.na(train_data$Age[i])){
    train_data$Age[i] <- mean_age
  }
}

# factor categorical variable
train_data$Pclass <- factor(train_data$Pclass)

# try deleting the two entries with missing embarked data
train_data_0 <- train_data[-which(is.na(train_data$Embarked)),]

# subset for cross validation using data after deleting rows missing embarked 
train_index <- sample(1:length(train_data_0$Survived), size=length(train_data_0$Survived)/2) # Select half of the data randomly
train <- train_data_0[train_index,]
valid <- train_data_0[-train_index,] 

# call logistic regression on train data
# eliminate irrelevant variables such as Name 
# model 1
survive_lg_1 <- glm(Survived ~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked, data=train, family = "binomial")
summary(survive_lg_1)
#Parch,Fare and Embarked have insignificant p values, try eliminate them one by one and see if improves model

# model 2
survive_lg_2 <- glm(Survived ~Pclass+Sex+Age+SibSp+Parch+Fare, data=train, family = "binomial")
summary(survive_lg_2)
# AIC decreased, Parch and Fare are still insignificant

# model 3
survive_lg_3 <- glm(Survived ~Pclass+Sex+Age+SibSp+Parch, data=train, family = "binomial")
#survive_lg_3 <- glm(Survived ~.-PassengerId-Name-Ticket-Cabin-Fare-Embarked-Pclass-SibSp, data=train, family = "binomial")
summary(survive_lg_3)
# AIC decreased, Parch insignificant

# model 4
# try remove Parch
survive_lg_4 <- glm(Survived ~Pclass+Sex+Age+SibSp, data=train, family = "binomial")
summary(survive_lg_4)
# AIC decreased, and all remaining variables are significant

# validate model 3 and model 4 on validation dataset
# model 3 - validation
prob_3 <-as.vector(predict(survive_lg_3,newdata=valid, type="response"))
pred_3 <- rep(0,length(valid$Survived))  # Initialize prediction vector
pred_3[prob_3>0.5] <- 1 # assign 1 to probabilities greater than 0.5
table(pred_3,valid$Survived) # among 445 obs, 84 are wrong predictions

# model 4 - validation
prob_4 <-as.vector(predict(survive_lg_4,newdata=valid, type="response"))
pred_4 <- rep(0,length(valid$Survived))  # Initialize prediction vector
pred_4[prob_3>0.5] <- 1 # assign 1 to probabilities greater than 0.5
table(pred_4,valid$Survived) # among 445 obs,84 are wrong predictions

# choose model 4 since all variables included are significant

# build model on entire training data (use the original one because Embarked is not included in model)
survive_lg_full <- glm(Survived ~Pclass+Sex+Age+SibSp, data=train_data, family = "binomial")
summary(survive_lg_full)


# make predictions on test data

# read in test data

test_data <- read_csv("test.csv")

# since Age is one factor, fill in missing values for Age


age_tot_2 <- 0
age_count_2 <- 0

for (i in 1:length(test_data$Age)){
  if (!is.na(test_data$Age[i])){
    age_tot_2 <- age_tot_2 + test_data$Age[i]
    age_count_2 <- age_count_2 + 1
  }
}

mean_age_2 <- age_tot_2/age_count_2

# fill in missing ages using the average 
for (i in 1:length(test_data$Age)){
  if (is.na(test_data$Age[i])){
    test_data$Age[i] <- mean_age_2
  }
}

# factorize Pclass
test_data$Pclass <- factor(test_data$Pclass)

# make prediction using model
prob_new <- as.vector(predict(survive_lg_full,newdata=test_data, type="response"))
pred_new <- rep(0,length(test_data$PassengerId))  # Initialize prediction vector
pred_new[prob_new>0.5] <-  1  # assign 1 to probabilities greater than 0.5
pred_new[1:20]

test_data$Survived <- pred_new
result <- select(test_data,PassengerId,Survived)

write.table(result,file = "mz6jv_titanic_prediction.csv", row.names = F, col.names = T, sep=",")




