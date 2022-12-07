#Name: Ashley Worrell
#Assignment: BZAN 542 Project
#Date: 11/28/22
#Model: Random Forest

library(tidyverse)
library(caret)
library(dplyr) 
library(tidyr) 
library(stringr) 
library(GGally)
library(pROC)
library(rpart.plot)


#reading in the cleaned data
fake_job_postings_clean <- read.csv("fake_job_postings_clean.csv")


#remove columns with data type character
fake_jobs_small <- fake_job_postings_clean %>% select(job_id, telecommuting, has_company_logo,has_questions,fraudulent, company_profile_length, description_length, requirements_length, benefits_length,candidatesinprofile,recruitinginprofile,bonusinprofile,careerinprofile,staffinginprofile,experienceinprofile,industryinprofile,dataintitle,entryintitle,assistantintitle,homeintitle,clerkintitle,
                                        payrollintitle,administrativeintitle,ampindescription,timeindescription,positionindescription,projectindescription,ampinrequirments,computerinrequirements,traininginbenefits,environmentinbenefits,lifeinbenefits,employeeinbenefits)


#remove NA's from the columns
fake_jobs_small$job_id[which(is.na(fake_jobs_small$job_id))]<- 0
fake_jobs_small$telecommuting[which(is.na(fake_jobs_small$telecommuting))]<- 0
fake_jobs_small$has_company_logo[which(is.na(fake_jobs_small$has_company_logo))] <- 0
fake_jobs_small$has_questions[which(is.na(fake_jobs_small$has_questions))] <- 0
fake_jobs_small$fraudulent[which(is.na(fake_jobs_small$fraudulent))] <- 0
fake_jobs_small$company_profile_length[which(is.na(fake_jobs_small$company_profile_length))] <- 0
fake_jobs_small$description_length[which(is.na(fake_jobs_small$description_length))] <- 0
fake_jobs_small$requirements_length[which(is.na(fake_jobs_small$requirements_length))] <- 0
fake_jobs_small$benefits_length[which(is.na(fake_jobs_small$benefits_length))] <- 0
fake_jobs_small$candidatesinprofile[which(is.na(fake_jobs_small$candidatesinprofile))] <- 0
fake_jobs_small$recruitinginprofile[which(is.na(fake_jobs_small$recruitinginprofile))] <- 0
fake_jobs_small$bonusinprofile[which(is.na(fake_jobs_small$bonusinprofile))] <- 0
fake_jobs_small$careerinprofile[which(is.na(fake_jobs_small$careerinprofile))] <- 0
fake_jobs_small$staffinginprofile[which(is.na(fake_jobs_small$staffinginprofile))] <- 0
fake_jobs_small$experienceinprofile[which(is.na(fake_jobs_small$experienceinprofile))] <- 0
fake_jobs_small$industryinprofile[which(is.na(fake_jobs_small$industryinprofile))] <- 0
fake_jobs_small$dataintitle[which(is.na(fake_jobs_small$dataintitle))] <- 0
fake_jobs_small$entryintitle[which(is.na(fake_jobs_small$entryintitle))] <- 0
fake_jobs_small$assistantintitle[which(is.na(fake_jobs_small$assistantintitle))] <- 0
fake_jobs_small$homeintitle[which(is.na(fake_jobs_small$homeintitle))] <- 0
fake_jobs_small$clerkintitle[which(is.na(fake_jobs_small$clerkintitle))] <- 0
fake_jobs_small$payrollintitle[which(is.na(fake_jobs_small$payrollintitle))] <- 0
fake_jobs_small$administrativeintitle[which(is.na(fake_jobs_small$administrativeintitle))] <- 0
fake_jobs_small$ampindescription[which(is.na(fake_jobs_small$ampindescription))] <- 0
fake_jobs_small$timeindescription[which(is.na(fake_jobs_small$timeindescription))] <- 0
fake_jobs_small$positionindescription[which(is.na(fake_jobs_small$positionindescription))] <- 0
fake_jobs_small$projectindescription[which(is.na(fake_jobs_small$projectindescription))] <- 0
fake_jobs_small$ampinrequirments[which(is.na(fake_jobs_small$ampinrequirments))] <- 0
fake_jobs_small$computerinrequirements[which(is.na(fake_jobs_small$computerinrequirements))] <- 0
fake_jobs_small$traininginbenefits[which(is.na(fake_jobs_small$traininginbenefits))] <- 0
fake_jobs_small$environmentinbenefits[which(is.na(fake_jobs_small$environmentinbenefits))] <- 0
fake_jobs_small$lifeinbenefits[which(is.na(fake_jobs_small$lifeinbenefits))] <- 0
fake_jobs_small$employeeinbenefits[which(is.na(fake_jobs_small$empoyeeinbenefits))] <- 0

fake_jobs_small$fraudulent <- as.factor(fake_jobs_small$fraudulent)
fake_jobs_small$fraudulent <- ifelse(fake_jobs_small$fraudulent=="0","Yes","No")

#splitting into training and testing data
train.rows <- sample(1:nrow(fake_jobs_small),0.7*nrow(fake_jobs_small))
TRAIN <- fake_jobs_small[train.rows,]
HOLDOUT <- fake_jobs_small[-train.rows,]
summary(TRAIN$fraudulent)
summary(HOLDOUT$fraudulent)

TRAIN <- TRAIN[, colSums(is.na(TRAIN)) == 0]

#Random Forest Model
rfFit <- train(fraudulent ~ ., data = TRAIN, method = "rf",
               trControl = trainControl(method = "cv", number = 5),
               ntree = 10,
               tuneGrid = expand.grid(
                 mtry = c(5,10,20,30)
               )
)
rfFit
#use AUC instead of Accuracy because of class imbalance


rfFit2 <- train(fraudulent ~ ., data = TRAIN, method = "rf",
               trControl = trainControl(method = "cv", number = 5, classProbs = TRUE, summaryFunction = twoClassSummary),
               ntree = 30,
               tuneGrid = expand.grid(
                 mtry = c(5,10,20,30)
               ),
               metric = "ROC"
)
rfFit2  #THIS IS THE BEST MODEL SO FAR
#mtry=10 gives us the highest ROC which equals 0.9675330  and ROCSD equals 0.007346378

rfFit2$results
rfFit2$bestTune



