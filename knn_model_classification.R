fake_jobs <- read.csv("fake_job_postings_clean_copy.csv")
library(tidyverse)
library(caret) 
library(e1071)
library(caTools)
library(class)



fake_jobs_small <- fake_jobs %>% select(job_id, telecommuting, has_company_logo,has_questions,fraudulent, company_profile_length, description_length, requirements_length, benefits_length,candidatesinprofile,recruitinginprofile,bonusinprofile,careerinprofile,staffinginprofile,experienceinprofile,industryinprofile,dataintitle,entryintitle,assistantintitle,homeintitle,clerkintitle,
                                        payrollintitle,administrativeintitle,ampindescription,timeindescription,positionindescription,projectindescription,ampinrequirments,computerinrequirements,traininginbenefits,environmentinbenefits,lifeinbenefits,employeeinbenefits)

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

split <- sample.split(fake_jobs_small, SplitRatio = 0.7)
train_cl <- subset(fake_jobs_small, split == "TRUE")
test_cl <- subset(fake_jobs_small, split == "FALSE")

train_scale <- scale(train_cl[, 1:4])
test_scale <- scale(test_cl[, 1:4])

classifier_knn1 <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_cl$fraudulent,
                      k = 1)
classifier_knn1
cm1 <- table(test_cl$fraudulent, classifier_knn1)
cm
misClassError <- mean(classifier_knn1 != test_cl$fraudulent)
print(paste('Accuracy =', 1-misClassError))

classifier_knn3 <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_cl$fraudulent,
                      k = 3)
misClassError <- mean(classifier_knn3 != test_cl$fraudulent)
print(paste('Accuracy =', 1-misClassError)) #knn 3 more accurate than knn1

classifier_knn5<- knn(train = train_scale,
                       test = test_scale,
                       cl = train_cl$fraudulent,
                       k = 5)
misClassError <- mean(classifier_knn5 != test_cl$fraudulent)
print(paste('Accuracy =', 1-misClassError)) #knn 5 more accurate than knn3

classifier_knn7 <- knn(train = train_scale,
                       test = test_scale,
                       cl = train_cl$fraudulent,
                       k = 7)
misClassError <- mean(classifier_knn7 != test_cl$fraudulent)
print(paste('Accuracy =', 1-misClassError)) #knn 7 less accurate than knn5



# Summary of model
model1

