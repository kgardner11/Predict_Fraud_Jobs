data <- read.csv("fake_job_postings_clean.csv")
library(regclass) 
library(caret)
library(pROC)

data <- data[complete.cases(data),]
data$fraudulent <- as.factor(data$fraudulent)
data$fraudulent <- ifelse(data$fraudulent=="0","Yes","No")


fitControl <- trainControl(method="cv", number=5)
svmGrid <- expand.grid( C=10^seq(-2.5,-1.5,by=.5) )
SVM <- train(fraudulent~company_profile_length+description_length+requirements_length+benefits_length+candidatesinprofile+
                              recruitinginprofile+bonusinprofile+careerinprofile+staffinginprofile+experienceinprofile+industryinprofile+
                              dataintitle+entryintitle+assistantintitle+homeintitle+clerkintitle+payrollintitle+administrativeintitle+
                              ampindescription+timeindescription+positionindescription+projectindescription+ampinrequirments+computerinrequirements+
                              traininginbenefits+environmentinbenefits+lifeinbenefits, data=data, method='svmLinear', tuneGrid = svmGrid,
                            trControl=fitControl, verbose=FALSE, preProc = c("center", "scale")) 
SVM$results[rownames(SVM$bestTune),]

#Accuracy 0.9696354 #SD 0.0003395194



