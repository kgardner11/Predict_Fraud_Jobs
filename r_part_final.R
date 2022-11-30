data <- read.csv("fake_job_postings_clean.csv")
library(regclass) 
library(caret)
library(pROC)


data <- data[complete.cases(data),]
data$fraudulent <- as.factor(data$fraudulent)
data$fraudulent <- ifelse(data$fraudulent=="0","Yes","No")

fitControl <- trainControl(method="cv", number=5, classProbs=TRUE, summaryFunction=twoClassSummary)
rpartGrid <- expand.grid(cp=10^seq(from=-5,to=-3,by=0.2))
RPART <- train(fraudulent~company_profile_length+description_length+requirements_length+benefits_length+candidatesinprofile+
                 recruitinginprofile+bonusinprofile+careerinprofile+staffinginprofile+experienceinprofile+industryinprofile+
                 dataintitle+entryintitle+assistantintitle+homeintitle+clerkintitle+payrollintitle+administrativeintitle+
                 ampindescription+timeindescription+positionindescription+projectindescription+ampinrequirments+computerinrequirements+
                 traininginbenefits+environmentinbenefits+lifeinbenefits,data=data,method="rpart",
                                                                                        trControl=fitControl, tuneGrid=rpartGrid, preProc=c("center","scale"))
plot(RPART)
bestTuneName <- rownames(RPART$bestTune) 
bestTuneRow <- RPART$results[bestTuneName,]
bestTuneRow

#ROC 0.8922271 #SD 0.0477672

