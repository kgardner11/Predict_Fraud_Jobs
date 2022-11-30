data <- read.csv("fake_job_postings_clean.csv")
library(regclass) 
library(caret)
library(pROC)

data <- data[complete.cases(data),]
data$fraudulent <- as.factor(data$fraudulent)
data$fraudulent <- ifelse(data$fraudulent=="0","Yes","No")

fitControl <- trainControl(method="cv", number=5, classProbs=TRUE, summaryFunction=twoClassSummary)
GLM1 <- train(fraudulent~company_profile_length,data=data,method="glm",trControl=fitControl, preProc=c("center","scale"),metric="ROC")
GLM1$results #ROC 0.5032335 $SD 0.01872159
GLM2 <- train(fraudulent~description_length,data=data,method="glm",trControl=fitControl, preProc=c("center","scale"),metric="ROC")
GLM2$results #ROC 0.4874014 $SD 0.06778271
GLM3 <- train(fraudulent~requirements_length,data=data,method="glm",trControl=fitControl, preProc=c("center","scale"),metric="ROC")
GLM3$results #ROC 0.5671266 $SD 0.02953356
GLM4 <- train(fraudulent~benefits_length,data=data,method="glm",trControl=fitControl, preProc=c("center","scale"),metric="ROC")
GLM4$results #ROC 0.5290217 $SD 0.08377196
GLM5 <- train(fraudulent~candidatesinprofile,data=data,method="glm",trControl=fitControl, preProc=c("center","scale"),metric="ROC")
GLM5$results #ROC 0.5813976 #SD 0.01215544
GLM6 <- train(fraudulent~recruitinginprofile,data=data,method="glm",trControl=fitControl, preProc=c("center","scale"),metric="ROC")
GLM6$results #ROC 0.5428559 #SD 0.01837276
GLM7 <- train(fraudulent~bonusinprofile,data=data,method="glm",trControl=fitControl, preProc=c("center","scale"),metric="ROC")
GLM7$results #ROC 0.5793117 #SD 0.02876976
GLM8 <- train(fraudulent~careerinprofile,data=data,method="glm",trControl=fitControl, preProc=c("center","scale"),metric="ROC")
GLM8$results #ROC 0.5883892 #SD 0.03574961
GLM9 <- train(fraudulent~staffinginprofile,data=data,method="glm",trControl=fitControl, preProc=c("center","scale"),metric="ROC")
GLM9$results #ROC 0.5075061 #SD 0.002221365
GLM10 <- train(fraudulent~experienceinprofile,data=data,method="glm",trControl=fitControl, preProc=c("center","scale"),metric="ROC")
GLM10$results #ROC 0.5390255 #SD 0.04155846
GLM11 <- train(fraudulent~industryinprofile,data=data,method="glm",trControl=fitControl, preProc=c("center","scale"),metric="ROC")
GLM11$results #ROC 0.6023324 #SD 0.05899599
GLM12 <- train(fraudulent~dataintitle,data=data,method="glm",trControl=fitControl, preProc=c("center","scale"),metric="ROC")
GLM12$results #ROC 0.5 $SD 0
GLM13 <-train(fraudulent~entryintitle,data=data,method="glm",trControl=fitControl, preProc=c("center","scale"),metric="ROC") 
GLM13$results #ROC 0.500565 #SD 0.0006120158
GLM14 <- train(fraudulent~assistantintitle,data=data,method="glm",trControl=fitControl, preProc=c("center","scale"),metric="ROC")
GLM14$results #ROC 0.5001614 #SD 0.0002210341
GLM15 <- train(fraudulent~homeintitle,data=data,method="glm",trControl=fitControl, preProc=c("center","scale"),metric="ROC")
GLM15$results #ROC 0.5 #SD 0
GLM16 <- train(fraudulent~clerkintitle,data=data,method="glm",trControl=fitControl, preProc=c("center","scale"),metric="ROC")
GLM16$results #ROC 0.5 #SD 0
GLM17 <- train(fraudulent~payrollintitle,data=data,method="glm",trControl=fitControl, preProc=c("center","scale"),metric="ROC")
GLM17$results #ROC 0.5 #SD 0
GLM18 <- train(fraudulent~administrativeintitle,data=data,method="glm",trControl=fitControl, preProc=c("center","scale"),metric="ROC")
GLM18$results #ROC 0.5 #SD 0
GLM19 <- train(fraudulent~ampindescription,data=data,method="glm",trControl=fitControl, preProc=c("center","scale"),metric="ROC")
GLM19$results #ROC 0.5328893 #SD 0.02906453
GLM20 <- train(fraudulent~timeindescription,data=data,method="glm",trControl=fitControl, preProc=c("center","scale"),metric="ROC")
GLM20$results #ROC 0.4878881 #SD 0.01765243
GLM21 <- train(fraudulent~positionindescription,data=data,method="glm",trControl=fitControl, preProc=c("center","scale"),metric="ROC")
GLM21$results #ROC 0.506753 #SD 0.03541703
GLM22 <- train(fraudulent~projectindescription,data=data,method="glm",trControl=fitControl, preProc=c("center","scale"),metric="ROC")
GLM22$results #ROC 0.5045148 #SD 0.04901659
GLM23 <- train(fraudulent~ampinrequirments,data=data,method="glm",trControl=fitControl, preProc=c("center","scale"),metric="ROC")
GLM23$results #ROC 0.5447269 #SD 0.03796952
GLM24 <- train(fraudulent~computerinrequirements,data=data,method="glm",trControl=fitControl, preProc=c("center","scale"),metric="ROC")
GLM24$results #ROC 0.5049088 #SD 0.02435655
GLM25 <- train(fraudulent~traininginbenefits,data=data,method="glm",trControl=fitControl, preProc=c("center","scale"),metric="ROC")
GLM25$results #ROC 0.5168448 #SD 0.02445291
GLM26 <- train(fraudulent~environmentinbenefits,data=data,method="glm",trControl=fitControl, preProc=c("center","scale"),metric="ROC")
GLM26$results #ROC 0.6150482 #SD 0.02379952
GLM27 <- train(fraudulent~lifeinbenefits,data=data,method="glm",trControl=fitControl, preProc=c("center","scale"),metric="ROC")
GLM27$results #ROC 0.5772421 #SD 0.03235255

fitControl <- trainControl(method="cv", number=5, classProbs=TRUE, summaryFunction=twoClassSummary)

GLMALL <- train(fraudulent~company_profile_length+description_length+requirements_length+benefits_length+candidatesinprofile+
                  recruitinginprofile+bonusinprofile+careerinprofile+staffinginprofile+experienceinprofile+industryinprofile+
                  dataintitle+entryintitle+assistantintitle+homeintitle+clerkintitle+payrollintitle+administrativeintitle+
                  ampindescription+timeindescription+positionindescription+projectindescription+ampinrequirments+computerinrequirements+
                  traininginbenefits+environmentinbenefits+lifeinbenefits,data=data,method="glm",trControl=fitControl, preProc=c("center","scale"),metric="ROC")
GLMALL$results
#ROC 0.762007 #SD 0.05231161


