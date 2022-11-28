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
fake_jobs_small$employeeinbenefits[which(is.na(fake_jobs_small$employeeinbenefits))] <- 0

fake_jobs_small$candidatesinprofile <- ifelse(fake_jobs_small$candidatesinprofile==0,0,1)
fake_jobs_small$recruitinginprofile <- ifelse(fake_jobs_small$recruitinginprofile==0,0,1)
fake_jobs_small$bonusinprofile <- ifelse(fake_jobs_small$bonusinprofile==0,0,1)
fake_jobs_small$careerinprofile <- ifelse(fake_jobs_small$careerinprofile==0,0,1)
fake_jobs_small$staffinginprofile <- ifelse(fake_jobs_small$staffinginprofile==0,0,1)
fake_jobs_small$experienceinprofile <- ifelse(fake_jobs_small$experienceinprofile==0,0,1)
fake_jobs_small$industryinprofile <- ifelse(fake_jobs_small$industryinprofile==0,0,1)
fake_jobs_small$dataintitle <- ifelse(fake_jobs_small$dataintitle==0,0,1)
fake_jobs_small$entryintitle <- ifelse(fake_jobs_small$entryintitle==0,0,1)
fake_jobs_small$assistantintitle <- ifelse(fake_jobs_small$assistantintitle==0,0,1)
fake_jobs_small$homeintitle <- ifelse(fake_jobs_small$homeintitle==0,0,1)
fake_jobs_small$clerkintitle <- ifelse(fake_jobs_small$clerkintitle==0,0,1)
fake_jobs_small$payrollintitle <- ifelse(fake_jobs_small$payrollintitle==0,0,1)
fake_jobs_small$administrativeintitle <- ifelse(fake_jobs_small$administrativeintitle==0,0,1)
fake_jobs_small$ampindescription <- ifelse(fake_jobs_small$ampindescription==0,0,1)
fake_jobs_small$timeindescription <- ifelse(fake_jobs_small$timeindescription==0,0,1)
fake_jobs_small$positionindescription <- ifelse(fake_jobs_small$positionindescription==0,0,1)
fake_jobs_small$projectindescription <- ifelse(fake_jobs_small$projectindescription==0,0,1)
fake_jobs_small$ampinrequirments <- ifelse(fake_jobs_small$ampinrequirments==0,0,1)
fake_jobs_small$computerinrequirements <- ifelse(fake_jobs_small$computerinrequirements==0,0,1)
fake_jobs_small$traininginbenefits <- ifelse(fake_jobs_small$traininginbenefits==0,0,1)
fake_jobs_small$environmentinbenefits <- ifelse(fake_jobs_small$environmentinbenefits==0,0,1)
fake_jobs_small$lifeinbenefits <- ifelse(fake_jobs_small$lifeinbenefits==0,0,1)
fake_jobs_small$employeeinbenefits <- ifelse(fake_jobs_small$employeeinbenefits==0,0,1)

x <- fake_jobs_small[,c('telecommuting','has_company_logo','has_questions','company_profile_length','description_length','requirements_length','benefits_length','candidatesinprofile','recruitinginprofile','bonusinprofile','careerinprofile','staffinginprofile','experienceinprofile',
                        'industryinprofile','dataintitle','entryintitle','assistantintitle','homeintitle','clerkintitle','payrollintitle','administrativeintitle','ampindescription','timeindescription',
                        'positionindescription','projectindescription','ampinrequirments','computerinrequirements','traininginbenefits','environmentinbenefits','lifeinbenefits','employeeinbenefits')]
x$telecommuting <- factor(x$telecommuting)
x$has_company_logo <- factor(x$has_company_logo)
x$has_questions <- factor(x$has_questions)
x$company_profile_length <- factor(floor(x$company_profile_length/100))
x$description_length <- factor(floor(x$description_length/100))
x$requirements_length <- factor(floor(x$requirements_length/100))
x$benefits_length <- factor(floor(x$benefits_length/100))
x$candidatesinprofile <- factor(x$candidatesinprofile)
x$recruitinginprofile <- factor(x$recruitinginprofile)
x$bonusinprofile <- factor(x$bonusinprofile)
x$careerinprofile <- factor(x$careerinprofile)
x$staffinginprofile <- factor(x$staffinginprofile)
x$experienceinprofile <- factor(x$experienceinprofile)
x$industryinprofile <- factor(x$industryinprofile)
x$dataintitle <- factor(x$dataintitle)
x$entryintitle <- factor(x$entryintitle)
x$assistantintitle <- factor(x$assistantintitle)
x$homeintitle <- factor(x$homeintitle)
x$clerkintitle <- factor(x$clerkintitle)
x$payrollintitle <- factor(x$payrollintitle)
x$administrativeintitle <- factor(x$administrativeintitle)
x$ampindescription <- factor(x$ampindescription)
x$timeindescription <- factor(x$timeindescription)
x$positionindescription <- factor(x$positionindescription)
x$projectindescription <- factor(x$projectindescription)
x$ampinrequirments <- factor(x$ampinrequirments)
x$computerinrequirements <- factor(x$computerinrequirements)
x$traininginbenefits <- factor(x$traininginbenefits)
x$environmentinbenefits <- factor(x$environmentinbenefits)
x$lifeinbenefits <- factor(x$lifeinbenefits)
x$employeeinbenefits <- factor(x$employeeinbenefits)

y <- fake_jobs_small$fraudulent
y <- ifelse(y==0,"real","fraudulent")
y.table <- table(y) 
y.table 
barplot(y.table) 

nTotal=nrow(x)
nTrain=floor(nTotal*(0.6))

train=sample(nTotal, nTrain)

train_control<- trainControl(method="boot", number=100)


x.train = x[train,]
y.train = y[train]
x.test = x[-train,]
y.test = y[-train]

d <- fake_jobs_small
d$telecommuting <- factor(d$telecommuting)
d$has_company_logo <- factor(d$has_company_logo)
d$has_questions <- factor(d$has_questions)
d$company_profile_length <- factor(floor(d$company_profile_length/100))
d$description_length <- factor(floor(d$description_length/100))
d$requirements_length <- factor(floor(d$requirements_length/100))
d$benefits_length <- factor(floor(d$benefits_length/100))
d$candidatesinprofile <- factor(d$candidatesinprofile)
d$recruitinginprofile <- factor(d$recruitinginprofile)
d$bonusinprofile <- factor(d$bonusinprofile)
d$careerinprofile <- factor(d$careerinprofile)
d$staffinginprofile <- factor(d$staffinginprofile)
d$experienceinprofile <- factor(d$experienceinprofile)
d$industryinprofile <- factor(d$industryinprofile)
d$dataintitle <- factor(d$dataintitle)
d$entryintitle <- factor(d$entryintitle)
d$assistantintitle <- factor(d$assistantintitle)
d$homeintitle <- factor(d$homeintitle)
d$clerkintitle <- factor(d$clerkintitle)
d$payrollintitle <- factor(d$payrollintitle)
d$administrativeintitle <- factor(d$administrativeintitle)
d$ampindescription <- factor(d$ampindescription)
d$timeindescription <- factor(d$timeindescription)
d$positionindescription <- factor(d$positionindescription)
d$projectindescription <- factor(d$projectindescription)
d$ampinrequirments <- factor(d$ampinrequirments)
d$computerinrequirements <- factor(d$computerinrequirements)
d$traininginbenefits <- factor(d$traininginbenefits)
d$environmentinbenefits <- factor(d$environmentinbenefits)
d$lifeinbenefits <- factor(d$lifeinbenefits)
d$employeeinbenefits <- factor(d$employeeinbenefits)
d$fraudulent <- ifelse(d$fraudulent==0,"real","fraudulent")
d$fraudulent <- as.factor(d$fraudulent)

fit <- train(x.train, y.train, method='nb', 
             tuneGrid = data.frame(usekernel = FALSE, fL = 0, adjust = 1),
             trControl = trainControl(method = "cv", number = 10)
)
fit

model <- train(fraudulent~.,data=d,trControl=train_control, method="nb")




