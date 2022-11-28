data <- read.csv("fake_job_postings_clean.csv")
library(regclass) 

M0 <- glm(fraudulent~1,data=data, family="binomial")

M2 <- glm(fraudulent~description_length,data=data, family="binomial")
summary(M2) #significant
1 - (deviance(M2)/deviance(M0))
# R2 is very low (0.06%)

M3 <- glm(fraudulent~requirements_length, data=data, family="binomial")
summary(M3) #significant
1- (deviance(M3)/deviance(M0))
#R2 larger this time (17.88% of variation in y explained)

M7 <- glm(fraudulent~bonusinprofile, data=data, family ="binomial")
summary(M7) #significant
1- (deviance(M7)/deviance(M0))
# R2 a lot larger this time (62.86%)

M8 <- glm(fraudulent~careerinprofile, data=data, family ="binomial")
summary(M8) #significant 
1 - (deviance(M8)/deviance(M0))
# R2 larger (60.998%)

M10 <- glm(fraudulent~experienceinprofile, data=data, family ="binomial")
summary(M10) #significant
1 - (deviance(M10)/deviance(M0))
# R2 large (60.53%)

M11 <- glm(fraudulent~industryinprofile, data=data, family ="binomial")
summary(M11) #significant
1 - (deviance(M11)/deviance(M0))
# R2 large (61.27%) 

M14 <- glm(fraudulent~assistantintitle, data=data, family ="binomial")
summary(M14) #significant
1 - (deviance(M14)/deviance(M0))
# R2 tiny (0.0533%)

M15 <- glm(fraudulent~homeintitle, data=data, family ="binomial")
summary(M15) #significant
1 - (deviance(M15)/deviance(M0))
# R2 tiny (0.0631%)

M20 <- glm(fraudulent~timeindescription, data=data, family ="binomial")
summary(M20) #significiant
1 - (deviance(M20)/deviance(M0))
# Small R2, 1.09%

M21 <- glm(fraudulent~positionindescription, data=data, family ="binomial")
summary(M21) #significant
1 - (deviance(M21)/deviance(M0))
# Tiny R2, 0.283%

M24 <- glm(fraudulent~ampinrequirments, data=data, family ="binomial")
summary(M24) # significiant
1 - (deviance(M24)/deviance(M0))
# R2 pretty small (17.56%)

M25 <- glm(fraudulent~computerinrequirements, data=data, family ="binomial")
summary(M25) # significant
1 - (deviance(M25)/deviance(M0))
# R2 pretty small (18.60%)

M26 <- glm(fraudulent~traininginbenefits, data=data, family ="binomial")
summary(M26) #significant
1 - (deviance(M26)/deviance(M0))
# R2 okay (41.75%)

M28 <- glm(fraudulent~lifeinbenefits, data=data, family ="binomial")
summary(M28) #significant
1 - (deviance(M28)/deviance(M0))
# R2 okay (41.72%) 

M_ALL_ADDED <- glm(fraudulent~description_length + requirements_length+bonusinprofile+careerinprofile+experienceinprofile+industryinprofile+assistantintitle+homeintitle+timeindescription+positionindescription+ampinrequirments+computerinrequirements+traininginbenefits+lifeinbenefits, data=data, family ="binomial")
summary(M_ALL_ADDED) #significant
1 - (deviance(M_ALL_ADDED)/deviance(M0))
# R2 largest (74.52%)

M_ABOVESIXTY_ADDED <- glm(fraudulent~bonusinprofile+careerinprofile+experienceinprofile+industryinprofile, data=data, family ="binomial")
summary(M_ABOVESIXTY_ADDED) #significant
1 - (deviance(M_ABOVESIXTY_ADDED)/deviance(M0))
# R2 pretty large (65.30%)

M_ABOVEFORTY_ADDED <- glm(fraudulent~bonusinprofile+lifeinbenefits+careerinprofile+experienceinprofile+industryinprofile+timeindescription+traininginbenefits+lifeinbenefits, data=data, family ="binomial")
summary(M_ABOVEFORTY_ADDED) #significant
1 - (deviance(M_ABOVEFORTY_ADDED)/deviance(M0))
# R2 large (74.23%)

par(mfrow=c(2,2))
plot(M_ALL_ADDED)
