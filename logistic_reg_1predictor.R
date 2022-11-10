data <- read.csv("fake_job_postings_clean_copy.csv")
library(regclass) 


M1 <- glm(fraudulent~company_profile_length,data=data,family="binomial")
summary(M1) #not significant

M2 <- glm(fraudulent~description_length,data=data, family="binomial")
summary(M2) #significant
 
M0 <- glm(fraudulent~1,data=data, family="binomial")
1 - (deviance(M2)/deviance(M0))
# R2 is very low (0.06%)

M3 <- glm(fraudulent~requirements_length, data=data, family="binomial")
summary(M3) #significant
1- (deviance(M3)/deviance(M0))
#R2 larger this time (17.88% of variation in y explained)

M4 <- glm(fraudulent~benefits_length, data=data, family="binomial")
summary(M4) #not significant

M5 <- glm(fraudulent~candidatesinprofile, data=data, family = "binomial")
summary(M5) #not significant

M6 <- glm(fraudulent~recruitinginprofile, data=data, family ="binomial")
summary(M6) #not significant

M7 <- glm(fraudulent~bonusinprofile, data=data, family ="binomial")
summary(M7) #significant
1- (deviance(M7)/deviance(M0))
# R2 a lot larger this time (62.86%)

M8 <- glm(fraudulent~careerinprofile, data=data, family ="binomial")
summary(M8) #significant 
1 - (deviance(M8)/deviance(M0))
# R2 larger (60.998%)

M9 <- glm(fraudulent~staffinginprofile, data=data, family ="binomial")
summary(M9) #not significiant

M10 <- glm(fraudulent~experienceinprofile, data=data, family ="binomial")
summary(M10) #significant
1 - (deviance(M10)/deviance(M0))
# R2 large (60.53%)

M11 <- glm(fraudulent~industryinprofile, data=data, family ="binomial")
summary(M11) #significant
1 - (deviance(M11)/deviance(M0))
# R2 large (61.27%) 

M12 <- glm(fraudulent~dataintitle, data=data, family ="binomial")
summary(M12) #not significant

M13 <- glm(fraudulent~entryintitle, data=data, family ="binomial")
summary(M13) #not significant

M14 <- glm(fraudulent~assistantintitle, data=data, family ="binomial")
summary(M14) #significant
1 - (deviance(M14)/deviance(M0))
# R2 tiny (0.0533%)

M15 <- glm(fraudulent~homeintitle, data=data, family ="binomial")
summary(M15) #significant
1 - (deviance(M15)/deviance(M0))
# R2 tiny (0.0631%)

M16 <- glm(fraudulent~clerkintitle, data=data, family ="binomial")
summary(M16) #not significant

M17 <- glm(fraudulent~payrollintitle, data=data, family ="binomial")
summary(M17) #not significant

M18 <- glm(fraudulent~administrativeintitle, data=data, family ="binomial")
summary(M18) #not significant

M19 <- glm(fraudulent~ampindescription, data=data, family ="binomial")
summary(M19) #not significant

M20 <- glm(fraudulent~timeindescription, data=data, family ="binomial")
summary(M20) #significiant
1 - (deviance(M20)/deviance(M0))
# Small R2, 1.09%

M21 <- glm(fraudulent~positionindescription, data=data, family ="binomial")
summary(M21) #significant
1 - (deviance(M21)/deviance(M0))
# Tiny R2, 0.283%

M22 <- glm(fraudulent~projectindescription, data=data, family ="binomial")
summary(M22) # not significant

M23 <- glm(fraudulent~projectindescription, data=data, family ="binomial")
summary(M23) # not significant

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

M27 <- glm(fraudulent~environmentinbenefits, data=data, family ="binomial")
summary(M27) #not significant

M28 <- glm(fraudulent~lifeinbenefits, data=data, family ="binomial")
summary(M28) #significant
1 - (deviance(M28)/deviance(M0))
# R2 okay (41.72%) 

M29 <- glm(fraudulent~employeeinbenefits, data=data, family ="binomial")
summary(M29) #not significant


