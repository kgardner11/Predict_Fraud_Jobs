setwd("~/Desktop/542 Project")
library(readr)
fake_job_postings <- read_csv("fake_job_postings.csv")
library(dplyr)
library(tidyverse)
library(tidytext)
#install.packages("tidytext")

# Change NA ---------------------------------------------------------------
fake_job_postings$department[is.na(fake_job_postings$department)] <- "Not specified"
fake_job_postings$salary_range[is.na(fake_job_postings$salary_range)] <- "Not specified"
fake_job_postings$employment_type[is.na(fake_job_postings$employment_type)] <- "Not specified"
fake_job_postings$required_experience[is.na(fake_job_postings$required_experience)] <- "Not specified"
fake_job_postings$required_education[is.na(fake_job_postings$required_education)] <- "Not specified"
fake_job_postings$industry[is.na(fake_job_postings$industry)] <- "Not specified"


# Total Word Count Columns --------------------------------------------------------------
fake_job_postings$company_profile_length <- nchar(fake_job_postings$company_profile[1:length(fake_job_postings$company_profile)])
fake_job_postings$description_length <- nchar(fake_job_postings$description[1:length(fake_job_postings$description)])
fake_job_postings$requirements_length <- nchar(fake_job_postings$requirements[1:length(fake_job_postings$requirements)])
fake_job_postings$benefits_length <- nchar(fake_job_postings$benefits[1:length(fake_job_postings$benefits)])


# Individual Word Count Columns -------------------------------------------
# Profile
fake_job_postings_profile <- fake_job_postings %>% select(company_profile) %>% drop_na()
profiles_df <- fake_job_postings_profile %>% unnest_tokens(word, company_profile) %>% anti_join(stop_words)
top_words_all_profiles <- profiles_df %>% count(word,sort=TRUE) %>% head(10)
fraud_jobs <- fake_job_postings %>% filter(fraudulent=="1")
fraud_jobs_profile <- fraud_jobs %>% select(job_id, company_profile) %>% drop_na() 
fraud_profiles_df <- fraud_jobs_profile %>% unnest_tokens(word, company_profile) %>% anti_join(stop_words) 
top_words_fraud_profiles <- fraud_profiles_df %>% count(word,sort=TRUE) %>% head(10)
# candidates
candidates_profile <- c()
for(i in 1:nrow(fake_job_postings)){
  candidates_profile[i] <- sum(grepl("candidates",fake_job_postings$company_profile[i]))/nchar(fake_job_postings$company_profile[i])
}
fake_job_postings$candidatesinprofile <- candidates_profile
# recruiting
recruiting_profile <- c()
for(i in 1:nrow(fake_job_postings)){
  recruiting_profile[i] <- sum(grepl("recruiting",fake_job_postings$company_profile[i]))/nchar(fake_job_postings$company_profile[i])
}
fake_job_postings$recruitinginprofile <- recruiting_profile
# bonus
bonus_profile <- c()
for(i in 1:nrow(fake_job_postings)){
  bonus_profile[i] <- sum(grepl("bonus",fake_job_postings$company_profile[i]))/nchar(fake_job_postings$company_profile[i])
}
fake_job_postings$bonusinprofile <- bonus_profile
# career
career_profile <- c()
for(i in 1:nrow(fake_job_postings)){
  career_profile[i] <- sum(grepl("career",fake_job_postings$company_profile[i]))/nchar(fake_job_postings$company_profile[i])
}
fake_job_postings$careerinprofile <- career_profile
# staffing
staffing_profile <- c()
for(i in 1:nrow(fake_job_postings)){
  staffing_profile[i] <- sum(grepl("staffing",fake_job_postings$company_profile[i]))/nchar(fake_job_postings$company_profile[i])
}
fake_job_postings$staffinginprofile <- staffing_profile
# experience
experience_profile <- c()
for(i in 1:nrow(fake_job_postings)){
  experience_profile[i] <- sum(grepl("experience",fake_job_postings$company_profile[i]))/nchar(fake_job_postings$company_profile[i])
}
fake_job_postings$experienceinprofile <- experience_profile
# industry
industry_profile <- c()
for(i in 1:nrow(fake_job_postings)){
  industry_profile[i] <- sum(grepl("industry",fake_job_postings$company_profile[i]))/nchar(fake_job_postings$company_profile[i])
}
fake_job_postings$industryinprofile <- industry_profile

# Title
fake_job_postings_title <- fake_job_postings %>% select(title) %>% drop_na()
titles_df <- fake_job_postings_title %>% unnest_tokens(word, title) %>% anti_join(stop_words)
top_words_all_titles <- titles_df %>% count(word,sort=TRUE) %>% head(10)
fraud_jobs_title <- fraud_jobs %>% select(job_id, title) %>% drop_na() 
fraud_titles_df <- fraud_jobs_title %>% unnest_tokens(word, title) %>% anti_join(stop_words) 
top_words_fraud_titles <- fraud_titles_df %>% count(word,sort=TRUE) %>% head(10)
# data
data_title <- c()
for(i in 1:nrow(fake_job_postings)){
  data_title[i] <- sum(grepl("data",fake_job_postings$title[i]))/nchar(fake_job_postings$title[i])
}
fake_job_postings$dataintitle <- data_title
# entry
entry_title <- c()
for(i in 1:nrow(fake_job_postings)){
  entry_title[i] <- sum(grepl("entry",fake_job_postings$title[i]))/nchar(fake_job_postings$title[i])
}
fake_job_postings$entryintitle <- entry_title
# assistant
assistant_title <- c()
for(i in 1:nrow(fake_job_postings)){
  assistant_title[i] <- sum(grepl("assistant",fake_job_postings$title[i]))/nchar(fake_job_postings$title[i])
}
fake_job_postings$assistantintitle <- assistant_title
# home
home_title <- c()
for(i in 1:nrow(fake_job_postings)){
  home_title[i] <- sum(grepl("home",fake_job_postings$title[i]))/nchar(fake_job_postings$title[i])
}
fake_job_postings$homeintitle <- home_title
#clerk
clerk_title <- c()
for(i in 1:nrow(fake_job_postings)){
  clerk_title[i] <- sum(grepl("clerk",fake_job_postings$title[i]))/nchar(fake_job_postings$title[i])
}
fake_job_postings$clerkintitle <- clerk_title
# payroll
payroll_title <- c()
for(i in 1:nrow(fake_job_postings)){
  payroll_title[i] <- sum(grepl("payroll",fake_job_postings$title[i]))/nchar(fake_job_postings$title[i])
}
fake_job_postings$payrollintitle <- payroll_title
#administrative
administrative_title <- c()
for(i in 1:nrow(fake_job_postings)){
  administrative_title[i] <- sum(grepl("administrative",fake_job_postings$title[i]))/nchar(fake_job_postings$title[i])
}
fake_job_postings$administrativeintitle <- administrative_title

# Description
fake_job_postings_description <- fake_job_postings %>% select(description) %>% drop_na()
descriptions_df <- fake_job_postings_description %>% unnest_tokens(word, description) %>% anti_join(stop_words)
top_words_all_descriptions <- descriptions_df %>% count(word,sort=TRUE) %>% head(10)
fraud_jobs_description <- fraud_jobs %>% select(job_id, description) %>% drop_na() 
fraud_descriptions_df <- fraud_jobs_description %>% unnest_tokens(word, description) %>% anti_join(stop_words) 
top_words_fraud_descriptions <- fraud_descriptions_df %>% count(word,sort=TRUE) %>% head(10)
# amp
amp_description <- c()
for(i in 1:nrow(fake_job_postings)){
  amp_description[i] <- sum(grepl("amp",fake_job_postings$description[i]))/nchar(fake_job_postings$description[i])
}
fake_job_postings$ampindescription <- amp_description
# time
time_description <- c()
for(i in 1:nrow(fake_job_postings)){
  time_description[i] <- sum(grepl("time",fake_job_postings$description[i]))/nchar(fake_job_postings$description[i])
}
fake_job_postings$timeindescription <- time_description
# position
position_description <- c()
for(i in 1:nrow(fake_job_postings)){
  position_description[i] <- sum(grepl("position",fake_job_postings$description[i]))/nchar(fake_job_postings$description[i])
}
fake_job_postings$positionindescription <- position_description
# project
project_description <- c()
for(i in 1:nrow(fake_job_postings)){
  project_description[i] <- sum(grepl("project",fake_job_postings$description[i]))/nchar(fake_job_postings$description[i])
}
fake_job_postings$projectindescription <-project_description

# Requirements
fake_job_postings_requirements <- fake_job_postings %>% select(requirements) %>% drop_na()
requirements_df <- fake_job_postings_requirements %>% unnest_tokens(word, requirements) %>% anti_join(stop_words)
top_words_all_requirements <- requirements_df %>% count(word,sort=TRUE) %>% head(10)
fraud_jobs_requirements <- fraud_jobs %>% select(job_id, requirements) %>% drop_na() 
fraud_requirements_df <- fraud_jobs_requirements %>% unnest_tokens(word, requirements) %>% anti_join(stop_words) 
top_words_fraud_requirements <- fraud_requirements_df %>% count(word,sort=TRUE) %>% head(10)
# amp
amp_requirements <- c()
for(i in 1:nrow(fake_job_postings)){
  amp_requirements[i] <- sum(grepl("amp",fake_job_postings$requirements[i]))/nchar(fake_job_postings$requirements[i])
}
fake_job_postings$ampinrequirments <- amp_requirements
# computer 
computer_requirements <- c()
for(i in 1:nrow(fake_job_postings)){
  computer_requirements[i] <- sum(grepl("computer",fake_job_postings$requirements[i]))/nchar(fake_job_postings$requirements[i])
}
fake_job_postings$computerinrequirements <- computer_requirements

# Benefits 
fake_job_postings_benefits <- fake_job_postings %>% select(benefits) %>% drop_na()
benefits_df <- fake_job_postings_benefits %>% unnest_tokens(word, benefits) %>% anti_join(stop_words)
top_words_all_benefits <- benefits_df %>% count(word,sort=TRUE) %>% head(10)
fraud_jobs_benefits <- fraud_jobs %>% select(job_id, benefits) %>% drop_na() 
fraud_benefits_df <- fraud_jobs_benefits %>% unnest_tokens(word, benefits) %>% anti_join(stop_words) 
top_words_fraud_benefits <- fraud_benefits_df %>% count(word,sort=TRUE) %>% head(10)
# training
training_benefits <- c()
for(i in 1:nrow(fake_job_postings)){
  training_benefits[i] <- sum(grepl("training",fake_job_postings$benefits[i]))/nchar(fake_job_postings$benefits[i])
}
fake_job_postings$traininginbenefits <- training_benefits
# environment
environment_benefits <- c()
for(i in 1:nrow(fake_job_postings)){
  environment_benefits[i] <- sum(grepl("environment",fake_job_postings$benefits[i]))/nchar(fake_job_postings$benefits[i])
}
fake_job_postings$environmentinbenefits <- environment_benefits
# life
life_benefits <- c()
for(i in 1:nrow(fake_job_postings)){
  life_benefits[i] <- sum(grepl("life",fake_job_postings$benefits[i]))/nchar(fake_job_postings$benefits[i])
}
fake_job_postings$lifeinbenefits <- life_benefits
# employee
employee_benefits <- c()
for(i in 1:nrow(fake_job_postings)){
  employee_benefits[i] <- sum(grepl("employee",fake_job_postings$benefits[i]))/nchar(fake_job_postings$benefits[i])
}
fake_job_postings$employeeinbenefits <- employee_benefits

write.csv(fake_job_postings,"fake_job_postings_clean.csv", row.names = FALSE)


