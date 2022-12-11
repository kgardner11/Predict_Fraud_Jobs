# Predict_Fraud_Jobs

This repo contains an example of using R programming language to implement data mining and text mining techniques learned in UTK's BZAN 542 - Data Mining Methods for Business Applications in Fall 2022. 

Collaborators:
Kyrah Gardner
Ashley Worrell
Westena Anderson
Michalea Shofner
Carrie Beth Workman

This project utilizes the "Fake Job Postings" dataset from Kaggle.com, containing 17,880 posted jobs descriptions, of which 866 are considered fraudulent job postings. The purpose of the project was to culminate concepts learned in BZAN 542 in order to successfully create a model to predict whether a job posting is real or fraudulent, mainly by identifying key words within the description. The columns contained within the dataset include job title, location, description, salary range, experience required and various binary variables for attributes such as telecommuting or whether the posting contains a company logo. See the dataset for further explanation.

The data was preprocessed with data cleaning techniques to deal with NA values and creating new columns with word counts in specific columns. The repository contains R code for 7 different data mining techniques: decision tree, generalized linear regression, Naive Bayes, K nearest-neighbors, logistic regression, random forest, and support vector machine (SVM). Word clouds were also created for various columns for real and fraudulent job postings to visualize commonly appearing words and phrases. 

Each model is found in the repository with its respective name as a .R file. Overall, given that the data file contains such a skewed amount of fraudulent job postings compared to a larger amount of real job postings, many of the model results are inconclusive. As a result, the best fitted model was found to be the SVM linear model. Results and discussion can be found in the repository as a .doc file.

In order to run the model, simply import a .csv of pre-cleaned data and replace the name of the file in the code with the desired file name. Some of the column names may need to be changed based on the dataset being used. We recommend using the SVM model as it results in the highest ROC. The wordclouds.Rmd and tree_diagram.R can be used for visualizations. 
