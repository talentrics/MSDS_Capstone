# MSDS_Capstone
Capstone project files for Northwestern University MSDS program (2019)
Daniel Macdonald @talentrics
May - 2019

### Project Description

This notebook is the first of 3 published for my [MSDS Capstone Project](https://sps.northwestern.edu/masters/data-science/curriculum-specializations.php){target="_blank"} at Northwestern University.   
The objective of this project is to demonstrate core MSDS programming and data analysis skills. 
   
These notebooks support analysis published in the [Summary: Model Development Guide (PDF)](https://github.com/talentrics/MSDS_Capstone_Project/blob/master/Credit_Problem_4_Model_Development_Guide.pdf){target="_blank"}
   
**Below is a summary of the notebooks published in relation to this project:**  
   
* [EDA & Data Transformation](https://github.com/talentrics/MSDS_Capstone_Project/blob/master/Credit_Problem_1_EDA.md){target="_blank"} 
* [Random Forest and Gradient Boosting Analysis](https://github.com/talentrics/MSDS_Capstone_Project/blob/master/Credit_Problem_2_Tree_Models.md){target="_blank"}  
* [Regression and Principal Components Analysis](https://github.com/talentrics/MSDS_Capstone_Project/blob/master/Credit_Problem_3_Regression_Models.md){target="_blank"}  
    
### Data Overview

* Source: [‘Default of Credit Card Clients Data Set’](https://archive.ics.uci.edu/ml/datasets/default+of+credit+card+clients){target="_blank"} on UCI Machine Learning Repository.   
* The data were provided by a bank in Taiwan in 2016 for purposes of 'default' prediction.
* The data are 30,000 individual customer observations with 30 attributes.
* Observations were over a six month period from April to September of 2005.
* Attributes = available credit, gender, age, marital status, & bill payment. 
* Response variable = 'DEFAULT' - did the customer default (1 = True).   
   
This notebook summarizes Exploratory Data Analysis and Data Tranformation.   
Model development and testing of predictions in notebooks linked above.

### Raw Data Review

Data installed from RData file saved on local computer.  File used for Capstone project is unique compared to raw data on UCI data to apply consistent train/test/validate split.  Project specific file can be found in the [github repository/data.](https://github.com/talentrics/MSDS_Capstone_Project/blob/master/data/credit_card_default.RData){target="_blank"} 
