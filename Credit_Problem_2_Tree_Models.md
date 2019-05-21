Credit Problem - Random Forest & Boosting
================
Daniel Macdonald @talentrics
5/21/2019

### Project Description

This notebook is the first of 3 published for my [MSDS Capstone
Project](https://sps.northwestern.edu/masters/data-science/curriculum-specializations.php)
at Northwestern University.  
Source data: [‘Default of Credit Card Clients Data
Set’](https://archive.ics.uci.edu/ml/datasets/default+of+credit+card+clients)
posted on the UCI Machine Learning Repository.

**Below is a summary of the notebooks published in relation to this
project:**  
\* [EDA & Data
Transformation](https://github.com/talentrics/MSDS_Capstone_Project/blob/master/Credit_Problem_1_EDA.md)  
\* [Random Forest and Gradient Boosting
Analysis](https://github.com/talentrics/MSDS_Capstone_Project/blob/master/Credit_Problem_2_Tree_Models.md)
**(This Notebook)**  
\* [Regression and Principal Components
Analysis](https://github.com/talentrics/MSDS_Capstone_Project/blob/master/Credit_Problem_3_Regression_Models.md)  
\* [Summary: Model Development Guide
(PDF)](https://github.com/talentrics/MSDS_Capstone_Project/blob/master/Credit_Problem_4_Model_Development_Guide.pdf)

### Part 1 Data Overview

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax
for authoring HTML, PDF, and MS Word documents. For more details on
using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that
includes both content as well as the output of any embedded R code
chunks within the document. You can embed an R code chunk like this:

``` r
summary(cars)
```

    ##      speed           dist       
    ##  Min.   : 4.0   Min.   :  2.00  
    ##  1st Qu.:12.0   1st Qu.: 26.00  
    ##  Median :15.0   Median : 36.00  
    ##  Mean   :15.4   Mean   : 42.98  
    ##  3rd Qu.:19.0   3rd Qu.: 56.00  
    ##  Max.   :25.0   Max.   :120.00

## Including Plots

You can also embed plots, for
example:

![](Credit_Problem_2_Tree_Models_files/figure-gfm/pressure-1.png)<!-- -->

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.
