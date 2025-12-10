# NFL-4th-Down-Analysis'
## Reproducing Results from the NFL 4th-Down-Analysis
**Author**
Jeff Raymond, Marten Shears, and Joseph Bettle
**Date**
12/9/2025
## Introduction
This repository contains the original data and the code needed to reduce the results found in the "NFl-4th-Down-Analysis". Specifically, to run analytics on whether you, as an Offensive Coordinator/Play Caller, should go for it on 4th down. 
## Requirements
**To install the required R packages, run the following code in R:**
Install the data through an R package that then needs to be cleaned through the cleaned data cleaning code due to a large number of variables and exhaustive rows. There is no data attached as the data we used was too big to upload here, but going through the step-by-step process shown here, you will get to the same place and code used in the analysis. The libraries will load the data set into the R workspace. 

**Here are the libraries needed:**
library(rpart)
library(rpart.plot)
library(tidyverse)
library(pROC)
library(reshape2)
library(nflfastR)


## Data
We used the NFL play-by-play data and cleaned the variables that would perfectly predict the play.
There is no data added here, as it will be loaded into the workspace when you load in the libraries. The code provided will clean and subset the data for you to what we used for the analysis. It is still to large to load into here. 

****

## Repoduce
1. Run the football_clean.R code first
2. Run the football_forest.R
3. football_GLM_test.R
4. This will get you all the code and information, and graphs we used


## References
https://nflreadr.nflverse.com/articles/dictionary_pbp.html

