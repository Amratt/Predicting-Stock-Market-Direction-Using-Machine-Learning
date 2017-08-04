# Predicting Stock Market Direction Using Machine Learning

## Research

The research we performed is generally discussed in the background and there was a lot of general research to do just to understand better understand financial markets.  This step in the process was very necessary but also very time consuming.   Time was spent reading previous academic research on modeling financial markets to understand various approaches, as well as acquiring general domain knowledge on the subject. For instance we needed to understand very fundamental topics like what a stock is, what goes into the pricing of a stock, what influences short term price changes, what influences long term price changes, what APIs are available to access the information, what to do with the information once it was queried, etc.  It was important for us to have an understanding of the available data because we were creating our own dataset and this phase included visualizing the financial information.  
## Data Preprocessing

Data preprocessing was performed to transform the data to be more useful for our purposes and to properly scope our project.  The financial data that we used was relatively easy to work with but because we created our own dataset for this project, it still needed to be processed to be used for classification and meet our defined objectives. We also created a training and test set in this phase.  A difficulty with this project was dealing with time series data for classification, which was not thoroughly covered in this class.  The majority of the preprocessing performed was to adequately scope the project for us to deal with lower dimension time series data that related all stocks in a balanced way.  
## Statistical Modeling

Statistical modeling was where the majority of our work was performed.  We wished to explore various models discussed in class and apply those models to the financial market.  We selected the different statistical models learned to apply to this dataset, specfically KNN, Logistic Regression, and Random Forest.  The optimization of our models through cross validation was also performed in this stage.
## Software

R Language

## Libraries Used

library(quantmod)
library(randomForest)
library(TTR)
library(class)
library(tree)
library(SDMTools)
library(ROCR)
