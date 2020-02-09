library(tidyverse)

# Create class method to split sample data into traing and test data
# All columns except last are assumed to be independant variables (predictor)
# Last column is assumed to be dependant (response)
# Therefore Input Data must be in the format of an Analytics Base Table

split_sample <- function(df){
  # Using Sample_frac from dplyr lib takes a percentage of source to use as training data
  # anti_join then takes remaining rows to be used for test data
  train <- sample_frac(df, 0.7)
  test <- anti_join(df, train)
  
  sampleclass <- setClass(
    "Split_sample", 
    slots = c(
      xtrain="tbl_df", 
      ytrain="tbl_df", 
      xtest="tbl_df", 
      ytest="tbl_df"
      )
  )
  
  sampleData <- sampleclass(
    xtrain = train[,-ncol(train)],
    ytrain = train[,ncol(train)],
    xtest = test[,-ncol(test)],
    ytest = test[,ncol(test)]
  )
  
  return(sampleData)
}

