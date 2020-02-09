library(tidyverse)
source("./lib/parse_input.r")
source("./lib/results_output.r")
source("./lib/perceptron.r")


inputData <- read_csv("./data/iris.csv")

# Save scatterplot matrix of descriptive features
pdf("iris_scatterplot_matrix_rplot.pdf") 
dev.cur()
pairs(inputData[,1:4], pch=12)
dev.off()

# Split input sample into train and test data
ss <- split_sample(inputData)

node_num <- 50
epoch <- 300
threshold_limit <- 0.5

# Initialize inputs for perceptron class
nn_train <- perceptron(ss@xtrain, ss@ytrain, node_num)

# Iterate through neural network using feedforward and backpropagation methods
for( i in 1:epoch){
  nn_train <- feedforward(nn_train)
  nn_train <- backprop(nn_train)
}

# results_table will reveal is neural net has successfully been trained
yhat_train_result <- threshold(nn_train@yhat_train, 0.5)
results_df_train <- results_table(nn_train@yhat_train, yhat_train_result, ss@ytrain)

# neural net can now be run on test data
results_test <- result(ss@xtest, nn_train@w1, nn_train@w2)
yhat_test_result <- threshold(results_test, 0.5)
results_df_train <- results_table(results_test, yhat_test_result, ss@ytest)







