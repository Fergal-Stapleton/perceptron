# Perceptron model on the Iris data set

Simple two layer Perceptron model used on Iris data to classify Iris-setosa and Iris-versicolor. Neural network uses sigmoid function as the activation function. Backpropagation is performed with the derivative of the sigmoid.

The program performs the following steps;

1. Parser function uses unseeded random sample to split a single source csv file into training and test data.
2. Perceptron neural network trains over training data.
3. Output R Data.frame compares predicted values against observed values.
