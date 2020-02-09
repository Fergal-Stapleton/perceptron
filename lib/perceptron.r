# A simple 2-layer neural network uses Perceptron as the activation function.
#
#          x - Descriptive features, multi-column table later converted to mxn matrix
#          y - Target feature, single column table later converted to 1D matrix
# yhat_train - zero matrix of same dimensions as y
#         w1 - first weight matrix with number of rows matching column 
#              number of x and w1 column number equal to the number of nodes.
#              Initialized with unseeded random variable between 0 and 1
#         w2 - second weight matrix, 1D array with number of rows matching
#              node number
#              Initialized with unseeded random variable between 0 and 1
#         h1 - layer 1 of the heural network. Same number of rows as input 
#              and column number equal to the number of nodes. Initialized with
#              zeros for all elements
#        dw1 - first weight gradient, dimensions match w1. Initialized with
#              zeros for all elements
#        dw2 - first weight gradient, dimensions match w2. Initialized with
#              zeros for all elements

perceptron <- function(input, output, n){
  nnclass <- setClass(
    "neural_net", 
    slots = c(
      x = "matrix",
      y = "matrix",
      yhat_train = "matrix",
      w1 ="matrix", 
      w2 ="matrix",
      h1 ="matrix",
      dw1 ="matrix", 
      dw2 ="matrix"
    )
  )
  
  neural_net <- nnclass(
    x = as.matrix(input),
    y = as.matrix(output),
    yhat_train = matrix(0L, 
                        nrow = dim(as.matrix(output))[1], 
                        ncol = dim(as.matrix(output))[2]),
    w1 = matrix(runif(ncol(input)*n, 0, 1), nrow = ncol(input), ncol = n),
    w2 = matrix(runif(n, 0, 1), nrow = n, ncol=1),
    h1 = matrix(0L, nrow = nrow(input), ncol = n),
    dw1 = matrix(0L, nrow = ncol(input), ncol = n),
    dw2 = matrix(0L, nrow = n, ncol = 1)
  )
  
  setGeneric("feedforward", function(neural_net) 
    standardGeneric("feedforward"))
  
  setGeneric("backprop", function(neural_net) 
    standardGeneric("backprop"))
  
  # Feedfoward method updates h1 and yhat_train matrices 
  setMethod(f = "feedforward", signature = "neural_net",
            function(neural_net){
              neural_net@h1 <- activation(dot(neural_net@x, neural_net@w1))
              neural_net@yhat_train <- activation(dot(neural_net@h1, neural_net@w2))
              return(neural_net)
            })
  
  # Backprop method updates weights and weight gradient matrices
  setMethod(f = "backprop", signature = "neural_net",
            function(neural_net){
              neural_net@dw1 <- dot(t(neural_net@x), 
                                    dot(2*(neural_net@y - neural_net@yhat_train) * 
                                    activation_der(neural_net@yhat_train), t(neural_net@w2)) * 
                                    activation_der(neural_net@h1))
              neural_net@dw2 <- dot(t(neural_net@h1), 
                                    2*(neural_net@y-neural_net@yhat_train) *
                                    activation_der(neural_net@yhat_train))
              neural_net@w1 = neural_net@w1 + neural_net@dw1
              neural_net@w2 = neural_net@w2 + neural_net@dw2
              return(neural_net)
            })

  return(neural_net)
}

# functions

dot <- function(a, b){
  c <- a %*% b
  return(c)
}

# sigmoid function
activation <- function(z){
  sigmoid <- 1/ (1 + exp(-z))
  return(sigmoid)
}

# Derivative of sigmoid function
activation_der <- function(z){
  sigmoid_der <- (activation(z)) * (1 - activation(z))
  return(sigmoid_der)
}

# Threshold function with binay output of 0 or 1 based on a threshold value f
# Kept separate from neural network classes as this can be used on test and train data
threshold <- function(yhat_thresh, f){
  yhat_thresh <- as.matrix(yhat_thresh)
  for(i in 1:nrow(yhat_thresh)){
    if (yhat_thresh[i,] < f){
      yhat_thresh[i,] <- 0L
    } else {
      yhat_thresh[i,] <- 1L
    }
  }
  return(yhat_thresh)
}
  
result <- function(result_x, result_w1, result_w2){
  result_x = as.matrix(result_x)
  result_h1 <- activation(dot(result_x, result_w1))
  yhat_test <- activation(dot(result_h1, result_w2))
  return(yhat_test)
}
  


  

