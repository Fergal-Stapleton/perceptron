library(tidyverse)
library(stringr)

results_table <-function(yhat, yhat_rounded, y){
  logical_test <- matrix(0L, dim(y)[1], dim(y)[2])
  logical_test <- ifelse(y == yhat_rounded, "True", "False")
  
  yhat = as.data.frame(yhat)
  yhat_rounded = as.data.frame(yhat_rounded)
  y = as.data.frame(as.matrix(y)) # y is numeric
  logical_test = as.data.frame(logical_test)
  
  yhat %>%
    cbind(yhat_rounded) %>%
    cbind(y) %>%
    cbind(logical_test) -> result_df
  colnames(result_df) <- c("yhat", "yhat_rounded", "y", "logical_test")
  
  View(result_df)

  sum(str_count(result_df$logical_test, "True")) -> true_count
  sum(str_count(result_df$logical_test, "False")) -> false_count
  
  true_count <- as.double(true_count)
  false_count <-as.double(false_count)
  
  success_perc <- true_count/(true_count + false_count)*100
  
  print(paste0("Percentage of rows successfully trained: ", 
               as.character(format(round(success_perc, 2), nsmall = 2)), "%"))
  
  return(result_df)
  
}
