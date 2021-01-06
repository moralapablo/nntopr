################################
## Function that divides the data in train and test given a proportion
## Author: Pablo Morala
###############################

divideTrainTest <- function(data, train_proportion) {
  index <- sample(1:nrow(data), round(train_proportion * nrow(data)))
  train <- data[index, ]
  test <- data[-index, ]

  output <- vector(mode = "list", length = 2)
  output[[1]] <- train
  output[[2]] <- test
  names(output) <- c("train", "test")
  
  return(output)
}
