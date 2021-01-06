################################
## Function that performs one of the following three methods to scale the given data:
##    - Scale to the [0,1] interval -> key: "0,1"
##    - Scale to the [1-,1] interval -> key: "-1,1"
##    - Standardizing the data (mean=0 and sd=1) -> key: "standardize"
## Author: Pablo Morala
###############################

scaleData <- function(data, scale_method = "0,1") {
  
  if (scale_method == "0,1") {
    #### Scale the data in the [0,1] interval and separate train and test ####
    
    maxs <- apply(data, 2, max) # obtain the max of each variable
    mins <- apply(data, 2, min) # obtain the min of each variable
    output <- as.data.frame(scale(data, center = mins, scale = maxs - mins))
    
  } else if (scale_method == "-1,1") {
    #### Scale the data in the [-1,1] interval and separate train and test ####
    
    maxs <- apply(data, 2, max) # obtain the max of each variable
    mins <- apply(data, 2, min) # obtain the min of each variable
    output <- as.data.frame(scale(data, center = mins + (maxs - mins) / 2, scale = (maxs - mins) / 2))
    
  } else if (scale_method == "standardize") {
    #### Scale the data to have mean=0 and sd=1 and separate train and test ####
    
    output <- as.data.frame(scale(data, center = TRUE, scale = TRUE))
    
  } else {
    
    print("Non valid method")
    output <- "Non valid method"
    
  }
  
  return(output)
  
}
