################################
## Function that performs redefines the evaluatePR function to be able 
## to use it for every combination of 2 variables, needed to create 
## in an easier way some of the plots that we will use later.
## Author: Pablo Morala
###############################
evaluatePR2D <- function(x1, x2, betas) {
  # performs the result of the polynomial regression expresion given the betas and their labels.
  
  # join x1 and x2
  x <- c(x1, x2)
  
  response <- betas[1] # this gets the intercept beta_0
  for (i in 2:length(betas)) {
    # here the label is transformed into a vector of the needed length with the index of each variable
    variable_indexes <- as.integer(unlist(strsplit(colnames(betas)[i], ",")))
    
    # Intialize the product as 1 and loop over all the indexes l_j to obtain the product of al the needed variables x
    product <- 1
    for (j in 1:length(variable_indexes)) {
      product <- product * x[variable_indexes[j]]
    }
    # We add to the response the product of those variables with their associated beta
    response <- response + betas[i] * product
  }
  return(response)
}