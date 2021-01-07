############################################################################
## Script that performs N simulations of automatic performance examples,
## using `performExampleAuto`, saving the MSE, and changing over different
## hyperparameters that are:
##    - The activation function (softplus, tanh, sigmoid).
##    - The number of hidden neurons.
##    - The order of the Taylor approximation q_max.
##    - The scaling mehtod.
## Author: Pablo Morala
###########################################################################

####################################
# 1 - Load all needed libraries
####################################
library(gtools)
library(neuralnet)
library(pracma)
library(mvtnorm)
library(ggplot2)
library(cowplot)


####################################
# 2 - Load all needed functions
####################################
source("functions/obtainCoeffsFromWeights.R")
source("functions/evaluatePR.R")
source("functions/generateNormalData.R")
source("functions/scaleData.R")
source("functions/divideTrainTest.R")
source("functions/performExampleAuto.R")

####################################
# 3 - Set up all parameters used
####################################

# Random seed
set.seed(12345)

# Parameters for the data generation
n_sample <- 200
p <- 3
q_original <- 2
mean_range <- c(-10, 10)
beta_range <- c(-5, 5)
error_var <- 0.1

# Activation functions
my_softplus <- function(x) log(1 + exp(x))
my_tanh <- function(x) tanh(x)
my_sigmoid <- function(x) 1 / (1 + exp(-x))
activation_function_list <- list(softplus = my_softplus, tanh = my_tanh, sigmoid = my_sigmoid)

# Scaling method
scale_method_vector <- c("0,1", "-1,1", "standardize")

# Vector with different Number of hidden neurons in the NN
h_1_vector <- c(4, 10)

# Vector with different max Degree for the Taylor approximation
q_taylor_vector <- c(3, 5, 7)

# Number of simulations for each combination of hyperparameters
n_simulation <- 1


####################################
# 4 - Simulation
####################################

# Loop over different hyperparameters
for (scale_method in scale_method_vector) {
  for (h_1 in h_1_vector) {
    for (q_taylor in q_taylor_vector) {

      # We will store the simulations for the 3 AF at together
      # This is done because of how we will later plot the simulations
      simulations_MSE_all_AF <- NULL

      for (activation_function in activation_function_list) {

        # Loop over number of simulations for a given combination of hyperparameters
        simulations_MSE <- rep(0, n_simulation)

        for (i in 1:n_simulation) {
          example <- performExampleAuto(
            n_sample,
            p,
            q_original,
            mean_range,
            beta_range,
            error_var,
            scale_method,
            h_1,
            activation_function,
            q_taylor
          )

          simulations_MSE[i] <- example$MSE.NN.vs.PR
        }

        # Store MSE values for the given AF with the other AFs
        simulations_MSE_all_AF <- rbind(simulations_MSE_all_AF, simulations_MSE)
      }

      # Generate a name to store the simulation file,
      simulation_name <- paste("temporal/Simulation", scale_method,
        "Hidden", h_1,
        "q_taylor", q_taylor,
        sep = "_")
      
      # Save simulation data
      saveRDS(simulations_MSE_all_AF, simulation_name)
    }
  }
}
