############################################################################
## Script that generates some performance examples and saves the results
## plot using `performExampleAuto`.
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
library(latex2exp)


####################################
# 2 - Load all needed functions
####################################
source("functions/obtainCoeffsFromWeights.R")
source("functions/evaluatePR.R")
source("functions/generateNormalData.R")
source("functions/scaleData.R")
source("functions/divideTrainTest.R")
source("functions/performExampleFromNN.R")
source("functions/plotTaylorAndSynapticPotentials.R")

####################################
# 3 - Define function that only needs to change q, h_1, scaling method and function.
#     We also need the interval to adjust the Taylor error plot and the plot title.
#     The parameters for the data generation are fixed inside the function.
####################################
plotPerfomanceExample <- function(my_seed, h_1, q_taylor, fun, scale_method, taylor_interval) {
  # Set random seed for reproducibility
  set.seed(my_seed)

  # Parameters for the data generation
  n_sample <- 200
  p <- 3
  q_original <- 2
  mean_range <- c(-10, 10)
  beta_range <- c(-5, 5)
  error_var <- 0.1

  # Generate the data:
  data_generated <- generateNormalData(n_sample, p, q_original, mean_range, beta_range, error_var)
  data <- data_generated$data
  original_betas <- data_generated$original_betas

  # Scale the data in the desired interval and separate train and test ####

  data_scaled <- scaleData(data, scale_method)

  aux <- divideTrainTest(data_scaled, train_proportion = 0.75)

  train <- aux$train
  test <- aux$test

  # To use neuralnet we need to create the formula as follows, Y~. does not work. This includes all the variables X:
  var.names <- names(train)
  formula <- as.formula(paste("Y ~", paste(var.names[!var.names %in% "Y"], collapse = " + ")))

  # train the net:
  nn <- neuralnet(formula, data = train, hidden = h_1, linear.output = T, act.fct = fun)

  # Generation of the example:
  example <- performExampleFromNN(train, test, nn, fun, q_taylor)

  plot.example <- example$plot

  # Create the Taylor plot with the input values:
  x <- seq(-taylor_interval, taylor_interval, length.out = 1000)
  tol <- 0.1
  title <- "Taylor approximation and input values distribution"

  plot_taylor <- plotTaylorAndSynapticPotentials(example, fun, x, tol, q_taylor, title)

  # Combine the plots
  plot <- plot_grid(plot.example, plot_taylor, labels = c("", "C"), nrow = 2, label_size = 10)

  return(plot)
}
####################################
# 4 - Examples
####################################

####################################
# 4.1 - Example 1
####################################

# Parameters
my_seed <- 12345
h_1 <- 4
q_taylor <- 3
scale_method <- "-1,1"
fun <- function(x) log(1 + exp(x)) # Softplus

# Plot:
taylor_interval <- 5
plot_example_1 <- plotPerfomanceExample(my_seed, h_1, q_taylor, fun, scale_method, taylor_interval)
plot_example_1

# Save the plot as eps file
# setEPS()
# postscript("temporal/Performance_Example_1.eps")
# plot_example_1
# dev.off()

####################################
# 4.2 - Example 2
####################################

# Parameters
my_seed <- 12345
h_1 <- 4
q_taylor <- 3
scale_method <- "-1,1"
fun <- function(x) tanh(x) # Hyperbolic Tangent

# Plot:
taylor_interval <- 2.5
plot_example_2 <- plotPerfomanceExample(my_seed, h_1, q_taylor, fun, scale_method, taylor_interval)
plot_example_2

# Save the plot as eps file

# setEPS()
# postscript("temporal/Performance_Example_2.eps")
# plot_example_2
# dev.off()



####################################
# 5 - Merge Examples to plot in a more compact way.
####################################


plot_together <- plot_grid(plot_example_1, 
                           plot_example_2, 
                           labels = c("(1)", "(2)"),
                           label_size = 12,
                           nrow = 2,
                           scale = 0.9)

plot_together

# Save the plot as eps file
setEPS()
postscript("temporal/Performance_Examples_Together.eps")
plot_together
dev.off()
