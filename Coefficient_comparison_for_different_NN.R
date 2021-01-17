############################################################################
## This script is based on the one where we compare coefficients, but now our
## goal is to generate different NN for the same data to see if it can obtain
## different polynomials and see how these coefficients relate to the original ones.
## This could explain the discrepancies between the coefficients in the original scale,
## and the original or polyreg ones, if the NN reaches local minima.
## The data will remain the same but the seed for the NN will be changed.
## Author: Pablo Morala
###############################################################################################################
# 1 - Load all needed libraries
####################################
library(gtools)
library(neuralnet)
library(pracma)
library(mvtnorm)
library(ggplot2)
library(cowplot)
library(reshape)
library(latex2exp)
library(plot3D)
library(RColorBrewer)
library(scales)
library(lemon)

####################################
# 2 - Load and define all needed functions
####################################
source("functions/obtainCoeffsFromWeights.R")
source("functions/evaluatePR.R")
source("functions/generateNormalData.R")
source("functions/scaleData.R")
source("functions/divideTrainTest.R")
source("functions/performExampleAuto.R")
source("functions/performExampleFromNN.R")
source("functions/renameCoefsPolyreg.R")
source("functions/rescale_coefs.R")
source("functions/evaluatePR2D.R")
source("functions/plotTaylorAndSynapticPotentials.R")
source("functions/plotSurfaceComparison.R")

####################################
# 3 - Generate data that will be used for all different NNs.
####################################

# Set random seed for reproducibility (This one is for the data generation)
set.seed(12345)

# Parameters for the data generation
n_sample <- 200
p <- 2
q_original <- 2
mean_range <- c(-10, 10)
beta_range <- c(-5, 5)
error_var <- 0.1
scale_method <- "0,1"

# Generate the data:
data_generated <- generateNormalData(n_sample, p, q_original, mean_range, beta_range, error_var)
data <- data_generated$data
original_betas <- data_generated$original_betas

# Scale the data in the desired interval and separate train and test ####
data_scaled <- scaleData(data, scale_method)
aux <- divideTrainTest(data_scaled, train_proportion = 0.75)
train <- aux$train
test <- aux$test

####################################
# 4 - Loop with different NN computations
####################################

# Parameters for the NN and the Taylor approximation
h_1 <- 4
q_taylor <- 2
fun <- function(x) log(1 + exp(x)) # Softplus
# To use neuralnet we need to create the formula as follows, Y~. does not work.
# This includes all the variables X:
var.names <- names(train)
formula <- as.formula(paste("Y ~", paste(var.names[!var.names %in% "Y"], collapse = " + ")))

# number of NN trained with the same parameters.
N <- 4
examples <- vector(mode = "list", length = 10)
# List of seeds to be used in order to obtain examples where the PR fits properly the NN
# These are chosen to represent different situations
my_seeds <- c(5, 623, 7345, 81)

for (i in 1:N) {
  # Train the net:
  set.seed(my_seeds[i])
  nn <- neuralnet(formula, data = train, hidden = h_1, linear.output = T, act.fct = fun)

  # Generation of the example:
  examples[[i]] <- performExampleFromNN(train, test, nn, fun, q_taylor)
}


####################################
# 5 - Generate the performance plot for each example.
####################################
# Empty list to store the plots
examples_plot <- vector(mode = "list", length = 9)

# Check that it works properly by adding the synaptic potential plots:
for (i in 1:N) {
  # Create the Taylor plot with the input values:
  x <- seq(-5, 5, length.out = 1000)
  tol <- 0.1
  title <- "Taylor approximation and input values distribution"

  plot_taylor <- plotTaylorAndSynapticPotentials(examples[[i]], fun, x, tol, q_taylor, title)

  # Combine the plots
  plot <- plot_grid(examples[[i]]$plot, plot_taylor, labels = c("", "C"), nrow = 2)

  examples_plot[[i]] <- plot
}



plot_all_examples <- plot_grid(examples_plot[[1]],
  examples_plot[[2]],
  examples_plot[[3]],
  examples_plot[[4]],
  labels = c("(1)", "(2)", "(3)", "(4)"),
  scale = 0.9
)


# Save the plot as eps file
setEPS()
postscript("temporal/Performance_examples_for_Coef_comparison.eps")
plot_all_examples
dev.off()


####################################
# 6 - Obtain the equivalent coefficients of our formula in the original scale:
####################################

# As we have the same data for all the NN,
# we need to obtain the parameters used in the scaling only once:
maxs <- apply(data, 2, max) # obtain the max of each variable
mins <- apply(data, 2, min) # obtain the min of each variable
centers <- mins
scales <- (maxs - mins)

# list containing the betas obtained in each NN:
new_betas <- vector(mode = "list", length = 9)
new_betas_rescaled <- vector(mode = "list", length = 9)
for (i in 1:N) {
  new_betas[[i]] <- examples[[i]]$coeff
  # Use our rescaling function to obtain the coefficients scaled back.
  new_betas_rescaled[[i]] <- rescale_coefs(examples[[i]]$coeff, centers, scales)
}

####################################
# 7 - Now we check that this has worked properly.
#     To do so, the Y obtained with the original X data and the rescaled betas
#     should be the same as the Y obtained with the scaled data, the betas obtained
#     in the scaled space, and scaling back the Y to the original scale
#     with its centers and scales.
#     To check that this has worked, the plot comparing both Y needs to fall in the diagonal.
####################################

# Original scale data:
original_train <- data[as.numeric(rownames(train)), ]
original_test <- data[as.numeric(rownames(test)), ]
n_test <- dim(original_test)[1]
n_train <- dim(original_train)[1]

plots_compare_scales <- vector(mode = "list", length = 9)
for (j in 1:N) {
  PR.prediction_rescaled <- rep(0, n_train)
  PR.prediction_rescaled_after_computing <- rep(0, n_train)

  for (i in 1:n_train) {
    PR.prediction_rescaled[i] <- evaluatePR(original_train[i, seq(p)], new_betas_rescaled[[j]])
    PR.prediction_rescaled_after_computing[i] <- evaluatePR(train[i, seq(p)], new_betas[[j]])
  }

  PR.prediction_rescaled_after_computing <- PR.prediction_rescaled_after_computing * scales[p + 1] + centers[p + 1]

  # plots to compare results:

  df.plot <- data.frame(PR.prediction_rescaled_after_computing, PR.prediction_rescaled)

  plots_compare_scales[[j]] <- ggplot(df.plot, aes(x = PR.prediction_rescaled_after_computing, y = PR.prediction_rescaled)) +
    geom_point() +
    labs(x = "Scaling back after computing Y") +
    labs(y = "Scaling back the coefficients") +
    theme(plot.title = element_text(hjust = 0.5)) +
    geom_abline(slope = 1, intercept = 0, color = "red") +
    theme_cowplot(12)
}


plot_all_scale_comparisons <- plot_grid(plots_compare_scales[[1]],
  plots_compare_scales[[2]],
  plots_compare_scales[[3]],
  plots_compare_scales[[4]],
  labels = ""
)

plot_all_scale_comparisons

####################################
# 8 - Now, we compare the coefficients obtained with our method and scaled back,
#     with the original ones and the obtained by polyreg in the original scale.
####################################

# Apply polyreg to the original data
polynomial_original_scale <- polyreg::polyFit(original_train, deg = q_taylor)

# We need to rename the polyreg result with our notation.
polyreg_betas <- renameCoefsPolyreg(polynomial_original_scale$fit$coefficients)
# And we also reorder them to match our notation.
polyreg_betas <- polyreg_betas[order(factor(names(polyreg_betas), levels = colnames(new_betas_rescaled[[1]])))]
# Reshape as needed
polyreg_betas <- t(as.matrix(polyreg_betas))

# Data frame with new betas, polyreg betas and original ones
df <- as.data.frame(rbind(
  new_betas_rescaled[[1]],
  new_betas_rescaled[[2]],
  new_betas_rescaled[[3]],
  new_betas_rescaled[[4]],
  polyreg_betas,
  original_betas
))
df$Coefficients <- c(
  "Example NN 1",
  "Example NN 2",
  "Example NN 3",
  "Example NN 4",
  "Polyreg",
  "Original"
)

df <- melt(df, id.vars = "Coefficients")
df$Betas <- as.factor(df$Coefficients)


# Set up color palette to retain the 2 last colors for the only polyreg comparison
my_colors <- scales::hue_pal()(6) #using default ggplot2 colors
#my_colors <- brewer.pal(n = 7, name = "Dark2")[c(1,2,3,4,5,6)] # Using RColorBrewer

plot_coeff_comparison <- ggplot(df, aes(fill = Coefficients, y = value, x = variable)) +
  geom_col(position = "dodge") +
  theme_cowplot(12) +
  xlab("Coefficients") +
  ylab("Values") +
  scale_fill_manual(values = my_colors) +
  labs(fill = "Obtained from") +
  geom_hline(yintercept = 0) +
  theme(axis.text.x = element_text(angle = 65, vjust = 1, hjust = 1))

plot_coeff_comparison

# Save the plot as eps file
# setEPS()
# postscript("temporal/Coef_comparison_all.eps")
# plot_coeff_comparison
# dev.off()


# we can repeat this only with polyreg and the original ones:

df <- as.data.frame(rbind(
  polyreg_betas,
  original_betas
))
df$Coefficients <- c(
  "Polyreg",
  "Original"
)

df <- melt(df, id.vars = "Coefficients")
df$Betas <- as.factor(df$Coefficients)

plot_coeff_comparison_only_polyreg <- ggplot(df, aes(fill = Coefficients, y = value, x = variable)) +
  geom_col(position = "dodge") +
  theme_cowplot(12) +
  xlab("Coefficients") +
  ylab("Values") +
  scale_fill_manual(values = my_colors[(N+1):(N+2)]) +
  labs(fill = "Obtained from") +
  geom_hline(yintercept = 0) +
  theme(axis.text.x = element_text(angle = 65, vjust = 1, hjust = 1))

plot_coeff_comparison_only_polyreg

# Save the plot as eps file
# setEPS()
# postscript("temporal/Coef_comparison_only_polyreg.eps")
# plot_coeff_comparison_only_polyreg
# dev.off()

#####
# Combine both previous plots
#####

# Remove the legend from the second plot
plot_coeff_comparison_only_polyreg <- plot_coeff_comparison_only_polyreg + theme(legend.position="none")

# Reposition the legend in the first one.
plot_coeff_comparison <- lemon::reposition_legend(plot_coeff_comparison, 'top right')

final_coeff_comparison <- plot_grid(plot_coeff_comparison,
                                    plot_coeff_comparison_only_polyreg,
                                    labels = c("A", "B"),
                                    scale = 1
                                    )
final_coeff_comparison


# Save the plot as eps file
setEPS()
postscript("temporal/Coef_comparison.eps")
final_coeff_comparison
dev.off()

####################################
# 9 - Plot 3D surfaces with input range and increased range:
####################################

# We need to give the same notationto the original betas
original_betas <- matrix(original_betas, nrow = 1)
colnames(original_betas) <- colnames(polyreg_betas)

# Create a grid of values for x1 and x2 in the input range
x1 <- seq(from = 1.5, to = 7, length.out = 100)
x2 <- seq(from = 4.5, to = 11, length.out = 100)

# Create a grid of values for x1 and x2 in an extended range (denoted as _big)
x1_big <- seq(from = -50, to = 50, length.out = 100)
x2_big <- seq(from = -50, to = 50, length.out = 100)

# Obtain the surface for all the polynomials:

# Input range: Initiate variables
y_new1 <- matrix(0, length(x1), length(x2))
y_new2 <- matrix(0, length(x1), length(x2))
y_new3 <- matrix(0, length(x1), length(x2))
y_new4 <- matrix(0, length(x1), length(x2))
y_polyreg <- matrix(0, length(x1), length(x2))
y_original <- matrix(0, length(x1), length(x2))
# Extended range: Initiate variables
y_new1_big <- matrix(0, length(x1_big), length(x2_big))
y_new2_big <- matrix(0, length(x1_big), length(x2_big))
y_new3_big <- matrix(0, length(x1_big), length(x2_big))
y_new4_big <- matrix(0, length(x1_big), length(x2_big))
y_polyreg_big <- matrix(0, length(x1_big), length(x2_big))
y_original_big <- matrix(0, length(x1_big), length(x2_big))

# Input range: Loop
for (i in 1:length(x1)) {
  for (j in 1:length(x2)) {
    y_new1[i, j] <- evaluatePR2D(x1[i], x2[j], new_betas_rescaled[[1]])
    y_new2[i, j] <- evaluatePR2D(x1[i], x2[j], new_betas_rescaled[[2]])
    y_new3[i, j] <- evaluatePR2D(x1[i], x2[j], new_betas_rescaled[[3]])
    y_new4[i, j] <- evaluatePR2D(x1[i], x2[j], new_betas_rescaled[[4]])
    y_polyreg[i, j] <- evaluatePR2D(x1[i], x2[j], polyreg_betas)
    y_original[i, j] <- evaluatePR2D(x1[i], x2[j], original_betas)
  }
}
# Extended range: Loop
for (i in 1:length(x1_big)) {
  for (j in 1:length(x2_big)) {
    y_new1_big[i, j] <- evaluatePR2D(x1_big[i], x2_big[j], new_betas_rescaled[[1]])
    y_new2_big[i, j] <- evaluatePR2D(x1_big[i], x2_big[j], new_betas_rescaled[[2]])
    y_new3_big[i, j] <- evaluatePR2D(x1_big[i], x2_big[j], new_betas_rescaled[[3]])
    y_new4_big[i, j] <- evaluatePR2D(x1_big[i], x2_big[j], new_betas_rescaled[[4]])
    y_polyreg_big[i, j] <- evaluatePR2D(x1_big[i], x2_big[j], polyreg_betas)
    y_original_big[i, j] <- evaluatePR2D(x1_big[i], x2_big[j], original_betas)
  }
}

# Obtain also the predictions for the data set points in each case:

PR.prediction_rescaled1 <- rep(0, n_train)
PR.prediction_rescaled2 <- rep(0, n_train)
PR.prediction_rescaled3 <- rep(0, n_train)
PR.prediction_rescaled4 <- rep(0, n_train)
PR.prediction_polyreg_rescaled_data <- rep(0, n_train)

for (i in 1:n_train) {
  PR.prediction_rescaled1[i] <- evaluatePR(original_train[i, seq(p)], new_betas_rescaled[[1]])
  PR.prediction_rescaled2[i] <- evaluatePR(original_train[i, seq(p)], new_betas_rescaled[[2]])
  PR.prediction_rescaled3[i] <- evaluatePR(original_train[i, seq(p)], new_betas_rescaled[[3]])
  PR.prediction_rescaled4[i] <- evaluatePR(original_train[i, seq(p)], new_betas_rescaled[[4]])
  PR.prediction_polyreg_rescaled_data[i] <- evaluatePR(original_train[i, seq(p)], polyreg_betas)
}


# Visuzalization parameters:
my_theta <- 130
my_phi <- 25
my_breaks <- seq(from = -25000, to = 25000, by = 1000)



# IMPORTANT: These following plots are intended to be saved manually.
# Polynomial from NN 1
plotSurfaceComparison(
  x1,
  x2,
  y_new1,
  original_train[, 1],
  original_train[, 2],
  PR.prediction_rescaled1,
  x1_big,
  x2_big,
  y_new1_big,
  my_breaks,
  my_theta,
  my_phi,
  "Example with Neural Network 1"
)


# Polynomial from NN 2
plotSurfaceComparison(
  x1,
  x2,
  y_new2,
  original_train[, 1],
  original_train[, 2],
  PR.prediction_rescaled2,
  x1_big,
  x2_big,
  y_new2_big,
  my_breaks,
  my_theta,
  my_phi,
  "Example with Neural Network 2"
)

# Polynomial from NN 3
plotSurfaceComparison(
  x1,
  x2,
  y_new3,
  original_train[, 1],
  original_train[, 2],
  PR.prediction_rescaled3,
  x1_big,
  x2_big,
  y_new3_big,
  my_breaks,
  my_theta,
  my_phi,
  "Example with Neural Network 3"
)

# Polynomial from NN 4
plotSurfaceComparison(
  x1,
  x2,
  y_new4,
  original_train[, 1],
  original_train[, 2],
  PR.prediction_rescaled4,
  x1_big,
  x2_big,
  y_new4_big,
  my_breaks,
  my_theta,
  my_phi,
  "Example with Neural Network 4"
)

# Polynomial from polyreg
plotSurfaceComparison(
  x1,
  x2,
  y_polyreg,
  original_train[, 1],
  original_train[, 2],
  PR.prediction_polyreg_rescaled_data,
  x1_big,
  x2_big,
  y_polyreg_big,
  my_breaks,
  my_theta,
  my_phi,
  "Example with 'polyreg'"
)

# Original Polynomial
plotSurfaceComparison(
  x1,
  x2,
  y_original,
  original_train[, 1],
  original_train[, 2],
  original_train[, p + 1],
  x1_big,
  x2_big,
  y_original_big,
  my_breaks,
  my_theta,
  my_phi,
  "Original polynomial"
)




