############################################################################
## Script that generates the plots needed to compare the obtained coefficients
## with the original ones and the polyreg ones.
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
library(reshape)
library(latex2exp)
library(plot3D)

####################################
# 2 - Load and define all needed functions
####################################
source("functions/obtainCoeffsFromWeights.R")
source("functions/evaluatePR.R")
source("functions/generateNormalData.R")
source("functions/scaleData.R")
source("functions/divideTrainTest.R")
source("functions/performExampleAuto.R")
source("functions/renameCoefsPolyreg.R")
source("functions/rescale_coefs.R")
source("functions/evaluatePR2D.R")

####################################
# 3 - Generate data and perform the computations as in the performance examples.
####################################

# Set random seed for reproducibility
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

# Parameters for the NN and the Taylor approximation
h_1 <- 4
q_taylor <- 2
fun <- function(x) log(1 + exp(x)) # Softplus

# To use neuralnet we need to create the formula as follows, Y~. does not work.
# This includes all the variables X:
var.names <- names(train)
formula <- as.formula(paste("Y ~", paste(var.names[!var.names %in% "Y"], collapse = " + ")))

# Train the net:
nn <- neuralnet(formula, data = train, hidden = h_1, linear.output = T, act.fct = fun)

# Generation of the example:
ex.1 <- performExampleFromNN(train, test, nn, fun, q_taylor)

# Check that it works properly
ex.1$plot

####################################
# 4 - Solve the same data (SCALED) as in the example with Polyreg:
####################################

# Get the polynomial.
polynomial <- polyreg::polyFit(train, deg = q_taylor)
polynomial$fit$coefficients

# Predict using test data:
Y_polyreg <- predict(polynomial, test[, -(p + 1)])

# And compare with MSE to Y in test data.
n_test <- dim(test)[1]
sum((test$Y - Y_polyreg)^2) / n_test

# Plot vs test Y:
df.plot <- data.frame(test$Y, Y_polyreg)

plot_polyreg <- ggplot(df.plot, aes(x = test.Y, y = Y_polyreg)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(y = "Original Y") +
  labs(x = "Predicted Y with Polyreg") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Polyreg vs Y") +
  theme_cowplot(12)

plot_polyreg

####################################
# 5 - Comparing the coefficients of our formula
#     and the ones of polyreg with the scaled data.
####################################

new_betas <- ex.1$coeff

# We need to rename the polyreg result with our notation.
polyreg_betas <- renameCoefsPolyreg(polynomial$fit$coefficients)
# And we also reorder them to match our notation.
polyreg_betas <- polyreg_betas[order(factor(names(polyreg_betas), levels = colnames(new_betas)))]

df <- as.data.frame(rbind(new_betas, polyreg_betas))
df$Betas <- c("new betas formula", "new betas polyreg")
df <- melt(df, id.vars = "Betas")
df$Betas <- as.factor(df$Betas)

plot_scaled <- ggplot(df, aes(fill = Betas, y = value, x = variable)) +
  geom_col(position = "dodge") +
  theme_cowplot(12) +
  scale_fill_manual("Legend", values = c("new betas formula" = "orange", "new betas polyreg" = "green")) +
  xlab("Beta coeficcients") +
  ylab("Values") +
  geom_hline(yintercept = 0) +
  theme(axis.text.x = element_text(angle = 65, vjust = 1, hjust = 1))

plot_scaled


####################################
# 6 - Obtain the equivalent coefficients of our formula in the original scale:
####################################

# We need to obtain the parameters used in the scaling:
maxs <- apply(data, 2, max) # obtain the max of each variable
mins <- apply(data, 2, min) # obtain the min of each variable
centers <- mins
scales <- (maxs - mins)

# Use our rescaling function to obtain the coefficients scaled back.
new_betas_rescaled <- rescale_coefs(new_betas,centers,scales)
new_betas_rescaled

####################################
# 7 - Now we check that this has worked properly.
#     To do so, the Y obtained with the original X data and the rescaled betas 
#     should be the same as the Y obtained with the scaled data, the betas obtained
#     in the scaled space, and scaling back the Y to the original scale 
#     with its centers and scales.
#     To check that this has worked, the plot comparing both Y vaneeds to fall in the diagonal.
####################################

# Original scale data:
original_train <- data[as.numeric(rownames(train)), ]
original_test <- data[as.numeric(rownames(test)), ]
n_test <- dim(original_test)[1]
n_train <- dim(original_train)[1]

PR.prediction_rescaled <- rep(0, n_train)
PR.prediction_rescaled_after_computing <- rep(0, n_train)

for (i in 1:n_train) {
  PR.prediction_rescaled[i] <- evaluatePR(original_train[i, seq(p)], new_betas_rescaled)
  PR.prediction_rescaled_after_computing[i] <- evaluatePR(train[i, seq(p)], new_betas)
}

PR.prediction_rescaled_after_computing <- PR.prediction_rescaled_after_computing * scales[p + 1] + centers[p + 1]

# plots to compare results:

df.plot <- data.frame(PR.prediction_rescaled_after_computing, PR.prediction_rescaled)

plot_compare_scales <- ggplot(df.plot, aes(x = PR.prediction_rescaled_after_computing, y = PR.prediction_rescaled)) +
  geom_point() +
  labs(x = "Scaling back after computing Y") +
  labs(y = "Scaling back the coefficients") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  theme_cowplot(12)

plot_compare_scales


####################################
# 8 - Now, we compare the coefficients obtained with our method and scaled back,
#     with the original ones and the obtained by polyreg in the original scale.
####################################

# Apply polyreg to the original data
polynomial_original_scale <- polyreg::polyFit(original_train, deg = q_taylor, )

# We need to rename the polyreg result with our notation.
polyreg_betas <- renameCoefsPolyreg(polynomial_original_scale$fit$coefficients)
# And we also reorder them to match our notation.
polyreg_betas <- polyreg_betas[order(factor(names(polyreg_betas), levels = colnames(new_betas)))]
# Reshape as needed
polyreg_betas=t(as.matrix(polyreg_betas))

# DF con betas originales, polyreg y formula.

df <- as.data.frame(rbind(new_betas_rescaled, polyreg_betas, original_betas))
df$Betas <- c("new betas rescaled formula", "new betas polyreg", "original betas")
df <- melt(df, id.vars = "Betas")
df$Betas <- as.factor(df$Betas)

plot <- ggplot(df, aes(fill = Betas, y = value, x = variable)) +
  geom_col(position = "dodge") +
  theme_cowplot(12) +
  scale_fill_manual("Legend", values = c("new betas rescaled formula" = "orange", "new betas polyreg" = "green", "original betas" = "red")) +
  xlab("Beta coeficcients") +
  ylab("Values") +
  geom_hline(yintercept = 0) +
  theme(axis.text.x = element_text(angle = 65, vjust = 1, hjust = 1))

plot

#####################################

PR.prediction_polyreg_rescaled_data <- rep(0, n_train)
for (i in 1:n_train) {
  PR.prediction_polyreg_rescaled_data[i] <- evaluatePR(original_train[i, seq(p)], polyreg_betas)
}

# plots to compare results:

df.plot <- data.frame(PR.prediction_polyreg_rescaled_data, PR.prediction_rescaled)

plot1 <- ggplot(df.plot, aes(x = PR.prediction_polyreg_rescaled_data, y = PR.prediction_rescaled)) +
  geom_point() +
  labs(x = "Coefficients of polyreg") +
  labs(y = "Scaling back the coefficients") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  theme_cowplot(12)

plot1


####################################
# 9 - Plot 3D surfaces:
####################################

# Create a grid of values for x1 and x2
x1=seq(from=0,to=10,length.out = 100)
x2=seq(from=0,to=10,length.out = 100)

# Obtain the surface for both polynomials. QUIZAS AÑADIR AQUI TB EL POLINO,IO ORGINAK
y_new=matrix(0,length(x1),length(x2))
y_polyreg=matrix(0,length(x1),length(x2))
for (i in 1:length(x1)){
  for (j in 1:length(x2)){
    y_new[i,j]=evaluate_PR_2D(x1[i],x2[j],new_betas_rescaled)
    y_polyreg[i,j]=evaluate_PR_2D(x1[i],x2[j],polyreg_betas)
  }
}

theta_vision=100

par(mfrow=c(1,2))
plot3D::persp3D(x1,x2,y_new, theta=theta_vision, phi = 15, xlab = "x1",ylab = "x2",zlab="y",main="Obtained Polynomial")
plot3D::points3D(original_train[,1], original_train[,2], PR.prediction_rescaled, colvar=NULL, col = "black", bg="red", size = 30, pch = 23, add=T)
plot3D::persp3D(x1,x2,y_polyreg, theta=theta_vision, phi = 15,xlab = "x1",ylab = "x2",zlab="y",main="Original Polynomial")
plot3D::points3D(original_train[,1], original_train[,2], PR.prediction_polyreg_rescaled_data, colvar=NULL, col = "black", bg="red", size = 30, pch = 23, add=T)

####################################
# 10 - Repeat the 3D surfaces but increasing the range:
####################################

x1=seq(from=-100,to=100,length.out = 100)
x2=seq(from=-100,to=100,length.out = 100)

y_new=matrix(0,length(x1),length(x2))
y_polyreg=matrix(0,length(x1),length(x2))
for (i in 1:length(x1)){
  for (j in 1:length(x2)){
    y_new[i,j]=evaluate_PR_2D(x1[i],x2[j],new_betas_rescaled)
    y_polyreg[i,j]=evaluate_PR_2D(x1[i],x2[j],polyreg_betas)
  }
}

par(mfrow=c(1,2))
plot3D::persp3D(x1,x2,y_new, theta=theta_vision, phi = 15, xlab = "x1",ylab = "x2",zlab="y",main="Obtained Polynomial")
plot3D::points3D(original_train[,1], original_train[,2], PR.prediction_rescaled, colvar=NULL, col = "black", bg="red", size = 30, pch = 23, add=T)
plot3D::persp3D(x1,x2,y_polyreg, theta=theta_vision, phi = 15,xlab = "x1",ylab = "x2",zlab="y",main="Original Polynomial")
plot3D::points3D(original_train[,1], original_train[,2], PR.prediction_polyreg_rescaled_data, colvar=NULL, col = "black", bg="red", size = 30, pch = 23, add=T)




