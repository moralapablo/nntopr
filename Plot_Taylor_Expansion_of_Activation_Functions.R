############################################################################
## Script that plots and saves the Taylor expansion of Activation functions.
## Author: Pablo Morala
###########################################################################

####################################
# 1 - Load all needed libraries
####################################
library(ggplot2)
library(cowplot)

####################################
# 2 - Define needed functions
####################################
taylor_expansion_error <- function(f, q, tol, x, title) {
  # compute the true function
  yf <- f(x)
  # compute the Taylor approximation
  p <- taylor(f, 0, q)
  yp <- polyval(p, x)
  # compute the erro as the absolute value of the difference
  error <- abs(yf - yp)

  # get points to place error bars for error <= tol
  ind <- which(error <= tol)
  error1 <- x[ind[1]]
  error2 <- x[ind[length(ind)]]

  # create the plot:
  df.plot <- data.frame(x, yf, yp, error)

  plot <- ggplot(df.plot, aes(x, yf)) +
    geom_line() +
    geom_line(aes(x, yp), color = "red") +
    geom_line(aes(x, error), color = "blue", linetype = "dashed") +
    geom_hline(yintercept = 0, color = "gray", linetype = "dashed") +
    labs(x = "x") +
    labs(y = "y") +
    geom_vline(xintercept = error1, color = "gray", linetype = "dashed") +
    geom_vline(xintercept = error2, color = "gray", linetype = "dashed") +
    theme_cowplot(12) +
    ggtitle(title) +
    theme(plot.title = element_text(hjust = 0.5))

  return(plot)
}

####################################
# 3 - Plots
####################################


####################################
# 3.1 - AF: Softplus
####################################

# define the AF an the parameters:
f <- function(x) log(1 + exp(x))
q_max <- 8
tol <- 0.1
x <- seq(-5, 5, length.out = 1000)

# Loop over different q values, incrasing 2 at a time and creating each plot:
plots <- vector(mode = "list", length = q_max)
for (q in seq(2, q_max, 2)) {
  title <- paste("Order q =", as.character(q))
  plots[[q]] <- taylor_expansion_error(f, q, tol, x, title) + ylim(-0.5, 5) + theme(plot.title = element_text(size = 10))
}


plot1 <- plot_grid(plots[[2]], plots[[4]], plots[[6]], plots[[8]], labels = c(""))

plot1

# Save the plot as eps file in temporal file:
setEPS()
postscript("temporal/Taylor_expansion_softplus.eps")
plot1
dev.off()

####################################
# 3.2 - AF: Hyperbolic Tangent
####################################

# define the AF an the parameters:
f <- function(x) tanh(x)
q_max = 7
tol=0.1
x <- seq(-3, 3, length.out=1000)

# Loop over different q values, incrasing 2 at a time and creating each plot:
plots <- vector(mode = "list", length = q_max)
for (q in seq(1, q_max, 2)) {
  title <- paste("Order q =", as.character(q))
  plots[[q]] <- taylor_expansion_error(f, q, tol, x, title) + ylim(-1.5,1.5) + theme(plot.title = element_text(size = 10))
}

# Combine all plots together:
plot2 = plot_grid(plots[[1]], plots[[3]],plots[[5]],plots[[7]], labels = c("")) 

plot2

# Save the plot as eps file in temporal file:
setEPS()
postscript("temporal/Taylor_expansion_tanh.eps")
plot2
dev.off()

####################################
# 3.3 - AF: Sigmoid
####################################

# define the AF an the parameters:
f <- function(x) 1/(1+exp(-x))
q_max = 7
tol=0.1
x <- seq(-5, 5, length.out=1000)

# Loop over different q values, incrasing 2 at a time and creating each plot:
plots <- vector(mode = "list", length = q_max)
for (q in seq(1, q_max, 2)) {
  title <- paste("Order q =", as.character(q))
  plots[[q]] <- taylor_expansion_error(f, q, tol, x, title) + ylim(-.5,1.5) + theme(plot.title = element_text(size = 10))
}

# Combine all plots together:
plot3 = plot_grid(plots[[1]], plots[[3]],plots[[5]],plots[[7]], labels = c("")) 

plot3

# Save the plot as eps file in temporal file:
setEPS()
postscript("temporal/Taylor_expansion_sigmoid.eps")
plot3
dev.off()
