############################################################################
## Script that plots and saves the MSE simulation boxplots as eps files.
## Author: Pablo Morala
###########################################################################

####################################
# 1 - Load all needed libraries
####################################
library(ggplot2)
library(cowplot)
library(reshape)

####################################
# 2 - Define needed functions
####################################
reshapingMSESimulations=function(simulation,h_1,q_taylor){
  rownames(simulation)=c("softplus","tanh","sigmoid")
  n_sim=dim(simulation)[2]
  df=as.data.frame(t(simulation))
  df$q.Taylor=as.factor(rep(q_taylor,n_sim))
  df$h.1=as.factor(rep(h_1,n_sim))
  df=melt(df,id.vars=c("q.Taylor","h.1"))
  names(df)[c(3,4)]=c("Act.Function","MSE")
  return(df)
}

####################################
# 3 - Plot the simulations:
####################################

# Load de data
# The "./MSE Simulations final results" directory is expected to
# contain only the needed simulations  and in a fixed order,
# so we read all the files in that directory.

all_simulation_names <- list.files("./Final MSE Simulations results", full.names = TRUE)

####################################
# 3.1 - "-1,1" scaling method:
####################################

# Load data
simulation1 <- readRDS(all_simulation_names[1])
simulation2 <- readRDS(all_simulation_names[2])
simulation3 <- readRDS(all_simulation_names[3])
simulation4 <- readRDS(all_simulation_names[4])
simulation5 <- readRDS(all_simulation_names[5])
simulation6 <- readRDS(all_simulation_names[6])

# Reshape Data using custom function 
df1 <- reshapingMSESimulations(simulation1, "Hidden neurons = 10", 3)
df2 <- reshapingMSESimulations(simulation2, "Hidden neurons = 10", 5)
df3 <- reshapingMSESimulations(simulation3, "Hidden neurons = 10", 7)
df4 <- reshapingMSESimulations(simulation4, "Hidden neurons = 4", 3)
df5 <- reshapingMSESimulations(simulation5, "Hidden neurons = 4", 5)
df6 <- reshapingMSESimulations(simulation6, "Hidden neurons = 4", 7)

# Joint dataframe
df.first <- rbind(df2, df1, df3, df5, df4, df6)

# Y axis breakpoints
my_breaks <- 10^c(-5,-3,-1,1,3,5,7,9,11)

# Create the plot
plot1 <- ggplot(df.first, aes(x = q.Taylor, y = MSE, fill = Act.Function)) +
  geom_boxplot() +
  facet_grid(h.1 ~ .) +
  labs(fill = "Activation\n Function") +
  xlab("Taylor approximation degree (q)") +
  scale_y_continuous("MSE between NN and obtained PR", breaks = my_breaks,trans = "log10")+
  theme_cowplot(12) +
  theme(panel.grid.major.y = element_line(size = .05, color = "grey", linetype = "dashed"))

plot1

# Save the plot in temporal file
setEPS()
postscript("temporal/MSEBoxplots-1,1.eps")
plot1
dev.off()

####################################
# 3.2 - "0,1" scaling method:
####################################

# Load data
simulation1 <- readRDS(all_simulation_names[7])
simulation2 <- readRDS(all_simulation_names[8])
simulation3 <- readRDS(all_simulation_names[9])
simulation4 <- readRDS(all_simulation_names[10])
simulation5 <- readRDS(all_simulation_names[11])
simulation6 <- readRDS(all_simulation_names[12])

# Reshape Data using custom function 
df1 <- reshapingMSESimulations(simulation1, "Hidden neurons = 10", 3)
df2 <- reshapingMSESimulations(simulation2, "Hidden neurons = 10", 5)
df3 <- reshapingMSESimulations(simulation3, "Hidden neurons = 10", 7)
df4 <- reshapingMSESimulations(simulation4, "Hidden neurons = 4", 3)
df5 <- reshapingMSESimulations(simulation5, "Hidden neurons = 4", 5)
df6 <- reshapingMSESimulations(simulation6, "Hidden neurons = 4", 7)

# Joint dataframe
df.second <- rbind(df2,df1, df3, df5, df4, df6)

# Y axis breakpoints
my_breaks <- 10^c(-5,0,5,10,15,20,25,30)

# Create the plot
plot2 <- ggplot(df.second, aes(x=q.Taylor, y=MSE, fill=Act.Function)) + 
  geom_boxplot() + 
  facet_grid(h.1 ~ .)  + 
  labs(fill = "Activation\n Function") + 
  xlab("Taylor approximation degree (q)") +
  scale_y_continuous("MSE between NN and obtained PR", breaks = my_breaks,trans = "log10")+
  theme_cowplot(12) +
  theme(panel.grid.major.y = element_line( size=.05, color="grey", linetype = "dashed" ))

plot2

# Save the plot in temporal file
setEPS()
postscript("temporal/MSEBoxplots0,1.eps")
plot2
dev.off()
