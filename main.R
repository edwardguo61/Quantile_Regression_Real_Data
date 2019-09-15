# clear working space
rm(list = ls())

# set r to show 200 digits
options(scipen = 200)

# source all functions
#source("data_cleaning.R")
source("estimator.R")
source("utils.R")
load("ROE.Rdata")

# load packages
library(tidyverse)
library(lubridate)
library(e1071)
library(coin)
library(plyr)
library(quantreg)

# set random seed
set.seed(2)

# draw histogram of Log-transformed Absolute ROE
histLogROE(data)

# Calculate kurtosis of ROE data
kurtosis(data$ROE)

# Prepare data for regression
Y <- data$Y
X <- data %>%
  select(-Code, -Year, -Y) %>%
  mutate(ROE = log(abs(ROE))) %>%
  # Normal Score Transformation on X variables
  mutate_at(c("Growth", "ATO", "PM","Asset"), normal_trafo)

# Set parameters
Tau <- seq(0.1,0.9,0.05)         # Tau sequence
N <- dim(X)[1]                   # Sample size
P <- dim(X)[2]                   # Number of X variables
n <- round(sqrt(N)*log(N))       # Size of pilot sample

# Iterate through all taus and get global/oneshot/onestep estimates and se
# calculate loss for all estimation methods
LOSS <- c()
for (tau in Tau){
  # get beta and se for global
  rq_global <- globalEstimate(as.matrix(X), Y, tau)
  # get beta and se using oneshot estimation
  rq_oneshot <- oneshotEstimate(X, Y, tau, index = "data$Year")  
  # get beta and se using onestep estimation
  rq_onestep <- onestepEstimate(X, Y, tau)
  
  # calculate loss of all quantile regressions
  loss_global <- rqLoss(as.matrix(X), Y, tau, rq_global$beta)
  loss_oneshot <- rqLoss(as.matrix(X), Y, tau, rq_oneshot$beta)
  loss_onestep <- rqLoss(as.matrix(X), Y, tau, rq_onestep$beta)  
  
  LOSS <- rbind(LOSS, c(loss_oneshot, loss_onestep, loss_global))
}
LOSS <- as.data.frame(LOSS)
colnames(LOSS) <- c("loss_oneshot", "loss_onestep","loss_global")

# Generate estimates & se when Tau = 0.5
set.seed(23)
# get beta and se for global
rq_global <- globalEstimate(as.matrix(X), Y, Tau = 0.5)
# get beta and se using oneshot estimation
rq_oneshot <- oneshotEstimate(X, Y, Tau = 0.5, index = "data$Year")  
# get beta and se using onestep estimation
rq_onestep <- onestepEstimate(X, Y, Tau = 0.5)
# write result into data frame
COEF_SE <- rbind("beta_global" = rq_global$beta,
                 "se_global" = rq_global$se,
                 "beta_oneshot" = rq_oneshot$beta,
                 "se_oneshot" = rq_oneshot$se,
                 "beta_onestep" = rq_onestep$beta,
                 "se_onestep" = rq_onestep$se)
write.csv(t(COEF_SE),"Tau0_5.csv")

# Plot DL for all taus
plotDL(LOSS)