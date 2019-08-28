# clear working space
rm(list = ls())

# set r to show 200 digits
options(scipen = 200)

# source all functions
source("data_cleaning.R")
source("estimator.R")
source("utils.R")

#load("ROE.Rdata")

# load packages
library(tidyverse)
library(lubridate)
library(e1071)
library(coin)
library(plyr)
library(quantreg)

# set random seed
set.seed(2019)

# Draw histogram of Log-transformed Absolute ROE
histLogROE(data)

# Calculate kurtosis of ROE data
kurtosis(data$ROE)

# Prepare data for regression
data = read.csv("ROE_data.csv",header = T)
Y <- data$Y
X <- data %>%
  select(-Code, -Year, -Y) %>%
  mutate(ROE = log(abs(ROE))) %>%
  # Normal Score Transformation on X variables
  mutate_at(c("Growth", "Asset", "ATO", "PM"), normal_trafo)

# Set parameters
Tau <- seq(0.1,0.9,0.1)          # Tau sequence
N <- dim(X)[1]                   # Sample size
P <- dim(X)[2]                   # Number of X variables
n <- round(sqrt(N)*log(N))       # Size of pilot sample
B <- 500                         # Number of replications

RPMSE <- c()
for (tau in Tau){
  rq_global <- globalEstimate(as.matrix(X), Y, tau)    # get beta and se for global
  rq_oneshot <- oneshotEstimate(X, Y, tau, index = "data$year")  # get beta and se using oneshot estimation
  
  # to get onestep estimation iterate 500 times
  temp <- sapply(1:B, onestepEstimate, x=X, Y=Y, Tau=tau)
  
  # RPMSE for onestep estimation
  rpmse_onestep <- apply((t(temp)[,1:6] - t(matrix(rep(rq_global$beta, B), 6, B)))^2, 2, mean) / rq_global$se
  rpmse_oneshot <- (rq_oneshot$beta -  rq_global$beta)^2 / rq_global$se
  
  RPMSE <- rbind(RPMSE, c(rpmse_onestep, rpmse_oneshot))
}

save(RPMSE, file = "RPMSE.RData")