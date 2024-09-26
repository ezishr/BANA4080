# Setup -------------------------------------------------------------------
library(tidyverse)
library(here)
library(completejourney)
#install.packages("completejourney")

<<<<<<< HEAD
c(promotions, transactions) %<-% get_data()
promotions
=======
c(promotions, transactions) %<-% get_data(which = 'both', verbose = FALSE)
>>>>>>> fdee76cec8f460e73108c3fc6cb178c12963f413
