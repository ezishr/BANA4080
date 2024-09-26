# Setup -------------------------------------------------------------------
library(tidyverse)
library(here)
library(completejourney)
#install.packages("completejourney")

c(promotions, transactions) %<-% get_data()
promotions
