library(tidyverse)
library(here)
library(completejourney)
library(readr)
c(promotions, transactions) %<-% get_data(which = 'both', verbose = FALSE)
setwd('/Users/ezishr/transactionsDocuments/CINCY/Fall 2024/BANA 4080/BANA4080_midterm_project')

transactions <- transactions %>% mutate(date = as.Date(transaction_timestamp))
promotions %>% group_by(product_id, store_id, display_location) %>%
  summarise(count = n())
promos <- promotions

sample <- promos %>% 
  dplyr::distinct(product_id, store_id, display_location, .keep_all = TRUE) %>%
  select(product_id, store_id, display_location)

