# Setup -------------------------------------------------------------------
library(tidyverse)
library(here)
library(completejourney)
#install.packages("completejourney")

c(promotions, transactions) %<-% get_data(which = 'both', verbose = FALSE)
transactions1 <- transactions %>% mutate(day = as.Date(transaction_timestamp))
length(unique(transactions1$day))
transactions1 %>% group_by(day, household_id) %>% n_distinct()


# left join the coupond redemptions with coupon to get only product_id that is redeemed
sample <- coupon_redemptions %>% left_join(coupons, by=c('coupon_upc','campaign_id'), relationship = 'many-to-many')

