# Setup -------------------------------------------------------------------
library(tidyverse)
library(here)
library(completejourney)
c(promotions, transactions) %<-% get_data(which = 'both', verbose = FALSE)
view(transactions)
view(campaigns)
view(coupon_redemptions)
view(coupons)
view(products)
view(campaign_descriptions)
view(demographics)

transactions <- transactions %>% mutate(date = as.Date(transaction_timestamp))

trans1 <- transactions %>% 
  dplyr::filter(retail_disc>0 | coupon_disc>0 | coupon_match_disc>0) %>%
  select(household_id, product_id, sales_value:coupon_match_disc, week, date) 

# 1. Join coupon_redemptions & campaign_descriptions => have the start and end date of the campaign. 
coupon_redemp_description <- coupon_redemptions %>% 
  full_join(campaign_descriptions, by='campaign_id', relationship = 'many-to-many') %>%
  drop_na(redemption_date)

# 2. Join coupon_redemptions & coupons => Understand which product_ids are associated with each campaign.
coupon_redemp_coupons <- coupon_redemptions %>% 
  full_join(coupons, by=c('campaign_id', 'coupon_upc'), relationship = 'many-to-many') %>%
  drop_na(redemption_date) # matching both coupon_upc and campaign_id

# 3. Join 1. and 2.
coupons1 <- coupon_redemp_description %>% left_join(coupon_redemp_coupons, 
                                                    by=c('campaign_id','household_id','coupon_upc','redemption_date'), 
                                                    relationship = 'many-to-many')

# 4. Join 3. & transactions
sample1 <- coupons1 %>% 
  inner_join(trans1, by=c('household_id', 'product_id'), relationship = 'many-to-many') %>%
  dplyr::filter(redemption_date <= end_date & redemption_date >= start_date) # need to confirm




# a. Customer is likely to buy stuff at store-front event w/o coupons --------
trans_s1 <- transactions %>% mutate(day = as.Date(transaction_timestamp))
length(unique(trans_s1$day))
trans_s1 %>% group_by(day, household_id) %>% n_distinct()


# left join the coupon redemptions with coupon to get only product_id that is redeemed
sample <- coupon_redemptions %>% left_join(coupons, by=c('coupon_upc','campaign_id'), relationship = 'many-to-many')

# b. Stuff at certain display_location with coupons will have the  ----------------


