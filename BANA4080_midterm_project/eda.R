# Setup -------------------------------------------------------------------
library(tidyverse)
library(here)
library(completejourney)
library(readr)
c(promotions, transactions) %<-% get_data(which = 'both', verbose = FALSE)
setwd('/Users/ezishr/Documents/CINCY/Fall 2024/BANA 4080/BANA4080_midterm_project')

transactions <- transactions %>% mutate(date = as.Date(transaction_timestamp))

# OFFICIAL CODE ------------
# Assume transactions with any discount are redeemed coupons
trans1 <- transactions %>% 
  dplyr::filter(retail_disc>0 | coupon_disc>0 | coupon_match_disc>0) %>%
  select(household_id, store_id, product_id, sales_value:coupon_match_disc, week, date) 

# Join coupons, coupon_redemptions, campaign_description to get full info of coupons
redemptions <- coupons %>%
  inner_join(campaign_descriptions) %>%
  inner_join(coupon_redemptions, relationship = 'many-to-many') %>%
  rename(date = redemption_date)

# Every transaction with redemptions
sample_df <- redemptions %>%
  left_join(trans1, by = c('household_id','product_id','date'), relationship = 'many-to-many')

colSums(is.na(sample_df)) # there are roughly 2M coupons redemptions w/o transaction records

transWithRedemptions <- redemptions %>%
  inner_join(trans1, by = c('household_id','product_id','date'), relationship = 'many-to-many') %>%
  select('coupon_upc', 'product_id', 'store_id', 'campaign_id', "campaign_type", 'start_date', 'end_date', 'household_id', 'date', 'sales_value', 'retail_disc') %>%
  arrange(desc(product_id))

write_csv(transWithRedemptions, 'Final_Transactions_With_Redemptions.csv')






# OLD CODE - NO INCLUDING THESE-----
# 1. Join coupon_redemptions & campaign_descriptions => have the start and end date of the campaign. 
coupon_redemp_description <- coupon_redemptions %>%
  full_join(campaign_descriptions, by = 'campaign_id') %>%
  drop_na(redemption_date)

# 2. Join coupon_redemptions & coupons => Understand which product_ids are associated with each campaign.
coupon_redemp_coupons <- coupon_redemptions %>% 
  left_join(coupons, by=c('campaign_id', 'coupon_upc'), relationship = 'many-to-many') %>%
  drop_na(redemption_date) # matching both coupon_upc and campaign_id

# 3. Join 1. and 2.
coupons1 <- coupon_redemp_description %>% 
  left_join(coupon_redemp_coupons, 
            by=c('campaign_id','household_id','coupon_upc','redemption_date'), 
            relationship = 'many-to-many') %>%
  rename(date = redemption_date)

# 4. Join 3. & transactions
transWithRedemptions <- coupons1 %>% 
  inner_join(trans1, by=c('household_id', 'product_id'), relationship = 'many-to-many') %>%
  dplyr::filter(redemption_date <= end_date & redemption_date >= start_date) # need to confirm

promotions1 <- promotions %>% dplyr::filter(display_location != 0 | mailer_location != 0)


# a. Customer is likely to buy stuff at store-front event w/o coupons ----

# Total sales from transactions with redemption, grouped by product_id and store_id
transWithRedemptions_groupedByProductStoreId <- transWithRedemptions %>%
  group_by(product_id, store_id) %>%
  summarise(total_sales = sum(sales_value))

transWithRedemptions_grouped <- transWithRedemptions_groupedByProductStoreId
#### Check if there is duplicate comb of product_id and store_id in every row
duplicates <- transWithRedemptions_grouped %>%
  dplyr::group_by(product_id, store_id) %>%
  dplyr::summarize(count = n()) %>%
  dplyr::filter(count > 1)
print(duplicates)


# Get unique existing product id and store id from the above
product_info_id <- unique(transWithRedemptions_groupedByProductStoreId$product_id)
store_info_id <- unique(transWithRedemptions_groupedByProductStoreId$store_id)

# Get rid of location 0 (not display) for display_location
promos_display <- promotions %>% 
  select(-(mailer_location:week)) %>% 
  dplyr::distinct(product_id, store_id, display_location, .keep_all = TRUE) %>%
  dplyr::filter(display_location != 0)

# Join display location to transactions with redemptions by keys product_id and store_id
transWithRedemptions_displayLocation <- transWithRedemptions_groupedByProductStoreId %>%
  inner_join(promos_display, by=c('product_id', 'store_id')) %>%
  left_join(products, by='product_id') %>%
  select(-c('manufacturer_id', 'brand','package_size'))

# Get rid of location 0 for mailer_location
promos_mailer <- promotions %>% 
  select(-c(display_location, week)) %>% 
  dplyr::distinct(product_id, store_id, mailer_location, .keep_all = TRUE) %>%
  dplyr::filter(mailer_location != 0)

# Join mailer location to transactions with redemptions by keys product_id and store_id
transWithRedemptions_mailerLocation <- promos_mailer %>%
  inner_join(transWithRedemptions_groupedByProductStoreId, by=c('product_id', 'store_id')) %>%
  left_join(products, by='product_id') %>%
  select(-c('manufacturer_id', 'brand','package_size'))

# Export as csv files
write_csv(transWithRedemptions_mailerLocation, "Transactions_W_Redemptions_Mailer_Location.csv")
write_csv(transWithRedemptions_displayLocation, "Transactions_W_Redemptions_Display_Location.csv")


# d. Customer doesn’t use the coupons if products are not shown in mail (mailer_location) ----
# Promotions with display_location = 0, which is not displayed
promos_display0 <- promotions %>% 
  select(-c(mailer_location, week)) %>% 
  dplyr::filter(display_location == 0) %>%
  dplyr::distinct(product_id, store_id, .keep_all = TRUE)
  
# Join display location = 0 to transactions with redemptions by keys product_id and store_id
transWithRedemptions_displayLocationIs0 <- transWithRedemptions_grouped %>%
  semi_join(promos_display0, by = c("product_id", "store_id")) %>%
  left_join(products, by='product_id') %>%
  select(-c('manufacturer_id', 'brand','package_size'))

# Promotions with mailer_location = 0, which is not displayed
promos_mailer0 <- promotions %>% 
  select(-c(display_location, week)) %>% 
  dplyr::filter(mailer_location == 0) %>%
  dplyr::distinct(product_id, store_id, .keep_all = TRUE)


# Join mailer location to transactions with redemptions by keys product_id and store_id
transWithRedemptions_mailerLocationIs0 <- transWithRedemptions_grouped %>%
  semi_join(promos_mailer0, by = c("product_id", "store_id")) %>%
  left_join(products, by='product_id') %>%
  select(-c('manufacturer_id', 'brand','package_size'))
 

# Export as csv files
write_csv(transWithRedemptions_displayLocationIs0, 'Transactions_W_Redemptions_Not_Displayed.csv')
write_csv(transWithRedemptions_mailerLocationIs0, 'Transactions_W_Redemptions_Not_Mailed.csv')








