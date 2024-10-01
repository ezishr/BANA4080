# Setup -------------------------------------------------------------------
library(tidyverse)
library(here)
library(completejourney)
library(readr)
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
  full_join(campaign_descriptions, by = 'campaign_id') %>%
  drop_na(redemption_date)

# 2. Join coupon_redemptions & coupons => Understand which product_ids are associated with each campaign.
coupon_redemp_coupons <- coupon_redemptions %>% 
  left_join(coupons, by=c('campaign_id', 'coupon_upc'), relationship = 'many-to-many') %>%
  drop_na(redemption_date) # matching both coupon_upc and campaign_id

# 3. Join 1. and 2.
coupons1 <- coupon_redemp_description %>% left_join(coupon_redemp_coupons, 
                                                    by=c('campaign_id','household_id','coupon_upc','redemption_date'), 
                                                    relationship = 'many-to-many')

# 4. Join 3. & transactions
transWithRedemptions <- coupons1 %>% 
  inner_join(trans1, by=c('household_id', 'product_id'), relationship = 'many-to-many') %>%
  dplyr::filter(redemption_date <= end_date & redemption_date >= start_date) # need to confirm

promotions1 <- promotions %>% dplyr::filter(display_location != 0 | mailer_location != 0)


# a. Customer is likely to buy stuff at store-front event w/o coupons --------
# Total sales from transactions with redemption, grouped by product_id
transWithRedemptions_groupedByProductId <- transWithRedemptions %>%
  group_by(product_id) %>%
  summarise(total_sales = sum(sales_value), .groups = 'drop') %>%
  arrange(desc(total_sales))

product_info_id <- unique(transWithRedemptions_groupedByProductId$product_id)

# Get rid of location 0 for display_location
promotions_displayNot0 <- promotions %>% 
  dplyr::filter(display_location != 0) %>% 
  select(-mailer_location)

transWithRedemptions_groupedByProductId_displayLocation <- promotions_displayNot0 %>% 
  dplyr::filter(product_id %in% product_info_id) %>% 
  left_join(transWithRedemptions_groupedByProductId, by='product_id', relationship = 'many-to-many') %>%
  select(-week) %>%
  left_join(products, by='product_id') %>%
  select(-c('manufacturer_id', 'brand','package_size'))


# Get rid of location 0 for mailer_location
promotions_mailerNot0 <- promotions %>% dplyr::filter(mailer_location != 0)

transWithRedemptions_groupedByProductId_mailerLocation <- promotions_mailerNot0 %>% 
  dplyr::filter(product_id %in% product_info_id) %>% 
  left_join(transWithRedemptions_groupedByProductId, by='product_id', relationship = 'many-to-many') %>%
  select(-week) %>%
  left_join(products, by='product_id') %>%
  select(-c('manufacturer_id', 'brand','package_size'))

# Export as csv file
write_csv(transWithRedemptions_groupedByProductId_mailerLocation, "Transactions_W_Redemptions_Mailer_Location.csv")

write_csv(transWithRedemptions_groupedByProductId_displayLocation, "Transactions_W_Redemptions_Display_Location.csv")



# d. Customer doesn’t use the coupons if products are not shown in mail (mailer_location) ----------------
# Promotions with display_location = 0, which is not displayed
promotions_displayIs0 <- promotions %>% 
  dplyr::filter(display_location == 0) %>% 
  select(-mailer_location)

transWithRedemptions_groupedByProductId_displayLocationIs0 <- promotions_displayIs0 %>% 
  dplyr::filter(product_id %in% product_info_id) %>% 
  left_join(transWithRedemptions_groupedByProductId, by='product_id', relationship = 'many-to-many') %>%
  select(-week) %>%
  left_join(products, by='product_id') %>%
  select(-c('manufacturer_id', 'brand','package_size'))


# Promotions with mailer_location = 0, which is not displayed
promotions_mailerIs0 <- promotions %>% 
  dplyr::filter(mailer_location == 0) %>% 
  select(-display_location)

transWithRedemptions_groupedByProductId_mailerLocationIs0 <- promotions_mailerIs0 %>% 
  dplyr::filter(product_id %in% product_info_id) %>% 
  left_join(transWithRedemptions_groupedByProductId, by='product_id', relationship = 'many-to-many') %>%
  select(-week) %>%
  left_join(products, by='product_id') %>%
  select(-c('manufacturer_id', 'brand','package_size'))

# Export as csv files
write_csv(transWithRedemptions_groupedByProductId_displayLocationIs0, 'Transactions_W_Redemptions_Not_Displayed.csv')
write_csv(transWithRedemptions_groupedByProductId_mailerLocationIs0, 'Transactions_W_Redemptions_Not_Mailed.csv')








