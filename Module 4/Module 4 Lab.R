library(tidyverse)
library(lubridate) 
library(completejourney)
transactions <- get_transactions()
dim(transactions)
promotions <- get_promotions() 
dim(promotions)

# Question 1 --------------------------------------------------------------
# how many transaction do we have demographics on?
transactions %>%
  inner_join(demographics, by = "household_id") %>% tally()
# how many transaction do we NOT have demographics on?
transactions %>%
  anti_join(demographics, by = "household_id") %>% tally()

# Question 2 --------------------------------------------------------------
transactions %>%
  inner_join(demographics, by = "household_id") %>% group_by(age) %>%
  summarise(total_sales = sum(sales_value)) %>% arrange(desc(total_sales))

# Question 3 --------------------------------------------------------------
# Identify households with $1000 or more in total sales
hshld_1000 <- transactions %>%
  group_by(household_id) %>%
  summarise(total_sales = sum(sales_value, na.rm = TRUE)) %>% dplyr::filter(total_sales >= 1000)

# How many of these households do we have demographic data on?
hshld_1000 %>%
  inner_join(demographics, by = "household_id") %>% tally()
# How many do we not have demographic on?
hshld_1000 %>%
  anti_join(demographics, by = "household_id") %>% tally()
# Which income range produces the most households that spend \$1000 or more?
hshld_1000 %>%
  inner_join(demographics, by = 'household_id') %>% group_by(income) %>% tally() %>% arrange(desc(n))

# Question 4 --------------------------------------------------------------
# join transactions and filtered promotions data
front_display_trans <- promotions %>%
  dplyr::filter(display_location==1) %>%
  inner_join(transactions, by = c('product_id', 'store_id', 'week'))
# total sales for all products displayed in the front of the store
front_display_trans %>% summarize(total_sales = sum(sales_value))

# Identify the product displayed in the front of the store that had the largest total sales
front_display_trans %>%
  group_by(product_id) %>% summarize(total_front_display_sales = sum(sales_value)) %>% arrange(desc(total_front_display_sales))

# Question 5 --------------------------------------------------------------
coupons %>%
  dplyr::filter(campaign_id == 18, coupon_upc == 10000089238) %>% inner_join(products, by = "product_id")

# Question 6 --------------------------------------------------------------
#Identify all different products that contain “pizza” in their product_type description. 
#Which of these products produces the greatest amount of total sales (compute total sales by product ID and product type)?
products %>% dplyr::filter(str_detect(product_type, pattern="PIZZA")) %>%
  inner_join(transactions, by='product_id') %>%
  group_by(product_id, product_type) %>%
  summarise(total_sales = sum(sales_value, na.rm=TRUE)) %>%
  arrange(desc(total_sales))

# Question 7 --------------------------------------------------------------
relevant_products <- products %>% dplyr::filter(
  str_detect(product_category, regex('pizza', ignore_case = TRUE)),
  str_detect(product_type, regex('snack|appetizer', ignore_case = TRUE)) )

relevant_products %>%
  inner_join(transactions, by = 'product_id') %>% group_by(product_id) %>%
  summarise(total_qty = sum(quantity)) %>% arrange(desc(total_qty))

# Question 8 --------------------------------------------------------------
pb <- products %>%
  dplyr::filter(str_detect(product_type, regex("peanut butter", ignore_case = TRUE)))
tally(pb)


pb %>%
  inner_join(transactions, by = "product_id") %>% 
  group_by(month = month(transaction_timestamp, label = TRUE)) %>% 
  summarize(total_sales = round(sum(sales_value),2)) %>% 
  arrange(desc(total_sales))

# Question 9 --------------------------------------------------------------
coupon_redemptions %>% dplyr::filter(campaign_id==18, coupon_upc==10000085475) %>%
  inner_join(transactions, by="household_id") %>%
  dplyr::filter(yday(redemption_date) == yday(transaction_timestamp)) %>%
  summarise(total_sales = sum(sales_value))

coupon_redemptions %>% dplyr::filter(campaign_id==18, coupon_upc==10000085475) %>%
  inner_join(transactions, by="household_id") %>% summarise(count_household = n_distinct(household_id))

# Question 10 -------------------------------------------------------------
coupon_redemptions %>% dplyr::filter(campaign_id == 18, coupon_upc == 10000085475) %>%
  inner_join(coupons, by="coupon_upc", relationship = "many-to-many") %>%
  inner_join(products, by="product_id") %>%
  dplyr::filter(str_detect(product_category, pattern="VEGETABLES")) %>%
  inner_join(transactions, by=c("household_id", "product_id")) %>%
  dplyr::filter(yday(transaction_timestamp)==yday(redemption_date)) %>%
  group_by(product_type) %>%
  summarise(total_sales = sum(sales_value)) %>%
  arrange(desc(total_sales))
