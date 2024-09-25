# Setup -------------------------------------------------------------------
library(tidyverse) 
library(completejourney)
transactions <- transactions_sample
products

# Part 1 --------------------------------------------------------------
## Exercise 1 --------------------------------------------------------------
transactions <- transactions %>% mutate(
  regular_price = (sales_value + retail_disc + coupon_match_disc) / quantity,
  loyalty_price = (sales_value + coupon_match_disc) / quantity,
  coupon_price = (sales_value - coupon_disc) / quantity) %>%
  select(regular_price, loyalty_price, coupon_price, product_id, everything())
# Q1. Identify the five households with the largest `loyalty_price` transactions. What 
# is unique about the transaction with the largest `loyalty_price` value? 
transactions %>%
  slice_max(order_by = loyalty_price, n = 5)
# Q2. Now filter for only those observations where quantity was greater than 0. Now which 
# household(s) have the largest `loyalty_price` transaction?
transactions %>%
  dplyr::filter(quantity > 0) %>% slice_max(order_by = loyalty_price, n = 5)
# Q3. Using the first transaction in the result from #2, filter the `products` data based
#     on the `product_id` to find out which product the largest `loyalty_price` transaction
# is associated with.
products %>%
  dplyr::filter(product_id == 12484608)

## Exercise 2 --------------------------------------------------------------

# how many products had a regular price of $1 or less
transactions %>% dplyr::filter(regular_price <= 1) %>% select(product_id) %>% n_distinct()

# how many products had a loyalty price of $1 or less
transactions %>% dplyr::filter(loyalty_price <= 1) %>% select(product_id) %>% n_distinct()

# how many products had a coupon price of $1 or less
transactions %>% dplyr::filter(coupon_price <= 1) %>% select(product_id) %>% n_distinct()

## Exercise 3 --------------------------------------------------------------
nrow_10 <- transactions %>% group_by(basket_id) %>% summarize(total_sales=sum(sales_value)) %>% dplyr::filter(total_sales > 10) %>% nrow()
nrow_20 <- transactions %>% group_by(basket_id) %>% summarize(total_sales=sum(sales_value)) %>% dplyr::filter(total_sales > 20) %>% nrow()
nrow_basket <- transactions %>% distinct(basket_id) %>% nrow()

prop_10 <- nrow_10 / nrow_basket
prop_20 <- nrow_20 / nrow_basket

## Exercise 4 --------------------------------------------------------------
# Which stores had the largest total `sales_value`
transactions %>%
  group_by(store_id) %>% summarize(total_sales_value = sum(sales_value)) %>% arrange(desc(total_sales_value))

# Which stores had the largest average loyalty discount
transactions %>%
  mutate(pct_loyalty_disc = 1 - (loyalty_price / regular_price)) %>%
  group_by(store_id) %>%
  summarize(avg_pct_loyalty_disc = mean(pct_loyalty_disc, na.rm = TRUE)) %>% arrange(desc(avg_pct_loyalty_disc))


# Part 2 --------------------------------------------------------------
## Exercise 1 --------------------------------------------------------------
library(readxl)
library(here)
excel_sheets(path = here('Module 3','mbta.xlsx'))
mbta <- read_excel(path = here('Module 3','mbta.xlsx'), skip = 1, na = 'NA')

## Exercise 2 --------------------------------------------------------------
#1. View the structure of mbta.
str(mbta)
View(mbta)
#2. View the first 6 rows of mbta.
head(mbta, 6)
view(head(mbta, 6))
#3. View a summary of mbta.
summary(mbta)
#4. How many missing values are in each column? If you see zero that means you didn’t complete exercise 1.3 correctly.
colSums(is.na(mbta))
sum(is.na(mbta))

## Exercise 3 --------------------------------------------------------------
mbta <- mbta %>%
  slice(-c(1,7,11)) %>% # Remove the first, seventh, and eleventh rows of mbta. 
  select(-1) # Remove the first column.
dim(mbta) # Now what is the dimensions of this new data frame?

## Exercise 4 --------------------------------------------------------------
mbta <- mbta %>% pivot_longer(cols = -c('mode'), names_to = c('date'), values_to = 'thou_riders')
dim(mbta)

## Exercise 5 --------------------------------------------------------------
mbta <- mbta %>%
  separate(col='date', into=c('year','month'), sep='-')
head(mbta)

## Exercise 6 --------------------------------------------------------------
mbta$thou_riders[(mbta$thou_riders==40) & (mbta$mode=='Boat')] <- 4

## Exercise 7 --------------------------------------------------------------
#1. Compute the average ridership per mode.
mbta %>% group_by(mode) %>% summarize(avg_ridership = mean(thou_riders))
#2. Compute the average ridership per mode for the month of January.
mbta %>%
  dplyr::filter(month == '01') %>% group_by(mode) %>% summarize(avg_ridership = mean(thou_riders))
#3. Which year had the largest total ridership for the boat mode?
mbta %>%
  dplyr::filter(mode == 'Boat') %>%
  group_by(year) %>%
  summarize(total_riders = sum(thou_riders))
#4. On average, which month experiences the greatest number of passengers on the Heavy Rail mode?
mbta %>% dplyr::filter(mode == "Heavy Rail") %>% group_by(month) %>% summarise(avg_passengers = mean(thou_riders))








