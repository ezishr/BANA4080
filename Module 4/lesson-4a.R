
## ----Package Requirements ------------------------------------------------
library(tidyverse) # or library(dplyr)
library(completejourney)

## ----Example Data set-up -------------------------------------------------
x <- tribble(
  ~key, ~val_x,
     1, "x1",
     2, "x2",
     3, "x3"
)
y <- tribble(
  ~key, ~val_y,
     1, "y1",
     2, "y2",
     4, "y3"
)

z <- tribble(
  ~key, ~val_z,
  3, 'z1',
  4, 'z2',
  5, 'z3'
)

## ----Exercises Data------------------------------------------------------
transactions <- transactions_sample
products
households



## ----Mutating Joins ------------------------------------------------------

# inner join
x %>% inner_join(y, by = "key")

# left join
x %>% left_join(y, by = "key")

# right join
x %>% right_join(y, by = "key")

# full join
x %>% full_join(y, by = "key")


## ----Your Turn 1---------------------------------------------------------

# 1. Join the transactions and products data using `inner_join()`. The join key is the
#    `product_id` variable.

# 2. Join the transactions, products, and demographics data using two `inner_join()`s.
#    The join key between transactions and products is the `product_id` variable and
#    the join key between transactions and demographics is the `household_id` variable.



## ----Filtering Joins ------------------------------------------------------

# semi-join
x %>% semi_join(y, by = "key")

# anti-join
x %>% anti_join(y, by = "key")


## ----Your Turn 2---------------------------------------------------------

# 1. Of the 800+ households in our demographics data, how many do we transaction data
#    for?

# 2. Of the 92,000+ products in our products data, how many are not represented in our
#    transactions data?




## ----Final Challenge -----------------------------------------------------

# Identify the top 10 `product_category` items that have the largest total
# `sales_value` for those customers that are 65 years of age or older.










