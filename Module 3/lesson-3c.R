
## ----Package requirement--------------------------------------------------
library(tidyr)
library(here)
path <- here('Module 3')

## ----Example Data --------------------------------------------------------
table1
table2
table3
table4a
table4b
table5

## ----Your Turn Data ------------------------------------------------------
data_path <- here(path, 'lesson-3c-data')
bomber_wide <- read_rds(here(data_path, "bomber_wide.rds"))
bomber_long <- read_rds(here(data_path, "bomber_long.rds"))
bomber_prefix <- read_rds(here(data_path, "bomber_prefix.rds"))
bomber_mess <- read_rds(here(data_path, "bomber_mess.rds"))


## ----Gather: wide to long ------------------------------------------------

# make data longer
table4a %>% pivot_longer(c(`1999`, `2000`), names_to="year", values_to = "population")

# These all produce the same results:
table4a %>% pivot_longer(c(`1999`, `2000`), names_to="year", values_to = "population")
table4a %>% pivot_longer(`1999`:`2000`, names_to="year", values_to = "population")
table4a %>% pivot_longer(2:3, names_to="year", values_to = "population")
table4a %>% pivot_longer(-country, names_to="year", values_to = "population")


## ----Spread: long to wide ------------------------------------------------

# make data wider
table2 %>% pivot_wider(names_from = type, values_from = count)


## ----Gather & Spread Your Turn -------------------------------------------

# 1. Reshape the bomber_wide data from wide to long and name the new value
#    column "Flying_Hrs"
bomber_wide %>% pivot_longer(cols = -c('Type','MD'), names_to='Year', values_to='Flying_HRS')
bomber_wide %>% pivot_longer(cols = '1996':'2014', names_to='Year', values_to='Flying_HRS')

# 2. Reshape the bomber_long data from long to wide using the "Output" variable
#    for the new column names and the "Value" variable to fill in values
bomber_long %>% pivot_wider(names_from = 'Output', values_from = 'Value')


## ----Separate: one to multiple-------------------------------------------

table3 %>% separate(col = rate, into = c("cases", "population"), sep = "/")


## ----Unite: multiple to one ---------------------------------------------

table5 %>% unite(col = year, c("century", "year"), sep = "")
table5 %>% unite(col = year, c("century", "year"))


## ----Separate & Unite Your Turn -----------------------------------------

# Reshape the bomber_prefix data so that the "prefix" and "number" columns
# are combined into a “MD” variable with “-“ separator
bomber_prefix
bomber_prefix %>% unite(col=MD, c('prefix','number'), sep='-')


## ----The Big Mess Challenge --------------------------------------------

# Reshape the bomber_mess data to look like whats on the screen
## # A tibble: 57 x 6
##    Type   MD    FY         Cost    FH   Gallons
##    <chr>  <chr> <chr>     <int> <int>     <int>
##  1 Bomber B-1   1996   72753781 26914  88594449
##  2 Bomber B-1   1997   71297263 25219  85484074
##  3 Bomber B-1   1998   84026805 24205  85259038
##  4 Bomber B-1   1999   71848336 23306  79323816

bomber_mess <- bomber_mess %>% unite(col=MD, c('prefix','number'), sep='-') %>%
  separate(col=Metric, into = c('Year', 'Categories'), sep='_') %>%
  pivot_wider(names_from = 'Categories', values_from = 'Value')
