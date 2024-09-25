
# Prerequisites -------------------------------------------------------------------

library(tidyverse)
library(lubridate)

# Data
library(completejourney)
transactions <- transactions_sample

library(nycflights13)
flights


# Create date-times ---------------------------------------------------------------

# year, month, day
ymd("2018-12-02")

# year, month, day, hour
ymd_h("2018-12-02 01")

# year, month, day, timestamp
ymd_hms("2018-12-02 01:31:27")

# format can differ
ymd("2018-12-02")
ymd("2018/12/02")
mdy("February 02, 2018")

# make date-time from separate string components
make_date(year = 2022, month = 08, day = 24)

# make date-time from separate variable components
year <- 2022
month <- 08
day <- 24
hour <- 09
min <- 24

make_datetime(year, month, day, hour, min)


# Your Turn! ----------------------------------------------------------------------

# The nycflights13::flights data has a year, month, and day variable. See if you can
# create a new column named date that converts these three columns into a date stamp.



# Accessomg components ------------------------------------------------------------

# get year
year("2018-12-02 01:31:27")

# get quarter
quarter("2018-12-02 01:31:27")

# get month
month("2018-12-02 01:31:27", label = TRUE)

# get weekday
wday("2018-12-02 01:31:27", label = TRUE, abbr = FALSE)

# use within filter
flights %>%
   select(time_hour, everything()) %>%
   filter(wday(time_hour) %in% 6:7)

# use within mutate
flights %>%
   mutate(weekday = wday(time_hour, label = TRUE)) %>%
   select(time_hour, weekday, everything())


# Your Turn! ----------------------------------------------------------------------

# Using the transactions data...

# 1. Compute the average sales (sales_value) by weekday. On average, which weekday
#    experiences the greatest mean sales?

# 2. Compute the total daily sales value (sales_value) for each day of the year. What
#    was the date that experienced the largest total sales. Is this surprising?
