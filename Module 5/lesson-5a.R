
## ---Package Requirements ------------------------------------------------
library(ggplot2) # or library(tidyverse)
library(dplyr)   # for other data wrangling tasks

## ---- Example Data ------------------------------------------------------
# built-in data set
mpg

## ---- Exercise Data -----------------------------------------------------
library(completejourney)
transactions <- transactions_sample
products


## ---- Canvas layer-------------------------------------------------------
ggplot(data = mpg)


## ---- Mapping variables to axes -----------------------------------------
ggplot(data = mpg, aes(x = displ, y = hwy))


## ---- Adding geometrics shapes to represent our data --------------------
ggplot(data = mpg, aes(x = hwy)) +
  geom_histogram()

ggplot(data = mpg, aes(x = hwy)) +
  geom_freqpoly()

ggplot(data = mpg, aes(x = hwy)) +
  geom_density()

ggplot(data = mpg, aes(x = displ, y = hwy)) +
  geom_point()

ggplot(data = mpg, aes(x = class, y = hwy)) +
  geom_boxplot()

ggplot(data = mpg, aes(x = class, y = hwy)) +
  geom_violin()



## ---- Your Turn 1 ---------------------------------------------------------
# Using the transactions & products data:
   # 1: Create a chart that illustrates the distribution of the sales_value variable.
   # 2: Create a chart that shows the counts for each brand
   # 3: Create a scatter plot of quantity vs sales_value
ggplot(data=transactions, aes(x=sales_value)) +
  geom_histogram(bins=20)
ggplot(data=products, aes(x=brand)) +
  geom_bar()
ggplot(data=transactions, aes(x=quantity, y=sales_value)) +
  geom_point()
## ---- Non-mapping Aesthetics----------------------------------------------

# adding color, changing size, different shapes, 50% transparent
ggplot(data = mpg, aes(x = displ, y = hwy)) +
  geom_point(color = "blue", size = 2, shape = 17, alpha = .5)


# where we place the arguments will make it non-mapping or
ggplot(data = mpg, aes(x = displ, y = hwy, color=class)) +
  geom_jitter(size = 2, shape = 17, alpha = .5) # move overlapping points to show better

# mapping# mappingclass
ggplot(data = mpg, aes(x = displ, y = hwy, color = class)) +
  geom_point()



## ---- Your Turn 2 ---------------------------------------------------------

# 1: Create a scatter plot of quantity vs sales_value and color all points blue.
# 2: Create a scatter plot of quantity vs sales_value and color all points based on
#    brand (hint: you'll need to join transactions & products data).
transactions %>% inner_join(products) %>%
  ggplot(aes(x=quantity, y=sales_value, color=brand)) +
  geom_point()



## ---- Creating Small Multiiples with Facetting ----------------------------

# facet_wrap
ggplot(data = mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  facet_wrap(~ class, nrow = 2)

# facet_grid
ggplot(data = mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  facet_grid(drv ~ cyl)


## ---- Your Turn 3 ---------------------------------------------------------
# Is there a difference in the relationship between quantity and sales_value based on
# the day of week? To answer this:
#   (1) extract the day of the week from transaction_timestamp,
#   (2) create a scatter plot of quantity vs sales_value, and
#   (3) facet by the day of the week.
transactions %>% mutate(dayOfWeek = wday(transaction_timestamp, label=TRUE)) %>%
  ggplot(aes(x=quantity, y=sales_value)) +
  geom_point() +
  facet_wrap(~dayOfWeek)


## ---- Titles --------------------------------------------------------------
ggplot(data = mpg, aes(x = displ, y = hwy)) +
  geom_jitter() +
  ggtitle("Displacement vs Highway MPG",
          subtitle = "Data from 1999 & 2008")

ggplot(data = mpg, aes(x = displ, y = hwy)) +
  geom_jitter() +
  labs(
    title = "Displacement vs Highway MPG",
    subtitle = "Data from 1999 & 2008",
    caption = "http://fueleconomy.gov"
    )

## ---- Adjusting Axes-------------------------------------------------------
ggplot(data = txhousing, aes(x = volume, y = median)) +
  geom_point(alpha = .25) +
  scale_x_log10()

ggplot(data = txhousing, aes(x = volume, y = median)) +
  geom_point(alpha = .25)  +
  scale_y_continuous(name = "Median Sales Price", labels = scales::dollar) + #<<
  scale_x_log10(name = "Total Sales Volume", labels = scales::comma) #<<

## ---- Titles & Axes-------------------------------------------------------
ggplot(data = txhousing, aes(x = volume, y = median)) +
  geom_point(alpha = .15) +
  scale_y_continuous(name = "Median Sales Price", labels = scales::dollar) +
  scale_x_log10(name = "Total Sales Volume", labels = scales::comma) +
  labs(
    title = "Texas Housing Sales",
    subtitle = "Sales data from 2000-2010 provided by the TAMU real estate center",
    caption = " http://recenter.tamu.edu/"
    )


## ---- Your Turn 4---------------------------------------------------------
# Complete this code to plot the relationship between total basket sales and quantity.
# See if you can adjust the x and y axis titles and also add a main title.
transactions %>%
   group_by(basket_id) %>%
   summarize(
      total_sales = sum(sales_value, na.rm = TRUE),
      total_qty = sum(quantity, na.rm = TRUE)
   ) %>%
   ggplot(aes(x = total_qty, y = total_sales)) +
   geom_point(alpha=0.1) +
   scale_x_log10(name="Total quantity", labels=scales::dollar) +
   scale_y_log10(name="Total sales", label=scales::dollar) +
   ggtitle("______")



## ---- Overplotting -------------------------------------------------------
ggplot(data = txhousing, aes(x = volume, y = median)) +
  geom_point(alpha = .1)  +
  scale_x_log10() +
  geom_smooth()

ggplot(data = txhousing, aes(x = volume, y = median)) +
  geom_point(alpha = .1)  +
  scale_x_log10() +
  geom_smooth(method = "lm") #line

ggplot(data = txhousing, aes(x = volume, y = median)) +
  geom_point(alpha = .1)  +
  scale_x_log10() +
  geom_smooth(method = "lm") +
  facet_wrap(~ month)


## ---- Global attributes--------------------------------------------------
ggplot(data = mpg, aes(x = displ, y = hwy, color = drv)) + #<<
  geom_point() +
  geom_smooth()

## ---- Local attributes---------------------------------------------------
ggplot(data = mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  geom_smooth(mapping = aes(color = drv)) #<<

