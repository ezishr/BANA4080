
## ----load-pkgs-----------------------------------------------------------
library(tidyverse)
# or more specifically...
# library(forcats)
# library(ggplot2)
library(here)

## ----load-data-----------------------------------------------------------
data_dir <- here("Module 5/lesson-5b-data")
products <- read_csv(here(data_dir, 'products.csv'))
households <- read_csv(here(data_dir, 'households.csv'))

## ----Factor Data Types -------------------------------------------------

# example
eyes <- factor(x = c("blue", "green", "green"), levels = c("blue", "brown", "green"))
eyes

unclass(eyes) #blue is 1, green is 3 in the levels

# distinct marital status factor levels
households %>% distinct(marital)

# distinct income levels factor levels
households %>% distinct(income_range)

# plotting factors
ggplot(households, aes(marital)) +
  geom_bar()

ggplot(households, aes(income_range)) +
  geom_bar()

# relevel income range factor levels -------------------------------------------
households <- households %>%
  mutate(income_range = fct_relevel(income_range, "UNDER 35K", "35-49K", "50-74K", "75-99K", "100-150K", "150K+", "null")) #<<

households %>% count(income_range)

ggplot(households, aes(income_range)) +
  geom_bar()

# recode-income range factor levels ----------------------------------------
households <- households %>%
  mutate(income_range = fct_recode(income_range, Unknown = "null")) #<<

households %>% count(income_range)

ggplot(households, aes(income_range)) +
  geom_bar()

# collapse marital status factor levels ------------------------------------
households <- households %>%
  mutate(
    marital = fct_collapse(marital, Unknown = c("null", "Unknown")), #<<
    marital = fct_relevel(marital, "Unknown", after = Inf)
    )

households %>% count(marital)

ggplot(households, aes(marital)) +
  geom_bar()


# change factor orders just for plotting purposes ---------------------------
households %>%
  mutate(homeowner = fct_collapse(homeowner, Unknown = c("Unknown", "null"))) %>%
  ggplot(aes(fct_infreq(homeowner))) + # to plot the frequency of levels and order in decreasing
  geom_bar()

households %>%
  mutate(homeowner = fct_collapse(homeowner, Unknown = c("Unknown", "null"))) %>%
  ggplot(aes(fct_rev(homeowner))) + # to plot the frequency of levels and order in increasing
  geom_bar()


products %>% count(commodity) %>%
  ggplot(aes(n, fct_reorder(commodity, n))) + # reorder the commodity based of the n (count)
  geom_point()

prod_count <- products %>%
  count(department) %>%
  drop_na() %>%
  ggplot(aes(n, fct_reorder(department, n))) + #<<
  geom_point()



## ---- Your Turn Factors ---------------------------------------------------
# Using the households data
#   1. Recode the hh_size factor so that "null" is now "Unknown"
#   2. Relevel the hh_size factor so that "Unknown" is at the end
#   2. Use a bar chart to illustrate the distribution of hh_size in our data
households %>% mutate(hh_size = fct_recode(hh_size, "Unknown"="null"),
                      hh_size = fct_relevel(hh_size, 'Unknown', after=Inf))%>%
  ggplot(aes(hh_size)) + geom_bar()







