
# Requirements --------------------------------------------------------------------

library(tidyverse)
library(completejourney)

transactions <- transactions_sample
products


# stringr function examples -------------------------------------------------------

# character string vector
x <- c("FROZEN MEAT", "FRZN MEAT/MEAT DINNERS", "MEAT - MISC", "CEREAL")

# force to lower case
str_to_lower(x)

# extract first 4 characters
str_sub(x, start = 1, end = 4)

# detect if "meat" is in each element
str_detect(x, pattern = "MEAT")

# replace first "MEAT" in each element with "NON-VEGGIE
str_replace(x, pattern = "MEAT", replacement = "NON-VEGGIE")

# replace all "MEAT" in each element with "NON-VEGGIE
str_replace_all(x, pattern = "MEAT", replacement = "NON-VEGGIE")


# Detecting matches ---------------------------------------------------------------

products %>%
   select(product_id, product_category) %>%
   dplyr::filter(str_detect(product_category, "MEAT"))

products %>%
   distinct(product_id, product_category) %>%
   mutate(meat_product = str_detect(product_category, "MEAT")) %>% #<<
   summarize(
      count = sum(meat_product, na.rm = TRUE),
      prop  = mean(meat_product, na.rm = TRUE)
   )


# Extracting,  replacing,  and then detecting -------------------------------------

products %>%
   mutate(product_category = str_replace_all(product_category, pattern = "FRZN", replacement = "FROZEN")) %>% #<<
   dplyr::filter(str_detect(product_category, "FROZEN")) %>%
   distinct(product_category)


# Regular expressions -------------------------------------------------------------

products %>%
  dplyr::filter(str_detect(product_category, regex("(FROZEN|FRZN)", ignore_case = TRUE)))


# Your Turn! ----------------------------------------------------------------------

# How many product categories contain "fall" or "spring" in their description?
products %>%
  dplyr::filter(str_detect(product_category, regex("FALL|SPRING", ignore_case=TRUE)))

# Line anchors --------------------------------------------------------------------

# begins with fruit
rgx <- regex("^fruit", ignore_case = TRUE)

products %>%
   dplyr::filter(str_detect(product_category, rgx)) %>%
   select(product_category)

# ends with fruit
rgx <- regex("fruit$", ignore_case = TRUE)

products %>%
   dplyr::filter(str_detect(product_category, rgx)) %>%
   select(product_category)

# begins or ends with fruit
rgx <- regex("(^fruit|fruit$)", ignore_case = TRUE)

products %>%
   dplyr::filter(str_detect(product_category, rgx)) %>%
   select(product_category)


# Special character classes -------------------------------------------------------

# Matching certain numeric or letter values
products %>%
   select(product_type, package_size) %>%
   dplyr::filter(str_detect(package_size, regex("[2-5]")))

# Matching any digit
products %>%
   select(product_type, product_type) %>%
   dplyr::filter(str_detect(product_type, regex("\\d")))

# Matching any punctuation
products %>%
   select(product_type, product_type) %>%
   dplyr::filter(str_detect(product_type, regex("[[:punct:]]")))


# Repetition ----------------------------------------------------------------------

# match a preceding pattern one or more times
products %>%
  dplyr::filter(str_detect(product_id, regex("8+")))

# match a preceding pattern exactly n times
products %>%
  dplyr::filter(str_detect(product_id, regex("8{2}")))

# match a preceding pattern at least n times but not more than m times
products %>%
  dplyr::filter(str_detect(product_id, regex("(87){2,4}")))


# Escaping character searches -----------------------------------------------------

# searching for $
products %>%
   select(product_type, product_type) %>%
  dplyr::filter(str_detect(product_type, regex("\\$")))

# searching for (
products %>%
   select(product_type, product_type) %>%
  dplyr::filter(str_detect(product_type, regex("\\(")))


# Your Turn! ----------------------------------------------------------------------
products %>%
  dplyr::filter(str_detect(product_category, pattern="PIZZA"), 
                str_detect(product_type, regex("(?i)snacks|appetizer"))) %>%
  inner_join(transactions) %>%
  group_by(product_id) %>%
  summarise(total_qty = sum(quantity, na.rm = TRUE)) %>%
  arrange(desc(total_qty))
  
  
# Identify all products that are categorized (`product_category`) as pizza but are
# considered a snack or appetizer (`product_type`). Which of these products
# (`product_id`) have the most sales (measured by quantity)? Hint: you'll need to join
# to the transactions data.
