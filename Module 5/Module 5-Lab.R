# Setup -------------------------------------------------------------------
library(tidyverse)
library(completejourney)
c(promotions, transactions) %<-% get_data(which = 'both', verbose = FALSE)
view(coupon_redemptions)
view(coupons)
view(transactions)
view(demographics)
view(campaign_descriptions)
view(campaigns)
view(products)
promotions

# Total Sales Distribution --------------------------------------------
top_5_category_sales <- transactions %>% left_join(products, by="product_id") %>% 
  group_by(product_category) %>% 
  summarise(total_sales = sum(sales_value), .groups = 'drop') %>%
  arrange(desc(total_sales)) %>%
  mutate(rank = row_number()) %>%
  dplyr::filter(rank <= 5) %>%
  pull(product_category)

sample2 <- transactions %>% left_join(products, by='product_id') %>%
  group_by(product_category, week) %>%
  summarise(sales_by_category = sum(sales_value), .groups = 'drop') %>%
  dplyr::filter(product_category %in% top_5_category_sales)

totalSalesDistribution <- ggplot(sample2, aes(x=week, y=product_category, fill=sales_by_category)) + 
  geom_raster() +
  scale_fill_gradientn(colors=c('lightblue','blue'), name = "Total Sales") +
  scale_x_continuous(breaks=seq(0,53, by=3)) +
  labs(title="Total Sales of Top 5 Product Categories Over Weeks", subtitle ="The total sales of top 5 categories in the year throughout the weeks are extracted to visualize the distribution", x="Week", y="Product Category") +
  theme_minimal()

# Redemption vs. Household Size --------------------------------------------------------
coupon_redemptions <- coupon_redemptions %>% mutate(dayOfWeek = wday(redemption_date, label=TRUE))
sample <- coupon_redemptions %>% 
  inner_join(demographics, by="household_id") %>% 
  select(household_id, campaign_id, redemption_date, household_size, age, income) %>%
  mutate(dayOfWeek=wday(redemption_date))
sample2 <- sample %>% group_by(dayOfWeek, household_size) %>% count()

custom_colors <- c('1'='blue','2'='red','3'='orange','4'='purple','5+'='darkgreen')

redemption_householdSize <- ggplot(sample2) +
  geom_line(data = subset(sample2, household_size == '2'), aes(x=dayOfWeek, y=n, colour = '2'), linewidth = 1.5, linetype='dotted') +
  geom_line(data = subset(sample2, household_size != '2'), aes(x=dayOfWeek, y=n, color = household_size)) + 
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7), labels = c('Sunday','Monday', 'Tuesday','Wednesday','Thursday','Friday','Saturday')) +
  scale_color_manual(values=custom_colors) +
  labs(title = "Distribution of Redemptions Over Weeks based off Household Size",
       subtitle = 'Household size of 2 has the highest redemptions mostly throughout the week',
       x = 'Day of Week',
       y = 'Count of Redemptions',
       color="Household Size") +
  theme_minimal() 

# Product Placement vs. Direct Marketing -------------------------------------------------
sample1 <- promotions %>% select(product_id, display_location, mailer_location, week) %>%
  right_join(coupons, by='product_id', relationship = 'many-to-many') %>%
  drop_na(display_location, mailer_location)

# Count of coupons issued
sample2 <- sample1 %>% group_by(display_location, mailer_location, campaign_id) %>% 
  summarise(count_coupon = n(), .groups='drop') %>% 
  arrange(desc(count_coupon))

# Count of redemption
sample3 <- sample2 %>% 
  inner_join(coupon_redemptions,by='campaign_id', relationship = 'many-to-many') %>%
  group_by(display_location, mailer_location, campaign_id) %>%
  summarise(count_redemptions = n(), .groups = 'drop')


# Drop the no-display in display_location and mailer_location
promotions_1 <- promotions %>% dplyr::filter(display_location != 0, mailer_location!=0)

transactions_1 <- transactions %>% 
  group_by(product_id, store_id) %>% 
  summarise(total_sales = sum(sales_value), .groups = 'drop')

sub_data <- promotions_1 %>% 
  inner_join(transactions_1, by=c('product_id', 'store_id')) %>% 
  group_by(display_location, mailer_location) %>% summarise(
  avg_sales_per_promo = sum(total_sales)/n(),
  .groups = 'drop'
) %>%
  dplyr::filter(avg_sales_per_promo > 0)

library(scales)

promotion_by_location <- ggplot(sub_data, aes(x = display_location, y = mailer_location, size = avg_sales_per_promo)) +
  geom_point(alpha = 0.6, color='blue') +
  scale_size(range = c(2, 15), name='Average Sales Per Promotion') +
  scale_x_discrete(labels=c(
    '1'='Store Front', 
    '2'='Store Rear', 
    '3'='Front end cap', 
    '4'='Mid-aisle end cap', 
    '5'='Rear end cap', 
    '6'='Side aisle end cap', 
    '7'='In-aisle', 
    '9'='Secondary location display', 
    'A'='In-shelf')) +
  scale_y_discrete(labels=c(
    'A'='Interior page feature', 
    'C'='Interior page line item', 
    'D'='Front page feature', 
    'F'='Back page feature', 
    'H'='Wrap front feature', 
    'J'='Wrap interior coupon', 
    'L'='Wrap back feature', 
    'P'='Interior page coupon', 
    'X'='Free on interior page', 
    'Z'='Free on front page, back page, or wrap')) +
  labs(title = 'Bubble Map of Average Sale per Promotion by Display and Mailer Locations',
       x = "Display Locations", 
       y = "Mailer Locations") +
  theme_minimal()




