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
sample <- transactions %>% left_join(products, by="product_id") %>% group_by(week, department) %>% summarise(total_sales = sum(sales_value), .groups = 'drop')
sample <- sample %>% dplyr::filter(!is.na(department))
sample2 <- sample %>% group_by(week) %>% arrange(desc(total_sales)) %>%
  mutate(rank=row_number()) %>%
  dplyr::filter(rank <= 5) %>%
  ungroup() %>%
  select(-rank)

totalSalesDistribution <- ggplot(sample2, aes(x=week, y=department, fill=total_sales)) + 
  geom_raster() +
  scale_fill_gradientn(colors=c('lightblue','blue'), name = "Total Sales") +
  scale_x_continuous(breaks=seq(0,53, by=2)) +
  labs(title="Total Sales of Top 5 Over Weeks", subtitle ="The total sales of top 5 departments per week are extracted to visualize the distribution", x="Week", y="Department") +
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
  drop_na(display_location)

# Count of coupons issued
sample2 <- sample1 %>% group_by(product_id, display_location, campaign_id) %>% 
  summarise(count_coupon = n(), .groups='drop') %>% 
  arrange(desc(count_coupon))

# Count of redemption
sample3 <- sample2 %>% 
  inner_join(coupon_redemptions,by='campaign_id', relationship = 'many-to-many') %>%
  group_by(product_id, display_location, campaign_id) %>%
  summarise(count_redemptions = n())


# Drop the no-display in display_location and mailer_location
promotions_1 <- promotions %>% dplyr::filter(display_location != 0, mailer_location!=0)
transactions_1 <- transactions %>% 
  group_by(product_id, store_id) %>% 
  summarise(total_sales = sum(sales_value), .groups = 'drop')
sub_data <- promotions_1 %>% 
  inner_join(transactions_1, by=c('product_id', 'store_id')) %>% 
  group_by(display_location, mailer_location) %>% summarise(
  promotion_count = n(),
  sales_impact = sum(total_sales),
  .groups = 'drop'
)

promotion_by_location <- ggplot(sub_data, aes(x = display_location, y = mailer_location, size = promotion_count, color = sales_impact)) +
  geom_point(alpha = 0.6) +
  scale_size(range = c(3, 20), name='Promotion Count') +
  scale_color_gradientn(colors = c('lightblue','purple','darkblue'), name='Sales Impact') +
  scale_x_discrete(labels=c('1'='Store Front', '2'='Store Rear', '3'='Front end cap', '4'='Mid-aisle end cap', '5'='Rear end cap', '6'='Side aisle end cap', '7'='In-aisle', '9'='Secondary location display', 'A'='In-shelf')) +
  scale_y_discrete(labels=c('A'='Interior page feature', 'C'='Interior page line item', 'D'='Front page feature', 'F'='Back page feature', 'H'='Wrap front feature', 'J'='Wrap interior coupon', 'L'='Wrap back feature', 'P'='Interior page coupon', 'X'='Free on interior page', 'Z'='Free on front page, back page, or wrap')) +
  labs(title = "Bubble Map of Promotions by Display Location and Sales Impact",
       x = "Display Locations", y = "Mailer Locations") +
  theme_minimal()
































