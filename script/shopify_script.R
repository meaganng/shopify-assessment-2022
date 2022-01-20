# Import packages
library(here)
library(tidyverse)

# Read in csv
shopify_data <- read_csv(here("2019 Winter Data Science Intern Challenge Data Set - Sheet1.csv")) 

shopify_data$order_amount # review the order_amount column

checkNA <- is.na(shopify_data) # check for any NA values in the data set

shopifyaveragevalue <- mean(shopify_data$order_amount) # This is the original way Shopify used to calculate the average order value

sort(shopify_data$order_amount, decreasing = T) # Dataset has large values upon reviewing the entire order_amount column

##### 1a ######

# Clean up
sneakers <- shopify_data %>%
  group_by(shop_id) %>%
  mutate(shop_count = n()) %>% #counts the number of times a user_id has bought items at that shop_id
  summarise(AverageStoreValue = mean(rep(order_amount)))%>% # Find the mean for each store
  group_by(AverageStoreValue) %>% 
  arrange(desc(AverageStoreValue)) # Sort by decreasing 


# Plot
ggplot(data = sneakers, mapping = aes(x = shop_id, AverageStoreValue, size = AverageStoreValue)) + 
  geom_point() + 
  labs(title = "Average order value of sneakers by shop", 
       x = "Shop Identification Number", 
       y = "Average order value", 
       size = "Store value by size") +
  guides(colour = FALSE)

# Check and remove outliers
sneaker_outliers <- sneakers %>%
  mutate(AverageStoreValue = sort(AverageStoreValue, decreasing = T)) %>% # You can see there are 2 outliers when we look at the ascending to descending values of the average store 
  filter(!AverageStoreValue > 1000) # Filter stores out that have an AverageStoreValue greater than 1000

# Plot the new data
ggplot(data = sneaker_outliers, mapping = aes(x = shop_id, AverageStoreValue, color = AverageStoreValue, size = AverageStoreValue)) + 
  geom_point() + 
  labs(title = "Average order value of sneakers by shop", 
       x = "Shop Identification Number", 
       y = "Average order value", 
       size = "Average order value by size",
       color = "Order amount")

##### 1b #####

#After reviewing Question 1a., the AOV per store makes it easier to visualize and then by removing the outliers from shop_id 42 and 78, you can see the data for the other shops average order value are in a similar range. Although the way Shopify calculates the AOV 'works', it doesnt take into account each store which may have a different amount of sales or the pricing per sneakers. A better metric to report this dataset is using the median. The median can be more descriptive in a dataset than calculating the average. This is because the median is less affected by outliers in a dataset which can give an approximate mean.

#### 1c ####'

sneaker_median <- median(shopify_data$order_amount)


