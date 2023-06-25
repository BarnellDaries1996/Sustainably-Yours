#Create Dataset for Store

product_list <- c("Organic Apples", "Organic Bananas", "Organic Tomatoes","Cotton T-shirts", "Bamboo Bedsheets", "Organic Milk","Cloth Shopping Bags", "Reusable Water Bottles", "Recycled Paper","Solar lights", "Eco-friendly Toys", "Reusable Straws") 

price_list <- c(3.50, 2.00, 4.50, 11.00, 20.00, 3.50, 8.00, 10.00, 5.00, 15.00, 11.00, 5.00)

# Create Data Frame

sustainable_products <- data.frame(product_list, price_list)

# Create Vector for Store Categories

category <- c("Fruit", "Fruit", "Fruit", "Clothing", "Bedding", "Dairy","Bags","Drinkware","Paper","Lighting","Toys","Straws")

# Add Category to Data Frame

sustainable_products$Category <- category

# Install Packages

install.packages(ggplot2)
install.packages(dplyr)

library(ggplot2)
library(dplyr)

# Plot Store's Products

ggplot(sustainable_products, aes(x = product_list, y = price_list)) + geom_bar(stat = "identity", fill = "blue") + labs(x="Product", y="Price") + ggtitle("Prices of Products at the Sustainable Store")

# Filter Data Frame for Products Under $10

under_ten <- filter(sustainable_products, price_list <= 10)

# Sum of Product Prices

sum_product_prices <- sum(price_list)

# Calculate Average Price

avg_price <- mean(price_list)

# Subset Fruit Data

fruit <- subset(sustainable_products, Category == "Fruit")

# Group Data by Category

category_price <- group_by(sustainable_products, Category)

# Calculate Mean Price by Group

mean_price_by_category <- summarise(category_price, mean_price = mean(price_list))

# Create Vector for Discounted Prices

discount_price_list <- price_list*0.8

# Create New Data Frame with Discounted Price List

discounted_products <- data.frame(product_list, discount_price_list)

# Merge Data Frames

merged_products <- merge(sustainable_products, discounted_products, by = "product_list")

# Rename Price Columns

renamed_columns <- rename(merged_products, price_list.x = "Original Price", price_list.y = "Discounted Price")

# Create Discount Flag

discount_flag <- ifelse(renamed_columns$price_list.x > renamed_columns$price_list.y, "Yes", "No")

# Add Discount Flag to Data Frame

renamed_columns$Discounted <- discount_flag

# Order By Price

order_by_price <- arrange(renamed_columns, price_list.y)

# Find Most Popular Product

most_popular <- arrange(renamed_columns, desc(price_list.x))

# Create Histogram

ggplot(renamed_columns) + geom_histogram(aes(x=price_list.x, fill=Category)) + labs(x="Price", y="Count") + ggtitle("Distribution of Prices at the Sustainable Store")

# Move Data to CSV File

write.csv(renamed_columns, "sustainable_products.csv", row.names = FALSE)