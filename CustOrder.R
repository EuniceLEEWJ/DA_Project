getwd()
setwd("C:/Users/User/OneDrive - Monash University/Desktop/ETW2001/Assignment/A2")

########################ETW2001 ASSIGNMENT 1####################################
####################Eunice Lee Wen Jing 33250979################################

#Clean the environment
rm(list = ls())

################################################################################
##############################SECTION A#########################################

#Load libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(nycflights13)

##Q1
#Perform inner join order & order items
order = read.csv("olist_orders_dataset.csv")
order_items = read.csv("olist_order_items_dataset.csv")

innerJoin = inner_join(order,order_items,by = "order_id")
head(innerJoin)

glimpse(order)
glimpse(order_items)
glimpse(innerJoin)


##Q2
#Perform left join orders & order reviews
review = read.csv("olist_order_reviews_dataset.csv")

leftJoin = left_join(order, review, by = "order_id")
head(leftJoin)

glimpse(order)
glimpse(review)
glimpse(leftJoin)

# Order with reviews
With_reviews <- leftJoin %>% filter(!is.na(review_id)) %>% nrow()
With_reviews # 99224

# Check how many orders do not have corresponding reviews
without_reviews <- leftJoin %>%
  filter(is.na(review_id)) %>%  # Filter rows where review_id is NA
  nrow() #Show no of rows without reviews

# Display the result
cat("Number of orders without corresponding reviews:", without_reviews, "\n") #768

##Q3
#Perform right join items & products
product = read.csv("olist_products_dataset.csv") #32951obv ,9var

rightJoin = right_join(order_items,product,by="product_id")
glimpse(rightJoin) #112650obv,15 var

# Identify products that have not been sold yet
unsold <- rightJoin %>%
  filter(is.na(order_id))%>%  # Filter rows where order_id is NA
  nrow()

glimpse(unsold) #All product is sold

cat("The number of unsold product in product dataset:",unsold,"\n")

##Q4
#Perform full outer join customers & order
customer = read.csv("olist_customers_dataset.csv")

fullJoin = full_join(customer, order, by = "customer_id")
glimpse(fullJoin)

# Any customers without orders or orders without customer details
without_orders <- fullJoin %>%
  filter(is.na(order_id)) %>%  # Filter rows where order_id is NA
  nrow()

without_customers <- fullJoin %>%
  filter(is.na(customer_id))%>%  # Filter rows where customer_id is NA
  nrow()

cat("Number of customers without orders:", without_orders, "\n") #0
cat("Number of orders without customer details:", without_customers, "\n") #0

##Q5
#Perform semiJoin sellers & items
seller = read.csv("olist_sellers_dataset.csv")

#details of customers who have placed orders
semiJoin = semi_join(seller, order_items, by = "seller_id")
glimpse(semiJoin)

# Characteristics of active sellers VS the complete seller list
cat("Number of active sellers:", nrow(semiJoin), "\n") #3095
cat("Total number of sellers in the complete list:", nrow(seller), "\n") #3095

##Q6
#Perform antijoin customer & order
customer = read.csv("olist_customers_dataset.csv")

antiJoin = anti_join(customer,order, by = "customer_id")
glimpse(antiJoin) #0 rows

##Q7
#Use Combination of join types to merge order, item, product, seller
# Merge the datasets
merged_data <- order %>%
  inner_join(order_items, by = "order_id") %>%
  inner_join(product, by = "product_id") %>%
  inner_join(seller, by = "seller_id")
glimpse(merged_data)


# Sellers to product
# Number of products sold by each seller
products_per_seller <- merged_data %>%
  group_by(seller_id) %>%
  mutate(total_products_sold = n_distinct(product_id))%>%
  select(seller_id,total_products_sold)
  
glimpse(products_per_seller)  
products_per_seller
summary(products_per_seller)

highest_products_sold <- products_per_seller %>%
  filter(total_products_sold == max(products_per_seller$total_products_sold))
highest_products_sold

lowest_products_sold <- products_per_seller %>%
  filter(total_products_sold == min(products_per_seller$total_products_sold))
lowest_products_sold
# Visualize the distribution of order values
# Histogram
ggplot(merged_data, aes(x = price)) +
  geom_histogram(binwidth = 100, fill = "pink", color = "black") +
  labs(title = "Distribution of Order Values", x = "Order Value", y = "Frequency")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))  # Center title
  


################################################################################
##############################SECTION B#########################################
#Load Library
library(ggplot2)
library(dplyr)
#Load Dataset
sales_data = read.csv("sales_data_sample.csv")

##Q1
#Generate Fig3 Pie Chart
colnames(sales_data)
str(sales_data)

table_product <- as.data.frame(table(sales_data$PRODUCTLINE))
colnames(table_product) <- c("ProductLine", "Count")
table_product

# Plotting the pie chart
ggplot(table_product, aes(x = "", y = Count, fill = ProductLine)) +
  geom_bar(stat = "identity") +
  # Converts the Cartesian coordinates to polar coordinates, turn bar plot into a pie chart. 
  # start = 0 specifies that the first slice should start at the top.
  coord_polar("y", start = 0)+
  
  labs(title = "Sales Distribution by PRODUCTLINE (Pie Chart)") +
  scale_fill_brewer(palette = "Set1") + 
  #Calculate the percentage, round percentage of the count to 1 digit
  geom_text(aes(label = paste0(ProductLine, "\n", round(Count / sum(Count) * 100,1), "%")),
            position = position_stack(vjust = 0.5)) +
  theme_void()+
  #Remove legend, labels in the pie chart already
  theme(legend.position = "none",plot.title = element_text(hjust = 0.5))

##Q2
#Generate Fig10 Area Chart
ggplot(sales_data, aes(x = 1:nrow(sales_data))) +
  geom_area(aes(y = SALES, fill = "SALES"), alpha = 0.5) +
  geom_area(aes(y = PRICEEACH, fill = "PRICEEACH"), alpha = 0.5) +
  geom_area(aes(y = QUANTITYORDERED, fill = "QUANTITYORDERED"), alpha = 0.5) +
  
  labs(title = "Area Chart of Sales Data",
       x = "Index", y = "Values") +
  theme_bw()+
  
  scale_fill_manual(values = c("QUANTITYORDERED" = "lightblue", "PRICEEACH" = "orange", "SALES" = "darkgreen")) +
  theme(
    legend.position = c(0, 1),  # Place legend at top left
    legend.justification = c(0, 1),  # Anchor legend to top left
    legend.title = element_blank(),  # Remove legend title
    plot.title = element_text(hjust = 0.5),  # Center title
    legend.background = element_rect(fill = "white", color = "black", size = 0.5),  # Add frame to legend
    axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),  # Adjust y-axis title position
  ) +
  scale_y_continuous(breaks = seq(0, 14000, by = 2000)) +  # Adjust y-axis ticks
  scale_x_continuous(breaks = seq(0, 2500, by = 500))  # Adjust x-axis ticks


##Q3
#Generate Fig17 Bar Chart
library(lubridate)

# Convert to Date format "YYYY-MM-DD"
sales_data <- sales_data %>%
  mutate(ORDERDATE = as.Date(ORDERDATE, format = "%m/%d/%Y %H:%M"))

# Generate Total_Sales column 
sales_data <- sales_data %>%
  group_by(ORDERDATE) %>%
  mutate(Total_Sales = sum(QUANTITYORDERED * PRICEEACH)) 
 
# Extract columns "ORDERDATE" and "Total_Sales"
sales_date <- sales_data %>%select(ORDERDATE, Total_Sales) %>% arrange(ORDERDATE)
sales_date

# Plot line chart
ggplot(sales_date, aes(x = ORDERDATE, y = Total_Sales)) +
  geom_line(color = "blue") + 
  geom_point(color = "blue") +
  labs(title = "Daily Total Sales Over Time - Line Chart",
       x = "Date",
       y = "Total Sales") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +  # Center title  
  scale_x_date(date_breaks = "3 months", date_labels = "%Y-%m", 
               limits = as.Date(c("2003-01-01", "2005-07-01"))) +
  scale_y_continuous(limits = c(0, 140000), breaks = seq(0, 140000, by = 20000))

#Q4
#Count the categorical variables in DEALSIZE
table_dealsize <- as.data.frame(table(sales_data$DEALSIZE))
colnames(table_dealsize) <- c("DealSize", "Count")
table_dealsize <- table_dealsize %>% arrange(desc(DealSize))

#Plot Bar Chart
ggplot(table_dealsize, aes(x = reorder(DealSize, -Count), y = Count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Distribution of Deal Size-Bar Chart",
       x = "Deal Size",
       y = "Count") +
  scale_y_continuous(breaks = seq(0, 1400, by = 200))+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))   # Center title  
  

#Q5
#Count number of sales for each country
country_sales <- sales_data %>% 
  group_by(COUNTRY) %>% 
  mutate(Number_of_Sales = n()) %>% 
  distinct(COUNTRY, Number_of_Sales) %>%
  arrange(COUNTRY)

# Plot bar plot
ggplot(country_sales, aes(x = COUNTRY, y = Number_of_Sales)) +
  geom_bar(stat = "identity", fill = "royalblue") +
  labs(title = "Number of Sales by Country",
       x = "Country",
       y = "Number of Sales") +
  scale_y_continuous(breaks = seq(0, 1000, by = 200)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))  # Rotate x-axis labels for better readability




