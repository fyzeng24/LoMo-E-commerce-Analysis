## 1. Import and prepare dataset
###1.1. Load necessary packages
library(dplyr)
library(tidyverse)

# Load Order and Sales datasets
setwd("D:/EBAC/EBA5002 Business Analytics Practice/EBA2.5 BAP Practice Project/Raw Data/Original Raw Data")
customers = read_csv("001_lomo_customers_dataset.csv")
products = read_csv("004_lomo_products_dataset.csv")
orders = read_csv("006_lomo_orders_dataset.csv")
items = read_csv("007_lomo_order_items_dataset.csv")
reviews = read_csv("009_lomo_order_reviews_dataset.csv")
sellers = read_csv("002_lomo_sellers_dataset.csv")
payments = read_csv("008_lomo_order_payments_dataset.csv")

#group_by(order_id)
data <- reviews %>%
  left_join(orders,by="order_id") %>%
  left_join(items,by="order_id") %>%
  left_join(products,by="product_id") %>%
  left_join(customers,by="customer_id") %>%
  left_join(sellers,by="seller_id") %>%
  left_join(payments,by="order_id") %>%
  distinct(order_id, .keep_all = TRUE)

dim(data)
head(data,10)

sapply(data, function(x) sum(is.na(x)))

#Replace NA values with empty values
data$review_comment_title<-replace(data$review_comment_title, is.na(data$review_comment_title), "")
data$review_comment_message<-replace(data$review_comment_message, is.na(data$review_comment_message), "")
head(data)
sapply(data, class)

data$order_delivered_customer_date <- as.POSIXct(data$order_delivered_customer_date, format = "%d/%m/%Y %H:%M")
data$order_purchase_timestamp <- as.POSIXct(data$order_purchase_timestamp, format = "%d/%m/%Y %H:%M")

data1 <- data %>%
  mutate(is_reviewed = ifelse(review_comment_title!="" | review_comment_message!="",1,0)) %>%
  # Creating indicator column that shows whether delivery was made on time
  mutate(delivered_on_time=ifelse((order_delivered_customer_date <= order_estimated_delivery_date) & !is.na(order_delivered_customer_date) , 1, 0)) %>%
  # Creating indicator column that shows whether shipment was completed on time
  mutate(shipped_on_time=ifelse((order_delivered_carrier_date <= shipping_limit_date) & !is.na(order_delivered_carrier_date), 1, 0)) %>%
  # Add shipping time column (how many days it took between order and delivery)
  mutate(delivery_time = round(as.numeric(difftime(order_delivered_customer_date, order_purchase_timestamp, units ="days")),2))

head(data1$delivery_time)
sapply(data1, function(x) sum(is.na(x)))

# Rank
result <- data1 %>%
  group_by(product_category_name) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  mutate(Rank = row_number())

merged_data <- merge(data1, result, by = "product_category_name", all.x = TRUE)

head(merged_data)

# 整合分析数据
dataset <- merged_data %>% 
  select("order_id", "order_purchase_timestamp","customer_city", "customer_state", "product_category_name","Rank", "product_description_lenght", "product_photos_qty" , "order_status", "shipped_on_time", "delivered_on_time", "delivery_time", "price", "freight_value","payment_type", "payment_installments", "payment_value", "review_score", "is_reviewed", "review_English")
dim(dataset)

write.csv(dataset, file = "Total_Analysis Data.csv")
