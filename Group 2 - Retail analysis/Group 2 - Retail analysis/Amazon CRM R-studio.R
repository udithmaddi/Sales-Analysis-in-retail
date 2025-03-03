# Install packages if not already installed
install.packages(c("dplyr", "ggplot2", "lubridate"))

# Load libraries
library(dplyr)
library(ggplot2)
library(lubridate)

# Load the dataset
install.packages("readr")
library(readr)
amazon_data <- read_csv("C:/Users/rvsja/Downloads/PP_amazon_data.csv")


# Convert date and time columns to appropriate formats
amazon_data <- amazon_data %>%
  mutate(
    Order_Date = as.Date(Order_Date),
    Order_Time = as.POSIXct(Order_Time, format = "%Y-%m-%d %H:%M:%S"),
    Pickup_Time = as.POSIXct(Pickup_Time, format = "%Y-%m-%d %H:%M:%S"),
    Month = month(Order_Date, label = TRUE),
    Time_of_Day = case_when(
      hour(Order_Time) < 12 ~ "Morning",
      hour(Order_Time) < 18 ~ "Afternoon",
      TRUE ~ "Evening"
    )
  )

# 1. Bar Plot - Average Delivery Time by Area
average_delivery_by_area <- amazon_data %>%
  group_by(Area) %>%
  summarise(Average_Delivery_Time = mean(Delivery_Time, na.rm = TRUE))

ggplot(average_delivery_by_area, aes(x = Area, y = Average_Delivery_Time)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Average Delivery Time by Area", x = "Area", y = "Average Delivery Time (minutes)")

# 2. Box Plot - Delivery Time Distribution by Time of Day
ggplot(amazon_data, aes(x = Time_of_Day, y = Delivery_Time, fill = Time_of_Day)) +
  geom_boxplot() +
  labs(title = "Delivery Time Distribution by Time of Day", x = "Time of Day", y = "Delivery Time (minutes)")

# 3. Line Plot - Average Delivery Time by Month
average_delivery_by_month <- amazon_data %>%
  group_by(Month) %>%
  summarise(Average_Delivery_Time = mean(Delivery_Time, na.rm = TRUE))

ggplot(average_delivery_by_month, aes(x = Month, y = Average_Delivery_Time)) +
  geom_line(group = 1, color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  labs(title = "Average Delivery Time by Month", x = "Month", y = "Average Delivery Time (minutes)")

# 4. Bar Plot - Average Delivery Time by Weather Condition
# Calculate average delivery time by weather condition
average_delivery_by_weather <- amazon_data %>%
  group_by(Weather) %>%
  summarise(Average_Delivery_Time = mean(Delivery_Time, na.rm = TRUE))

# Create pie chart for average delivery time by weather condition
ggplot(average_delivery_by_weather, aes(x = "", y = Average_Delivery_Time, fill = Weather)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Average Delivery Time by Weather Condition") +
  theme_void() +  # Remove axes and background
  theme(legend.title = element_blank())
