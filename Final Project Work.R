#Load the library
library(ggplot2)
library(readr)
library(tidyr)
library(dplyr)
library(lubridate)
library(tidyverse)

flight <- read_csv("C:/Users/Bharathi/OneDrive/Documents/Data Visualisation/DATA/flights_sample_3m.csv")
flight <- flight %>% select(-AIRLINE_DOT, -DOT_CODE, -FL_NUMBER,-WHEELS_OFF,-WHEELS_ON)



# Convert the date column (assuming it's named "DATE_COLUMN") to Date format
flight$FL_DATE <- as.Date(flight$FL_DATE)  

# Extract the year and create a new column
flight$YEAR <- year(flight$FL_DATE)

# Plot histogram
ggplot(flight, aes(x = DISTANCE)) +
  geom_histogram(binwidth = 100, fill = "steelblue", color = "white") +
  facet_wrap(~ YEAR) +
  labs(title = "Distribution of Flight Distances by Year",
       x = "Distance (miles)", y = "Count of Flights") +
  theme_minimal()

# Summarize delay reasons by airline
delay_data <- flight %>%
  filter(CANCELLED == 0, DIVERTED == 0) %>%
  group_by(AIRLINE) %>%
  summarise(
    Carrier = mean(DELAY_DUE_CARRIER, na.rm = TRUE),
    Weather = mean(DELAY_DUE_WEATHER, na.rm = TRUE),
    NAS = mean(DELAY_DUE_NAS, na.rm = TRUE),
    Security = mean(DELAY_DUE_SECURITY, na.rm = TRUE),
    LateAircraft = mean(DELAY_DUE_LATE_AIRCRAFT, na.rm = TRUE)
  ) %>%
  pivot_longer(-AIRLINE, names_to = "Reason", values_to = "Average_Delay")

ggplot(delay_data, aes(x = Reason, y = AIRLINE, fill = Average_Delay)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "plasma") +
  labs(title = "Heatmap of Delay Reasons by Airline",
       x = "Reason", y = "Airline") +
  theme_minimal()



#AIR_TIME Distritbution Histogram
flight_airtime <- flight %>%
  filter(CANCELLED == 0, DIVERTED == 0) %>%
  filter(AIRLINE_CODE %in% c("WN","DL","AA","OO","UA")) %>%
  mutate(AIR_TIME_HOURS = AIR_TIME / 60)


ggplot(flight_airtime, aes(x = AIR_TIME_HOURS)) +
  geom_histogram(binwidth = 0.25, color = "white", alpha = 0.8) +
  facet_grid(~ AIRLINE_CODE) +
  labs(title = "Histogram of Air Time (Hours) by Airline",
       x = "Air Time (hours)",
       y = "Number of Flights",
       fill = "Flight Type") +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold", size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1))

#To find out which airports are the most popular by year

flights_per_dest_year <- flight %>%
  group_by(DEST, YEAR) %>%
  summarise(Flight_Count = n(), .groups = "drop")

n_unique_dest <- flight %>%
  summarise(Number_of_Unique_DEST = n_distinct(DEST))

print(n_unique_dest)

#Since there is 380 over airports, lets explore the top 15 and bottom 15
top_dest <- flights_per_dest_year %>%
  group_by(DEST) %>%
  summarise(Total = sum(Flight_Count)) %>%
  top_n(15, Total)

top_dest_codes <- top_dest$DEST

top_dest_plot <- flights_per_dest_year %>%
  filter(DEST %in% top_dest_codes)


ggplot(top_dest_plot, aes(x = DEST, y = Flight_Count, fill = DEST)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ YEAR) +
  labs(title = "Number of Flights per Destination (Faceted by Year)",
       x = "Destination Airport",
       y = "Number of Flights") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "none") 


# Get bottom 10 destinations
bottom_10 <- flights_per_dest_year %>%
  slice_min(order_by = Flight_Count, n = 10)

# Plot bar chart
ggplot(bottom_10, aes(x = DEST, y = Flight_Count, fill = DEST)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ YEAR) +
  coord_flip() +  
  labs(title = "Bottom 10 Destination Airports by Number of Flights",
       x = "Destination Airport",
       y = "Number of Flights") +
  theme_minimal() +
  theme(legend.position = "none")



#Taxi-in time

flights_taxiin <- flight %>%
  filter(!is.na(TAXI_IN)) %>%
  filter(CANCELLED == 0, DIVERTED == 0) %>%
  filter(TAXI_IN < 60)   # to remove extreme outliers

top_15_dest <- flights_taxiin %>%
count(DEST, sort = TRUE) %>%
slice_max(n, n = 15) %>%
pull(DEST)

# Filter data to only top 15 destinations
top_dest_data <- flights_taxiin %>%
  filter(DEST %in% top_15_dest)

# Box plot of Taxi-In time by destination
ggplot(top_dest_data, aes(x = reorder(DEST, TAXI_IN, FUN = median), y = TAXI_IN, fill = DEST)) +
  geom_boxplot() +
  coord_flip() +
  scale_y_continuous(breaks = seq(0, max(top_dest_data$TAXI_IN, na.rm = TRUE), by = 5)) +
  labs(title = "Taxi-In Time by Top 15 Destination Airports",
       x = "Destination Airport",
       y = "Taxi-In Time (minutes)") +
  theme_minimal() +
  theme(legend.position = "none")  



library(usmap)
library(dplyr)
library(ggplot2)
library(tidyr)

flight <- flight %>%
  separate(ORIGIN_CITY, into = c("ORIGIN_CITY", "ORIGIN_STATE"), sep = ", ")


cancelled_flights <- flight %>%
  filter(CANCELLED == 1)

# Summarize cancellations by origin state
state_cancel <- cancelled_flights %>%
  group_by(ORIGIN_STATE, YEAR) %>%
  summarise(cancel_count = n(), .groups = "drop")

# Rename to match expected column in usmap
colnames(state_cancel)[1] <- "state"

# Plot
plot_usmap(data = state_cancel, regions = "states", values = "cancel_count") +
  facet_wrap(~ YEAR) +
  scale_fill_continuous(
    low = "lightblue", high = "darkred", name = "Cancelled Flights",
    label = scales::comma
  ) +
  labs(title = "Flight Cancellations by Origin State") +
  theme(legend.position = "right")




