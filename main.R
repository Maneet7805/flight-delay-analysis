# ==================================================
# TEAM MEMBERS
# ==================================================
# MANEET ARVIND MEHTA, TP082340
# AUBDOOL ABDOOL RASHID MUHMMAD SHAAD, TP081115 
# MAYUR DEWKURUN, TP081112 
# ==================================================

# ==================================================
# 1) Data Import
# ==================================================
# Load required libraries
library(dplyr)
library(ggplot2)
library(stringr)
library(broom)
library(readxl)

# Set working directory
setwd("C:\\Users\\Maneet\\Downloads\\2 Dataset")

# Load Data Set
flights_data <- read.csv("flights.csv")
flights_data
# Load airline names
airlines <- read.csv("iata_airline_codes.csv")
airlines

# ==================================================
# 2) Cleaning / Pre-processing
# ==================================================

# Step 2.1: Remove duplicates
flights_clean <- distinct(flights_data)
flights_clean

# Step 2.2: Handle missing values
flights_clean <- flights_clean %>%
  mutate(
    CANCELLATION_REASON = ifelse(is.na(CANCELLATION_REASON), "Not Cancelled", CANCELLATION_REASON),
    across(c(AIR_SYSTEM_DELAY, SECURITY_DELAY, AIRLINE_DELAY, LATE_AIRCRAFT_DELAY, WEATHER_DELAY),
           ~ ifelse(is.na(.), 0, .))
  )
flights_clean

# Step 2.3: Keep only completed flights
flights_completed <- flights_clean %>%
  filter(CANCELLED == 0)
flights_completed

# Step 2.4: Drop irrelevant columns
flights_completed <- flights_completed %>%
  select(-c(DIVERTED:CANCELLATION_REASON, AIR_SYSTEM_DELAY:WEATHER_DELAY))
flights_completed

# Step 2.5: Drop rows with NA in key variables
flights_completed <- flights_completed %>%
  filter(!is.na(ARRIVAL_DELAY) &
           !is.na(DEPARTURE_DELAY) &
           !is.na(DISTANCE) &
           !is.na(DAY_OF_WEEK) &
           !is.na(TAXI_OUT) &
           !is.na(SCHEDULED_DEPARTURE) &
           !is.na(AIRLINE))
flights_completed

# Step 2.6: Correct data types
flights_completed <- flights_completed %>%
  mutate(
    YEAR = as.integer(YEAR),
    MONTH = as.integer(MONTH),
    DAY = as.integer(DAY),
    DAY_OF_WEEK = as.factor(DAY_OF_WEEK),
    AIRLINE = as.factor(AIRLINE),
    ORIGIN_AIRPORT = as.factor(ORIGIN_AIRPORT),
    DESTINATION_AIRPORT = as.factor(DESTINATION_AIRPORT),
    FLIGHT_NUMBER = as.character(FLIGHT_NUMBER),
    TAIL_NUMBER = as.character(TAIL_NUMBER)
  )
flights_completed

# Step 2.7: Extract Scheduled Departure Hour
flights_completed <- flights_completed %>%
  mutate(SCHED_DEP_HH = floor(SCHEDULED_DEPARTURE / 100))
flights_completed

# ==================================================
# 3) Data Validation
# ==================================================

# Step 3.1: Check missing values
missing_summary <- sapply(flights_completed, function(x) sum(is.na(x)))
missing_summary

# Step 3.2: Check unrealistic values
invalid_summary <- list(
  negative_distance = sum(flights_completed$DISTANCE < 0, na.rm = TRUE),
  negative_taxi_out = sum(flights_completed$TAXI_OUT < 0, na.rm = TRUE),
  invalid_sched_dep = sum(flights_completed$SCHEDULED_DEPARTURE < 0 | flights_completed$SCHEDULED_DEPARTURE > 2359, na.rm = TRUE),
  invalid_day_of_week = sum(!flights_completed$DAY_OF_WEEK %in% 1:7, na.rm = TRUE),
  extreme_negative_departure_delay = sum(flights_completed$DEPARTURE_DELAY < -60, na.rm = TRUE),
  extreme_negative_arrival_delay = sum(flights_completed$ARRIVAL_DELAY < -60, na.rm = TRUE)
)
invalid_summary

# ==================================================
# 4) Objective 1 Plots
# ==================================================
# Contributor: MANEET ARVIND MEHTA, TP082340
# ==================================================

# Plot 4.1: Departure vs Arrival Delay
ggplot(flights_completed, aes(x = DEPARTURE_DELAY, y = ARRIVAL_DELAY, colour = AIRLINE)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", color = "darkred") +
  labs(
    title = "Relationship Between Departure Delay and Arrival Delay",
    x = "Departure Delay (minutes)",
    y = "Arrival Delay (minutes)",
    colour = "Airline"
  )

# Plot 4.2: Effect of Flight Distance
flights_completed <- flights_completed %>%
  mutate(DISTANCE_GROUP = ntile(DISTANCE, 4))  # Quartiles

ggplot(flights_completed, aes(x = DEPARTURE_DELAY, y = ARRIVAL_DELAY, color = as.factor(DISTANCE_GROUP))) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Effect of Flight Distance on Delay Propagation",
    x = "Departure Delay (minutes)",
    y = "Arrival Delay (minutes)",
    color = "Distance Group\n(1 = Shortest, 4 = Longest)"
  ) 

slopes_by_group <- flights_completed %>%
  group_by(DISTANCE_GROUP) %>%
  do(model = lm(ARRIVAL_DELAY ~ DEPARTURE_DELAY, data = .)) %>%
  summarise(
    DISTANCE_GROUP,
    slope = coef(model)[["DEPARTURE_DELAY"]],
    intercept = coef(model)[["(Intercept)"]]
  )
slopes_by_group

# Plot 4.3: Delay Propagation by Day of the Week
ggplot(flights_completed, aes(x = DEPARTURE_DELAY, y = ARRIVAL_DELAY)) +
  geom_point(alpha = 0.2, color = "purple") +
  geom_smooth(method = "lm", color = "black") +
  facet_wrap(~ DAY_OF_WEEK) +
  labs(
    title = "Delay Propagation by Day of the Week",
    x = "Departure Delay (minutes)",
    y = "Arrival Delay (minutes)"
  ) 

# ==================================================
# 5) Objective 2 Plots
# ==================================================
# Contributor: AUBDOOL ABDOOL RASHID MUHMMAD SHAAD, TP081115
# ==================================================

# Plot 5.1: Average Arrival Delay by Scheduled Departure Hour
avg_delay_by_hour <- flights_completed %>%
  group_by(SCHED_DEP_HH) %>%
  summarise(mean_arrival_delay = mean(ARRIVAL_DELAY, na.rm = TRUE))

ggplot(avg_delay_by_hour, aes(x = SCHED_DEP_HH, y = mean_arrival_delay)) +
  geom_col(fill = "darkorange") +
  labs(
    title = "Average Arrival Delay by Scheduled Departure Hour",
    subtitle = "Shows effect of congestion during peak hours",
    x = "Scheduled Departure Hour (0–23)",
    y = "Average Arrival Delay (minutes)"
  )

flight_volume_by_hour <- flights_completed %>%
  group_by(SCHED_DEP_HH) %>%
  summarise(flight_count = n())

ggplot(flight_volume_by_hour, aes(x = SCHED_DEP_HH, y = flight_count)) +
  geom_col(fill = "skyblue") +
  labs(title = "Flight Volume by Scheduled Departure Hour",
       x = "Hour of Day", y = "Number of Flights")

# Plot 5.2: Taxi-Out Time vs Arrival Delay
ggplot(flights_completed, aes(x = TAXI_OUT, y = ARRIVAL_DELAY)) +
  geom_point(alpha = 0.3, color = "purple") +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(
    title = "Taxi-Out Time vs Arrival Delay",
    subtitle = "Reflects effect of airport congestion on delays",
    x = "Taxi-Out Time (minutes)",
    y = "Arrival Delay (minutes)"
  ) 

# ==================================================
# 5) Objective 3 Plot
# ==================================================
# Contributor: MAYUR DEWKURUN, TP081112 
# ==================================================

# Join airline names to completed flights data set
flights_completed <- flights_completed %>%
  left_join(airlines %>% rename(airline_name = AIRLINE), by = c("AIRLINE" = "IATA_CODE"))

# Plot 6.1: Descriptive Statistics
avg_delay_airline <- flights_completed %>%
  group_by(AIRLINE, airline_name) %>%
  summarise(
    mean_arrival_delay = mean(ARRIVAL_DELAY, na.rm = TRUE),
    median_arrival_delay = median(ARRIVAL_DELAY, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_arrival_delay))
avg_delay_airline

# Visualization
# Plot 6:1: Bar Chart: Average Delay by Airline
ggplot(avg_delay_airline,
       aes(x = reorder(coalesce(airline_name, as.character(AIRLINE)), mean_arrival_delay),
           y = mean_arrival_delay)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = sprintf("%.1f", mean_arrival_delay)), hjust = -0.1, size = 3) +
  coord_flip(clip = "off") +
  labs(title = "Average Arrival Delay by Airline",
       subtitle = "Completed flights only",
       x = "Airline", y = "Mean Arrival Delay (minutes)")

# Plot 6.2: Violin Plot: Delay Distribution by Airline
ggplot(flights_completed, aes(x = AIRLINE, y = ARRIVAL_DELAY, fill = AIRLINE)) +
  geom_violin(trim = FALSE, show.legend = FALSE, alpha = 0.7) +
  labs(title = "Arrival Delay Distribution by Airline",
       x = "Airline", y = "Arrival Delay (minutes)") 

# Plot 6.3: Regression Analysis
model_obj3 <- lm(ARRIVAL_DELAY ~ DEPARTURE_DELAY + DISTANCE + DAY_OF_WEEK + AIRLINE, data = flights_completed)
summary(model_obj3)

airline_effects <- tidy(model_obj3) %>%
  filter(grepl("^AIRLINE", term)) %>%
  mutate(airline = gsub("^AIRLINE", "", term)) %>%
  arrange(estimate)

airline_effects

# Regression-adjusted airline effects plot
ggplot(airline_effects, aes(x = reorder(airline, estimate), y = estimate)) +
  geom_col(fill = "darkorange") +
  coord_flip() +
  labs(title = "Adjusted Airline Effects on Arrival Delay",
       x = "Airline", y = "Effect on Arrival Delay (minutes)") 
