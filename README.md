# R----Cyclistic-Bike-Share-Analysis-Converting-Casual-Riders-to-Annual-Members
This project analyzes Cyclistic’s historical trip data from the first quarter of 2019 and 2020 to identify behavioral differences between annual members and casual riders. By examining metrics such as trip duration and weekly volume, the study provides data-driven insights to help design a marketing strategy for casual riders

title: "Cyclistic Bike share 2020"
author: "Emmanuel Aryeetey"
date: "2025-12-30"
output:
  html_document: default
  word_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## **Executive Summary**
The goal of this analysis is to identify how Annual Members and Casual Riders use Cyclistic bikes differently. By analyzing ride data from 2019 Q1 and 2020 Q1, we have identified distinct patterns in ride duration and frequency. These insights will help drive a new marketing strategy aimed at converting casual riders into long-term members.The dataset was provided by Motivate International Inc. [Click here to view data source for 2019](https://docs.google.com/spreadsheets/d/1uCTsHlZLm4L7-ueaSLwDg0ut3BP_V4mKDo2IMpaXrk4/template/preview?resourcekey=0-dQAUjAu2UUCsLEQQt20PDA#gid=1797029090) [Click here to view data source for 2020](https://docs.google.com/spreadsheets/d/179QVLO_yu5BJEKFVZShsKag74ZaUYIF6FevLYzs3hRc/template/preview#gid=640449855)

```{r Load required libraries}
library(tidyverse) # Wrangling and visualization
library(janitor)   # Data cleaning
library(lubridate) # Date and time manipulation
library(skimr)     # Data summary

bike_trip_data_n1 <- read.csv("Divvy_Trips_2019_v1.csv")
bike_trip_2020 <- read.csv("Divvy_Trips_2020_Q1.csv")
```

## **Data Preparation and Cleaning**

Before analysis, the data underwent a rigorous cleaning process to ensure accuracy. This included:

* Converting raw character strings into formal datetime objects.

* Calculating ride_length in seconds and extracting the day and month from each trip.

* Removing "bad data," such as trips with zero or negative durations, which often represent system tests or maintenance.

### For 2019 Q1 data

```{r}
# Convert IDs and Year to Character
bike_trip_data_n1$birth_year <- as.character(bike_trip_data_n1$birth_year)
bike_trip_data_n1$from_station_id <- as.character(bike_trip_data_n1$from_station_id)
bike_trip_data_n1$to_station_id <- as.character(bike_trip_data_n1$to_station_id)
bike_trip_data_n1$trip_id <- as.character(bike_trip_data_n1$trip_id)
bike_trip_data_n1$bike_id <- as.character(bike_trip_data_n1$bike_id)

# Remove the comma and convert to numeric for calculations
bike_trip_data_n1$trip_duration <- gsub(",", "", bike_trip_data_n1$trip_duration)
bike_trip_data_n1$trip_duration <- as.numeric(bike_trip_data_n1$trip_duration)
```

Converting dates using mdy_hms for Month/Day/Year format
```{r}
bike_trip_data_v3 <- bike_trip_data_n1 %>%
  mutate(
    start_time = mdy_hms(start_time),
    end_time = mdy_hms(end_time)
  )
```


I created new columns of day and month from the start_time and end_time by creating a different data version (V4)

```{r}
bike_trip_data_v4 <- bike_trip_data_v3 %>%
  mutate(
    ride_day = wday(start_time, label = TRUE, abbr = FALSE),
    ride_month = month(start_time, label = TRUE, abbr = FALSE)
  )
```
Volume by day and month for both members and casual users

```{r}
volume_by_day <- bike_trip_data_v4 %>%
  count(user_type, ride_day) %>%
  arrange(ride_day)

```

```{r}
volume_by_month <- bike_trip_data_v4 %>%
  count(user_type, ride_month) %>%
  arrange(ride_month)
```



### For 2020 Q1 data

```{r pressure, echo=FALSE}
# 1. Convert columns from character to datetime
# 2. Calculate ride_length in minutes
bike_trip_2020 <- bike_trip_2020 %>%
  mutate(
    started_at = ymd_hms(started_at),
    ended_at = ymd_hms(ended_at),
    ride_length = as.numeric(difftime(ended_at, started_at, units = "secs"))
  )

```


```{r}
# Remove 'bad data': rides less than 0 seconds (maintenance/errors)
bike_trip_2020_v2 <- bike_trip_2020 %>% 
  filter(ride_length > 0)
```

```{r}
bike_trip_2020_v2 <- bike_trip_2020_v2 %>%
  mutate(
    # Convert to datetime
    started_at = ymd_hms(started_at),
    ended_at = ymd_hms(ended_at),
    
    # Calculate ride length in seconds
    ride_length = as.numeric(difftime(ended_at, started_at, units = "secs")),
    
    # Create Day of Week column (Sunday, Monday, etc.)
    day_of_week = wday(started_at, label = TRUE, abbr = FALSE),
    
    # Create Month column (January, February, etc.)
    month = month(started_at, label = TRUE, abbr = FALSE)
  ) %>%
  # Filter out data quality issues (rides <= 0 seconds)
  filter(ride_length > 0)
```


## **Analyzing Rider Behavior**

### Prefered bike for causual and members.

```{r}
bike_trip_2020_v2 %>%
  # Step 1: Count the rides by month and user type
  group_by(month, member_casual) %>%
  summarise(number_of_rides = n(), .groups = 'drop') %>%
  # Step 2: Plot using the new 'number_of_rides' column
  ggplot(aes(x = month, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") + 
  scale_y_continuous(labels = scales::comma) + # This now works because y is a number!
  scale_fill_manual(values = c("member" = "#2c3e50", "casual" = "#e74c3c")) +
  labs(
    title = "Docked Bike Usage: Members vs. Casual",
    subtitle = "Monthly trends for Q1 2020",
    x = "Month",
    y = "Total Number of Rides",
    fill = "User Type"
  ) +
  theme_minimal()

```

### Ride demand throughout the day

This is to show the behavior of both casual and members in usage of our bikes during the day from weekdays and weekends 

```{r}
# 1. Prepare and Group Data
# We define Weekdays (Mon-Fri) and Weekends (Sat-Sun)
hourly_grouped <- bind_rows(
  bike_trip_data_v4 %>% 
    mutate(year = "2019", hour = hour(start_time), day = ride_day,
           user_category = ifelse(user_type == "Subscriber", "Member", "Casual")),
  bike_trip_2020_v2 %>% 
    mutate(year = "2020", hour = hour(started_at), day = day_of_week,
           user_category = str_to_title(member_casual))
) %>%
  mutate(day_type = case_when(
    day %in% c("Saturday", "Sunday") ~ "Weekend",
    TRUE ~ "Weekday"
  )) %>%
  group_by(year, day_type, hour, user_category) %>%
  summarise(number_of_rides = n(), .groups = 'drop')

Visualization

ggplot(hourly_grouped, aes(x = hour, y = number_of_rides, color = user_category)) +
  geom_line(linewidth = 1.2) +
  # Facet by Year (Rows) and Day Type (Columns)
  facet_grid(year ~ day_type, scales = "free_y") +
  
  scale_x_continuous(breaks = seq(0, 23, by = 4), 
                     labels = c("12am", "4am", "8am", "12pm", "4pm", "8pm")) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(values = c("Member" = "#2c3e50", "Casual" = "#e74c3c")) +
  
  labs(
    title = "Weekdays vs. Weekends",
    subtitle = "Members dominate the 8am/5pm commute; Casuals usage increase during weekend afternoons",
    x = "Time of Day",
    y = "Total Rides",
    color = "User Type"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold", size = 14),
    panel.spacing = unit(2, "lines"),
    axis.text.x = element_text(size = 10, face = "bold"),
    legend.position = "bottom",
    plot.title = element_text(size = 18, face = "bold")
  )
```


###  How Long Do They Ride?

To understand the purpose of these trips, I first looked at how long each user group keeps a bike.

```{r}
# 1. Summarize 2019 Average Duration (Minutes)
avg_dur_2019 <- bike_trip_data_v4 %>%
  group_by(user_type) %>%
  summarise(avg_duration = mean(trip_duration, na.rm = TRUE) / 60, .groups = 'drop') %>%
  mutate(
    year = "2019 Q1",
    user_category = ifelse(user_type == "Subscriber", "Member", "Casual")
  )

# 2. Summarize 2020 Average Duration (Minutes)
# Note: your 2020 ride_length was in seconds, so we divide by 60
avg_dur_2020 <- bike_trip_2020_v2 %>%
  group_by(member_casual) %>%
  summarise(avg_duration = mean(ride_length, na.rm = TRUE) / 60, .groups = 'drop') %>%
  mutate(
    year = "2020 Q1",
    user_category = str_to_title(member_casual)
  )


combined_avg_duration <- bind_rows(avg_dur_2019, avg_dur_2020)


ggplot(combined_avg_duration, aes(x = user_category, y = avg_duration, fill = user_category)) +
  geom_col(width = 0.7) +
  facet_wrap(~year) +
  # Add text labels on top of bars so people don't have to guess the number
  geom_text(aes(label = paste0(round(avg_duration, 1), " min")), 
            vjust = -0.5, fontface = "bold") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) + # Adds space for the labels
  scale_fill_manual(values = c("Member" = "#2c3e50", "Casual" = "#e74c3c")) +
  labs(
    title = "The 'Duration Gap': How Long Do They Ride?",
    subtitle = "Casual riders consistently ride 3x to 4x longer than Members",
    x = "",
    y = "Average Trip Duration (Minutes)",
    fill = "User Type"
  ) +
  theme_minimal() +
  theme(
    strip.background = element_rect(fill = "#ecf0f1", color = "black"),
    strip.text = element_text(face = "bold", size = 12),
    legend.position = "none" # Legend is redundant because of x-axis labels
  )
```


Insight: Casual riders ride for nearly three times longer than annual members. This suggests that while members use the bikes for efficient transit, casual riders use them for leisure and exploration.

### B. When Do They Ride? (Weekly Trends)
Next, we looked at which days of the week are most popular for each group.

```{r}
# 1. Prepare 2019 Weekly Summary
summary_weekly_2019 <- bike_trip_data_v4 %>%
  group_by(ride_day, user_type) %>%
  summarise(number_of_rides = n(), .groups = 'drop') %>%
  mutate(
    year = "2019 Q1",
    member_casual = ifelse(user_type == "Subscriber", "member", "casual")
  ) %>%
  rename(day_of_week = ride_day)

# 2. Prepare 2020 Weekly Summary
summary_weekly_2020 <- bike_trip_2020_v2 %>%
  group_by(day_of_week, member_casual) %>%
  summarise(number_of_rides = n(), .groups = 'drop') %>%
  mutate(year = "2020 Q1")


combined_weekly <- bind_rows(summary_weekly_2019, summary_weekly_2020)

ggplot(combined_weekly, aes(x = day_of_week, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  facet_wrap(~year) +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("member" = "#2c3e50", "casual" = "#e74c3c")) +
  labs(
    title = "Weekly Usage Comparison: 2019 vs. 2020 Q1",
    subtitle = "Comparing ride volume across days of the week",
    x = "Day of the Week",
    y = "Total Trips",
    fill = "User Type"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
```


Insight: Annual members have a very steady usage pattern from Monday to Friday, suggesting a commuter-based behavior. Conversely, Casual ridership spikes on Saturday and Sunday, confirming that their usage is primarily recreational.


### C. Seasonality: Which Months are Most Popular?
Finally, we analyzed the monthly volume to see how the time of year affects behavior.

```{r}
# 1. Prepare 2019 Summary (Normalizing names to 'member' and 'casual')
summary_2019 <- bike_trip_data_v4 %>%
  group_by(ride_month, user_type) %>%
  summarise(number_of_rides = n(), .groups = 'drop') %>%
  mutate(
    year = "2019 Q1",
    member_casual = ifelse(user_type == "Subscriber", "member", "casual")
  ) %>%
  rename(month = ride_month)

# 2. Prepare 2020 Summary
summary_2020 <- bike_trip_2020_v2 %>%
  group_by(month, member_casual) %>%
  summarise(number_of_rides = n(), .groups = 'drop') %>%
  mutate(year = "2020 Q1")

combined_data <- bind_rows(summary_2019, summary_2020)


ggplot(combined_data, aes(x = month, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") + # Puts Member/Casual bars side-by-side
  facet_wrap(~year) + # Creates the 2019 vs 2020 side-by-side view
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("member" = "#2c3e50", "casual" = "#e74c3c")) +
  labs(
    title = "Market Evolution: 2019 vs. 2020 Q1",
    subtitle = "Comparing monthly trends for Members and Casual riders",
    x = "Month",
    y = "Total Trips",
    fill = "User Type"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"), # Formats the '2019'/'2020' labels
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1) # Tilts months for easier reading
  )
```

In a typical year like 2019 Q1, March marks the beginning of spring.

  * Subscribers (Members) who took a break during the freezing months of January and February return to the bikes as soon as the ice melts. This is weather-driven. People are eager to get back to their routine commutes and avoid stuffy buses or trains now that the temperature is rising.


In 2020 Q1, the data stops following the weather and starts following the news.

  * Instead of the usual spring "bounce," Member rides continued to drop linearly from January through to March. This is policy-driven. In March 2020, the COVID-19 pandemic hit.
Most members are office workers. When offices closed in March, the morning and evening commute peaks simply vanished.

Unlike 2019 Q1, where people felt safe to go out in March, the 2020 lock downs kept people inside, regardless of how nice the weather was.

### Top 10 Stations Comparison

```{r}
# 1. Process 2019 Q1 Casual Stations (Labels were 'Customer')
casual_stations_2019 <- bike_trip_data_v4 %>%
  filter(user_type == "Customer") %>%
  group_by(from_station_name) %>%
  summarise(number_of_rides = n(), .groups = 'drop') %>%
  mutate(year = "2019 Q1") %>%
  rename(station_name = from_station_name) %>%
  slice_max(number_of_rides, n = 10)

# 2. Process 2020 Q1 Casual Stations (Labels were 'casual')
casual_stations_2020 <- bike_trip_2020_v2 %>%
  filter(member_casual == "casual") %>%
  group_by(start_station_name) %>%
  summarise(number_of_rides = n(), .groups = 'drop') %>%
  mutate(year = "2020 Q1") %>%
  rename(station_name = start_station_name) %>%
  slice_max(number_of_rides, n = 10)


combined_casual_top10 <- bind_rows(casual_stations_2019, casual_stations_2020)

ggplot(combined_casual_top10, aes(x = reorder(station_name, number_of_rides), y = number_of_rides)) +
  geom_col(fill = "#e74c3c") + # Using the 'Casual Red' color
  coord_flip() +
  facet_wrap(~year, scales = "free") +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Top 10 Stations for Casual Riders Only",
    subtitle = "Q1 2019 vs. Q1 2020 comparison",
    x = "Station Name",
    y = "Number of Rides"
  ) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold", size = 12))
```



## **Final Conclusions and Recommendations**

### Key Findings

  * Members are Commuters: They ride consistently during the week for shorter duration.

  * Casuals are Explorers: They ride mostly on weekends and keep the bikes out for much longer periods.

  * Seasonality Matters: Usage for both groups is heavily dictated by the time of year, with spring showing the highest growth potential.

### Recommendations

  * Create a "Weekend-Only" membership tier to convert casual riders who only use the system on Saturdays and Sundays.

  * Use marketing materials to show casual riders that if they ride for more than 30 minutes twice a month, an annual membership is more cost-effective.

  * Launch the heavy-hitting marketing campaigns in late March or early April to capture the casual riders as they return to the system for the warmer months.
