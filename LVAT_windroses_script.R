# Load required libraries
library(readr)
library(dplyr)
library(lubridate)
library(openair)

# Read data
windmast_input <- read_csv("D:/Data Analysis/LVAT_Animal_Temperature_data/2025.04.08-2025.06.30_ATB_5_min_wind_speed_and_direction.csv")

# Combine and parse datetime robustly
windmast_input <- windmast_input %>%
        mutate(
                datetime_str = paste(date, time),
                datetime = dmy_hms(datetime_str, tz = "Europe/Berlin"),
                day = as.Date(datetime),
                hour = hour(datetime)
        )

# Define wind speed breaks, labels, and colors
speed_breaks <- c(0, 1, 2, 4, 6, 12, Inf)
speed_labels <- c("0-1", "1-2", "2-4", "4-6", "6-12", ">12")
speed_colors <- c(
        "0-1"  = "deepskyblue",
        "1-2"  = "forestgreen",
        "2-4"  = "gold",
        "4-6"  = "darkorange",
        "6-12" = "red",
        ">12"  = "brown"
)

# Filter data for one day
wind_08.04 <- filter(windmast_input, day == as.Date("2025-04-08")) %>%
        mutate(date = day)   


# Plot wind rose for the day
windRose(
        wind_08.04,
        ws = "wind_speed",
        wd = "wind_direction",
        angle = 10,
        breaks = speed_breaks,
        cols = speed_colors,
        auto.text = FALSE,
        paddle = FALSE,
        key = list(
                labels = speed_labels,
                header = "Wind speed (m/s)",
                footer = format(wind_08.04$date[1], "%d.%m.%Y")),
        key.position = "bottom")
