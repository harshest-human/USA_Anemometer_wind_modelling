# Load required libraries
library(readr)
library(dplyr)
library(lubridate)
library(openair)

# Read data
windmast_input <- read_csv("D:/Data Analysis/LVAT_Animal_Temperature_data/2025.04.08-2025.06.30_ATB_5_min_wind_speed_and_direction.csv",show_col_types = FALSE)

# Date formatting 
windmast_input <- windmast_input %>%
        mutate(date = as.Date(date, format = "%d.%m.%Y"))


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

# Plot wind rose for the day
windRose(
        windmast_input,
        ws = "wind_speed",
        wd = "wind_direction",
        angle = 30,
        breaks = speed_breaks,
        cols = speed_colors,
        auto.text = FALSE,
        paddle = FALSE,
        key = list(
                labels = speed_labels,
                header = "Wind speed (m/s)"
        ),
        key.position = "bottom",
        type = "date",      # facet by date
        layout = c(7, 2)    # 3 columns and 3 rows (adjust as needed)
)

