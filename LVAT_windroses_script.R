library(readr)
library(dplyr)
library(lubridate)
library(openair)

# Read data
windmast_input <- read_csv("D:/Data Analysis/LVAT_Animal_Temperature_data/2025.04.08-2025.06.30_ATB_5_min_wind_speed_and_direction.csv", show_col_types = FALSE)

# Combine and parse datetime robustly
windmast_input <- windmast_input %>%
        mutate(
                datetime = dmy_hms(paste(date, time), tz = "Europe/Berlin"),
                date = as.Date(datetime)
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

# Define weekly intervals
week_starts <- seq(as.Date("2025-04-08"), as.Date("2025-06-30"), by = "1 week")
week_ends <- week_starts + 6

# Loop over each week to generate wind roses faceted by date
for (i in seq_along(week_starts)) {
        week_data <- windmast_input %>%
                filter(date >= week_starts[i] & date <= week_ends[i])
        
        if (nrow(week_data) > 0) {
                cat("Plotting wind roses for week", i, ":", week_starts[i], "to", week_ends[i], "\n")
                
                windRose(
                        mydata = week_data,
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
                        type = "date",
                        main = paste("Wind Roses:", week_starts[i], "to", week_ends[i])
                )
        }
}



