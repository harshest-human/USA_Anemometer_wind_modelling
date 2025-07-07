##################### Packages ##########################
getwd()
library(tidyverse)
library(reshape2)
library(hablar)
library(lubridate)
library(psych)
library(rmarkdown)
library(ggplot2)
library(readxl)
library(readr)
library(dplyr)
library(openair)
library(rvest)

############ Windmast Import ################
windmast_input = read.csv("D:/Data Analysis/LVAT_Animal_Temperature_data/2025.04.08-2025.06.30_ATB_5_min_wind_speed_and_direction.csv")

# Convert to datetime
windmast_input$date <- as.POSIXct(paste(windmast_input$date, windmast_input$time), 
                                  format = "%d.%m.%Y %H:%M:%S")

# Filter the date range of interest
windmast_input <- windmast_input %>%
        filter(date >= as.POSIXct("2025-04-08 00:00:00"),
               date <= as.POSIXct("2025-06-30 23:59:59"))

# Add a 'week' factor column (start of week as label)
windmast_input <- windmast_input %>%
        mutate(week = paste0("Week ", isoweek(date), ": ", format(floor_date(date, "week", week_start = 1), "%d.%m"), 
                             " - ", format(floor_date(date, "week", week_start = 1) + days(6), "%d.%m")))

# Optional: Check how many weeks there are
unique(windmast_input$week)


############ Windrose Visualization ################
# Create weekly wind roses using openair's `type` argument
windRose(windmast_input,
         ws = "wind_speed",
         wd = "wind_direction",
         type = "week",                # Facet by week
         breaks = c(0, 1, 2, 4, 6, 12),
         auto.text = FALSE,
         paddle = FALSE,
         grid.line = 5,
         key = list(labels = c("0 - 1",
                               "1 - 2",
                               "2 - 4",
                               "4 - 6",
                               "6 - 12")),
         key.header = "08.04.2025 - 30.06.2025",
         key.footer = "Wind speed (m/s)",
         key.position = "bottom",
         par.settings = list(axis.line = list(col = "lightgray")),
         col = c("#4f4f4f", "#0a7cb9", "#f9be00", "#ff7f2f", "#d7153a"))

