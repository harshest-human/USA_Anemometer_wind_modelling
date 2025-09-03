######### Load libraries ###########
getwd()
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(openair)
library(gridExtra)
library(grid)
library(latticeExtra)
source("winduv_function.R") 
source("UV2WDWS_function.R")

################# Data Import and Cleaning ########################
USA_Trv_input <- winduvsh(
        folder_path = "2025_USA_Traverse_raw",
        ID = "USA.Tr",
        start_time = "2025-04-08 12:00:00",
        end_time   = "2025-04-14 23:59:59",
        output_file = "2025_04_08_USA_Trv_5_min_avg.csv")

USA_Mst_input <- winduvsh(
        folder_path = "2025_USA_16_MAST_raw",
        ID = "USA.Mst",
        start_time = "2025-04-08 00:00:00",
        end_time   = "2025-04-14 23:59:59",
        output_file = "2025_04_08_USA_Mst_5_min_avg.csv")

windmast_input <- read_csv("D:/Data Analysis/LVAT_Animal_Temperature_data/2025.04.08-2025.06.30_ATB_5_min_wind_speed_and_direction.csv", show_col_types = FALSE)


########## Day wise Windrose plots ########
dates <- c("08.04.2025", "09.04.2025", "10.04.2025",
           "11.04.2025", "12.04.2025", "13.04.2025", "14.04.2025")

plots <- list()

for (d in dates) {
        
        wind_day <- windmast_input %>%
                mutate(date = as.Date(date, format = "%d.%m.%Y")) %>%
                filter(date == dmy(d))
        
        p <- windRose(wind_day,
                      ws = "wind_speed",
                      wd = "wind_direction",
                      breaks = c(0, 1, 2, 4, 6, 12, Inf),
                      auto.text = FALSE,
                      paddle = FALSE,
                      grid.line = 10,
                      max.freq = 60,
                      key = list(labels = c("0 - 1","1 - 2","2 - 4","4 - 6","6 - 12",expression(infinity)),
                                 header = d,
                                 footer = "Wind speed (m/s)",
                                 position = "bottom"),
                      par.settings = list(axis.line = list(col = "lightgray")),
                      col = c("#4f4f4f","#0a7cb9","#f9be00","#ff7f2f","#d7153a","#800080"))
        
        # Draw rectangle on top of the plot using viewport
        pushViewport(viewport(x = 0.57, y = 0.55, width = 1, height = 1, angle = 75))
        grid.rect(x = 0.51, y = 0.59,
                  width = 0.2, height = 0.12,
                  gp = gpar(col = "black", fill = NA, alpha = 1, lty = "dotted"))
        popViewport()
        
        # Record **everything together** (plot + rectangle)
        plots[[d]] <- recordPlot()
}

