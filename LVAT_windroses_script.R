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
library(purrr)
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


########## Parameters ##########
dates <- c("08.04.2025", "09.04.2025", "10.04.2025",
           "11.04.2025", "12.04.2025", "13.04.2025", "14.04.2025")

ws_breaks <- c(0, 1, 2, 4, 6, 12, Inf)
ws_labels <- c("0-1","1-2","2-4","4-6","6-12",">12")

########## Prepare data ##########
wind_all <- windmast_input %>%
        mutate(
                date = dmy(date),
                day_label = format(date, "%d.%m.%Y"),
                ws_cat = cut(wind_speed, breaks = ws_breaks, labels = ws_labels, right = FALSE),
                wd_bin = cut(
                        wind_direction %% 360,
                        breaks = seq(0, 360, by = 30),
                        include.lowest = TRUE,
                        right = FALSE
                )
        ) %>%
        filter(day_label %in% dates)

wind_summary <- wind_all %>%
        group_by(day_label, wd_bin, ws_cat) %>%
        summarise(count = n(), .groups = "drop") %>%
        group_by(day_label) %>%
        mutate(freq = count / sum(count)) %>%
        ungroup()

# Reorder factor levels so 0° bin is first
wind_summary <- wind_summary %>%
        mutate(
                wd_bin = factor(
                        wd_bin,
                        levels = levels(cut(0:359, breaks = seq(0,360, by=30), include.lowest = TRUE, right = FALSE))
                )
        )

########## Plot ##########
windrose_plot <- ggplot(wind_summary, aes(x = wd_bin, y = freq, fill = ws_cat)) +
        geom_col(color = "black", width = 1) +
        coord_polar(start = -pi/12, clip = "off") +  # rotate first bin to center at top
        facet_wrap(~day_label, nrow = 1) +
        scale_fill_manual(
                values = c("#4f4f4f","#0a7cb9","#f9be00","#ff7f2f","#d7153a","#800080"),
                name = "Wind speed (m/s)"
        ) +
        # Show all degree labels
        scale_x_discrete(labels = function(x) {
                sapply(x, function(lbl) {
                        deg <- sub("^\\[?([0-9]+),.*", "\\1", lbl)
                        paste0(deg, "°")
                })
        }) +
        scale_y_continuous(expand = c(0,0)) +
        theme_minimal(base_size = 14) +
        theme(
                axis.title = element_blank(),
                axis.text.y = element_blank(),
                axis.text.x = element_text(size = 14),
                strip.text = element_text(size = 14, face = "bold"),
                legend.title = element_text(size = 14),
                legend.text = element_text(size = 14),
                legend.position = "bottom",
                panel.spacing = unit(2, "lines"),
                plot.margin = margin(t=15,r=15,b=15,l=15)
        )

windrose_plot

########## Save ##########
ggsave("windrose_ggplot.pdf", windrose_plot, width = 20, height = 4)

