##################### Packages ##########################
getwd()
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)


########################## DWD DATA ########################################
DWD_input <- read.csv("DWD_20240105_20250707_00011.txt", header=TRUE, sep=";") %>%
        select(DWD_input, MESS_DATUM, DD_10, FF_10)


# Combine and parse datetime robustly
DWD_input <- DWD_input %>%
        mutate(date = as.Date(MESS_DATUM, format = "%d.%m.%Y"),
               wind_speed = as.numeric(FF_10),
               wind_direction = as.numeric(DD_10))

DWD_input <- filter(DWD_input, MESS_DATUM >= "2025-04-08 12:00:00", MESS_DATUM <= "2025-06-30 23:00:00")

# Function to bin wind direction into 8 sectors
bin_wind_dir_8 <- function(degrees) {
        deg_mod <- degrees %% 360
        breaks <- seq(-22.5, 337.5, by = 45)
        labels <- c("N", "NE", "E", "SE", "S", "SW", "W", "NW")
        bins <- cut(deg_mod, breaks = breaks, labels = labels, include.lowest = TRUE, right = FALSE)
        bins[is.na(bins) & deg_mod >= 337.5] <- "N"
        return(bins)
}

# Bin wind speed and wind direction
DWD_input <- DWD_input %>%
        mutate(
                ws_bin = cut(wind_speed,
                             breaks = c(0, 1, 2, 4, 6, 12, Inf),
                             right = FALSE,
                             labels = c("0-1", "1-2", "2-4", "4-6", "6-12", ">12")),
                wd_bin = bin_wind_dir_8(wind_direction)
        )

# Prepare data for plotting
plot_data <- DWD_input %>%
        count(date, wd_bin, ws_bin) %>%
        group_by(date) %>%
        mutate(freq = 100 * n / sum(n)) %>%
        ungroup()

# Color palette for wind speed bins
speed_colors <- c(
        "0-1"  = "deepskyblue",
        "1-2"  = "forestgreen",
        "2-4"  = "gold",
        "4-6"  = "darkorange",
        "6-12" = "red",
        ">12"  = "brown"
)

# Get unique dates and split into groups of 9 (3x3 grid)
unique_dates <- sort(unique(plot_data$date))
date_groups <- split(unique_dates, ceiling(seq_along(unique_dates)/6))

# Loop date wise to compile daily plots
for (i in seq_along(date_groups)) {
        group_dates <- date_groups[[i]]
        cat("Plotting dates:", paste(group_dates, collapse = ", "), "\n")
        flush.console()
        
        plot_subset <- plot_data %>% filter(date %in% group_dates)
        
        p <- ggplot(plot_subset, aes(x = wd_bin, y = freq, fill = ws_bin)) +
                geom_col(width = 1, color = "white", size = 0.2) +
                coord_polar(start = -pi/8) +
                scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20), labels = NULL) +
                scale_fill_manual(values = speed_colors, name = "Wind speed (m/s)") +
                facet_wrap(~date, ncol = 3, nrow = 2) +
                labs(x = NULL, y = NULL) +
                theme_minimal(base_size = 14) +
                theme(
                        strip.text = element_text(face = "bold", color = "white", size = 14),
                        strip.background = element_rect(fill = "gray30", color = "black"),
                        axis.text.y = element_blank(),
                        axis.text.x = element_text(face = "bold", size = 12),
                        panel.border = element_rect(color = "gray", fill = NA),
                        legend.position = "bottom",
                        legend.title = element_text(face = "bold", size = 14),
                        legend.text = element_text(size = 12)
                )
        print(p)
        
        fname <- paste0("windrose_DWD_00011", format(min(group_dates), "%Y%m%d"), "_to_", format(max(group_dates), "%Y%m%d"), ".png")
        ggsave(filename = fname, plot = p, width = 12, height = 8, dpi = 300)
        
        cat("All plots saved as", fname, "in directory:", getwd(), "\n")
}




