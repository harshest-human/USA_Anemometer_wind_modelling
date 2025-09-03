######## Development of function ########
winduvsh <- function(folder_path, ID, start_time = NULL, end_time = NULL, output_file = "compiled_wind_data.csv") {
        library(dplyr)
        library(lubridate)
        library(stringr)
        
        # Helper function: Convert u, v to wind direction and speed
        uv2wdws <- function(u, v) {
                degrees <- function(radians) 180 * radians / pi
                mathdegs <- degrees(atan2(v, u))
                wdcalc <- ifelse(mathdegs > 0, mathdegs, mathdegs + 360)
                wd <- ifelse(wdcalc < 270, 270 - wdcalc, 270 - wdcalc + 360)
                ws <- sqrt(u^2 + v^2)
                return(data.frame(wd = wd, ws = ws))
        }
        
        files <- list.files(folder_path, pattern = "\\.txt$", full.names = TRUE)
        if (length(files) == 0) {
                message("No .txt files found in folder.")
                return(NULL)
        }
        
        message("Processing ", length(files), " file(s):")
        full_data <- data.frame()
        
        for (i in seq_along(files)) {
                file <- files[i]
                cat(sprintf(" [%02d/%02d] %s\n", i, length(files), basename(file)))
                
                lines <- readLines(file, warn = FALSE)
                
                # Gill-style lines: start with "H,"
                gill_lines <- grep("^H,", lines, value = TRUE)
                
                if (length(gill_lines) > 0) {
                        parsed <- str_split_fixed(gill_lines, ",", 9)
                        u_vals <- as.numeric(parsed[, 2])
                        v_vals <- as.numeric(parsed[, 3])
                        w_vals <- as.numeric(parsed[, 4])
                        dt_vals <- suppressWarnings(dmy_hms(parsed[, 9]))
                        
                        df <- data.frame(
                                u = u_vals,
                                v = v_vals,
                                w = w_vals,
                                DATE.TIME = dt_vals,
                                stringsAsFactors = FALSE
                        )
                } else {
                        # USA-style lines: tab-separated, values usually from column 6 onward
                        usa_lines <- lines[nchar(lines) > 20]
                        parsed <- str_split_fixed(usa_lines, "\t", 10)
                        
                        # Corrected column positions: assuming 6 = U, 7 = V, 8 = W
                        u_vals <- as.numeric(str_replace_all(parsed[, 6], ",", "."))
                        v_vals <- as.numeric(str_replace_all(parsed[, 7], ",", "."))
                        w_vals <- as.numeric(str_replace_all(parsed[, 8], ",", "."))
                        
                        date_time_str <- paste(parsed[, 2], str_replace(parsed[, 3], ",", "."))
                        dt_vals <- suppressWarnings(dmy_hms(date_time_str))
                        
                        df <- data.frame(
                                u = u_vals,
                                v = v_vals,
                                w = w_vals,
                                DATE.TIME = dt_vals,
                                stringsAsFactors = FALSE
                        )
                }
                
                df <- df %>%
                        filter(!is.na(DATE.TIME)) %>%
                        mutate(ID = ID)
                
                full_data <- bind_rows(full_data, df)
        }
        
        if (nrow(full_data) == 0) {
                message("No valid data found across all files.")
                return(NULL)
        }
        
        # Optional filtering by datetime
        if (!is.null(start_time)) {
                start_time <- ymd_hms(start_time)
                end_time   <- ymd_hms(end_time)
                full_data <- full_data %>%
                        filter(DATE.TIME >= start_time & DATE.TIME <= end_time)
        }
        
        # Calculate wind direction (wd) and wind speed (ws)
        wdws_df <- uv2wdws(full_data$u, full_data$v)
        full_data <- cbind(full_data, wdws_df)
        
        # Write to CSV
        write.csv(full_data, output_file, row.names = FALSE)
        message("Output written to: ", output_file)
        
        return(full_data)
}


########## Example winduv #########
#windmast_input <- winduvsh(
        #folder_path = "2025_USA_16_MAST_raw",
        #ID = "USA.Mst",
        #start_time = "2025-04-08 00:00:00",
        #end_time   = "2025-04-14 23:59:59",
        #output_file = "2025_04_08_USA_Mst_5_min_avg.csv")



########## Development of harose function #########
windrosesh <- function(data,
                   date_col = "date",
                   ws_col = "wd_speed",
                   wd_col = "wd_direction",
                   bin_ws_breaks = c(0, 1, 2, 4, 6, 12, Inf),
                   bin_ws_labels = c("0-1", "1-2", "2-4", "4-6", "6-12", ">12"),
                   wind_dir_sectors = 8,
                   facet_ncol = 3,
                   facet_nrow = 2,
                   save_dir = getwd(),
                   save_prefix = "windrose",
                   group_size = 6,
                   speed_colors = c(
                           "0-1"  = "deepskyblue",
                           "1-2"  = "forestgreen",
                           "2-4"  = "gold",
                           "4-6"  = "darkorange",
                           "6-12" = "red",
                           ">12"  = "brown"
                   )) {
        
        # Load required packages
        require(dplyr)
        require(ggplot2)
        require(lubridate)
        require(stringr)
        
        # Input checks
        stopifnot(is.data.frame(data))
        stopifnot(all(c(date_col, ws_col, wd_col) %in% names(data)))
        
        # Ensure date_col is Date type (or POSIX), convert if needed
        if (!inherits(data[[date_col]], "Date") && !inherits(data[[date_col]], "POSIXt")) {
                data[[date_col]] <- as.Date(data[[date_col]])
        }
        
        # Wind direction binning function (8 or 16 sectors)
        bin_wind_dir <- function(degrees, sectors = 8) {
                sectors <- match.arg(as.character(sectors), choices = c("8", "16"))
                sectors <- as.numeric(sectors)
                deg_mod <- degrees %% 360
                angle_step <- 360 / sectors
                half_step <- angle_step / 2
                
                breaks <- seq(-half_step, 360 - half_step, by = angle_step)
                
                if (sectors == 8) {
                        labels <- c("N", "NE", "E", "SE", "S", "SW", "W", "NW")
                } else {
                        labels <- c("N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE",
                                    "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW")
                }
                
                bins <- cut(deg_mod, breaks = breaks, labels = labels, include.lowest = TRUE, right = FALSE)
                bins[is.na(bins) & deg_mod >= (360 - half_step)] <- "N"
                return(bins)
        }
        
        data <- data %>%
                mutate(
                        ws_bin = cut(.data[[ws_col]],
                                     breaks = bin_ws_breaks,
                                     right = FALSE,
                                     labels = bin_ws_labels),
                        wd_bin = bin_wind_dir(.data[[wd_col]], sectors = wind_dir_sectors)
                )
        
        plot_data <- data %>%
                count(.data[[date_col]], wd_bin, ws_bin) %>%
                group_by(across(all_of(date_col))) %>%
                mutate(freq = 100 * n / sum(n)) %>%
                ungroup()
        
        unique_dates <- sort(unique(plot_data[[date_col]]))
        date_groups <- split(unique_dates, ceiling(seq_along(unique_dates) / group_size))
        
        plots <- list()
        
        for (i in seq_along(date_groups)) {
                group_dates <- date_groups[[i]]
                message("Plotting dates: ", paste(group_dates, collapse = ", "))
                
                plot_subset <- plot_data %>% filter(.data[[date_col]] %in% group_dates)
                
                p <- ggplot(plot_subset, aes(x = wd_bin, y = freq, fill = ws_bin)) +
                        geom_col(width = 1, color = "white", size = 0.2) +
                        coord_polar(start = -pi / length(bin_ws_labels)) +
                        scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20), labels = NULL) +
                        scale_fill_manual(values = speed_colors, name = "Wind speed (m/s)") +
                        facet_wrap(as.formula(paste("~", date_col)), ncol = facet_ncol, nrow = facet_nrow) +
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
                
                plots[[i]] <- p
                
                fname <- file.path(save_dir,
                                   paste0(save_prefix, "_",
                                          format(min(group_dates), "%Y%m%d"),
                                          "_to_",
                                          format(max(group_dates), "%Y%m%d"),
                                          ".png"))
                
                ggsave(filename = fname, plot = p, width = 12, height = 8, dpi = 300)
                message("Saved plot to: ", fname)
        }
        
        invisible(plots)  # Return list of plots invisibly
}


