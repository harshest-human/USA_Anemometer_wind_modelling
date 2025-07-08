######## Development of function ########
winduv <- function(folder_path, ID, minute_avg = 1, start_time = NULL, end_time = NULL, output_file = "averaged_wind_data.csv") {
        library(dplyr)
        library(lubridate)
        library(stringr)
        
        files <- list.files(folder_path, pattern = "\\.txt$", full.names = TRUE)
        if (length(files) == 0) {
                message("No .txt files found in folder.")
                return(NULL)
        }
        
        full_data <- data.frame()
        
        for (file in files) {
                lines <- readLines(file, warn = FALSE)
                
                #### Gill-style (starts with H,)
                gill_lines <- grep("^H,", lines, value = TRUE)
                if (length(gill_lines) > 0) {
                        parsed <- str_split_fixed(gill_lines, ",", 9)
                        df <- data.frame(
                                U = as.numeric(parsed[, 2]),
                                V = as.numeric(parsed[, 3]),
                                W = as.numeric(parsed[, 4]),
                                DATE.TIME = suppressWarnings(dmy_hms(parsed[, 9])),
                                stringsAsFactors = FALSE
                        )
                } else {
                        #### USA tab-separated fallback
                        usa_lines <- lines[nchar(lines) > 20]
                        parsed <- str_split_fixed(usa_lines, "\t", 10)
                        
                        # Replace decimal commas in U, V, W
                        u_vals <- as.numeric(str_replace_all(parsed[, 7], ",", "."))
                        v_vals <- as.numeric(str_replace_all(parsed[, 8], ",", "."))
                        w_vals <- as.numeric(str_replace_all(parsed[, 9], ",", "."))
                        
                        date_time_str <- paste(parsed[, 2], str_replace(parsed[, 3], ",", "."))
                        date_times <- suppressWarnings(dmy_hms(date_time_str))
                        
                        df <- data.frame(
                                U = u_vals,
                                V = v_vals,
                                W = w_vals,
                                DATE.TIME = date_times,
                                stringsAsFactors = FALSE
                        )
                }
                
                df <- df %>%
                        filter(!is.na(DATE.TIME)) %>%
                        mutate(
                                ID = ID,
                                wd_speed = sqrt(U^2 + V^2 + W^2),
                                wd_direction = (atan2(V, U) * 180 / pi + 360) %% 360
                        )
                
                full_data <- bind_rows(full_data, df)
        }
        
        if (nrow(full_data) == 0) {
                message("No valid data found across all files.")
                return(NULL)
        }
        
        if (!is.null(start_time)) {
                start_time <- ymd_hms(start_time)
                end_time   <- ymd_hms(end_time)
                full_data <- full_data %>%
                        filter(DATE.TIME >= start_time & DATE.TIME <= end_time)
        }
        
        full_data <- full_data %>%
                mutate(minute_group = floor_date(DATE.TIME, unit = paste(minute_avg, "min")))
        
        averaged <- full_data %>%
                group_by(ID, minute_group) %>%
                summarise(
                        wd_speed = mean(wd_speed, na.rm = TRUE),
                        wd_direction = mean(wd_direction, na.rm = TRUE),
                        .groups = "drop"
                ) %>%
                rename(DATE.TIME = minute_group)
        
        write.csv(averaged, output_file, row.names = FALSE)
        message("Output written to: ", output_file)
        return(averaged)
}



########## Example #########
#windmast_input <- winduv(
        #folder_path = "2025_USA_16_MAST_raw",
        #ID = "USA.Mst",
        #minute_avg = 5,
        #start_time = "2025-04-08 00:00:00",
        #end_time   = "2025-04-14 23:59:59",
        #output_file = "2025_04_08_USA_Mst_5_min_avg.csv")

