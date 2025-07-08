winduv <- function(folder_path, ID, minute_avg = 1,
                   start_time = NULL, end_time = NULL,
                   output_file = "averaged_wind_data.csv") {
        # Load libraries
        library(dplyr)
        library(lubridate)
        library(stringr)
        
        # List all .txt files
        files <- list.files(folder_path, pattern = "\\.txt$", full.names = TRUE)
        
        # Initialize empty dataframe
        full_data <- data.frame()
        
        for (file in files) {
                cat("Processing file:", file, "\n")
                
                # Read all lines
                lines <- readLines(file, warn = FALSE)
                
                # Keep only lines starting with 'H,'
                data_lines <- grep("^H,", lines, value = TRUE)
                if (length(data_lines) == 0) next
                
                # Parse lines
                parsed <- str_split_fixed(data_lines, ",", 9)
                
                # Convert and clean
                df <- data.frame(
                        U = as.numeric(parsed[, 2]),
                        V = as.numeric(parsed[, 3]),
                        W = as.numeric(parsed[, 4]),
                        DATE.TIME = dmy_hms(parsed[, 9]),
                        stringsAsFactors = FALSE
                )
                
                df$ID <- ID
                
                # Compute wind speed and direction
                df <- df %>%
                        mutate(
                                wd_speed = sqrt(U^2 + V^2 + W^2),
                                wd_direction = (atan2(V, U) * 180 / pi + 360) %% 360
                        )
                
                full_data <- bind_rows(full_data, df)
        }
        
        if (nrow(full_data) == 0) {
                message("No valid data found.")
                return(NULL)
        }
        
        # Filter by start_time and end_time if provided
        if (!is.null(start_time)) {
                start_time <- ymd_hms(start_time)
                full_data <- filter(full_data, DATE.TIME >= start_time)
        }
        if (!is.null(end_time)) {
                end_time <- ymd_hms(end_time)
                full_data <- filter(full_data, DATE.TIME <= end_time)
        }
        
        # Round to minute intervals
        full_data <- full_data %>%
                mutate(minute_group = floor_date(DATE.TIME, unit = paste(minute_avg, "min")))
        
        # Average over defined minute intervals
        averaged <- full_data %>%
                group_by(ID, minute_group) %>%
                summarise(
                        wd_speed = mean(wd_speed, na.rm = TRUE),
                        wd_direction = mean(wd_direction, na.rm = TRUE),
                        .groups = "drop"
                ) %>%
                rename(DATE.TIME = minute_group)
        
        # Write to CSV
        write.csv(averaged, file = output_file, row.names = FALSE)
        message("Output written to: ", output_file)
        
        return(averaged)
}


########## Example #########
windmast_input <- winduv(
        folder_path = "2025_USA_Traverse_raw",
        ID = "USA.Tr",
        minute_avg = 5,
        start_time = "2025-04-08 00:00:00",
        end_time   = "2025-04-14 23:59:59",
        output_file = "2025_04_08_USA_5_min_avg.csv"
)

