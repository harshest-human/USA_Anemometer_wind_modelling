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

################# Windmast Import ########################
windmast_input = read.csv("windmast_06_10.txt", header=FALSE) 
rownames(windmast_input) = 1:nrow(windmast_input)
names(windmast_input) = c("Ultrasonic", "U", "V", "col4", "col5", "Temperature", "col7", "col8", "Count", "DateTime")
windmast_input = na.omit(windmast_input) 
windmast_input$DateTime <- dmy_hms(windmast_input$DateTime)
windmast_input$DateTime_WI3min = round_date(windmast_input$DateTime, "3 minutes")

windmast_input <- windmast_input %>% 
        filter(DateTime_WI3min >= ymd_hms("2021-09-02 11:42:00"),
               DateTime_WI3min <= ymd_hms("2021-10-06 11:21:00"))

windmast_input_3min<- windmast_input %>% 
        group_by(DateTime_WI3min) %>% 
        summarise(U = mean(U), V = mean(V))

##################### WD/WS ##############################
UV2WDWS_function <- source("UV2WDWS_function.R")

if (typeof(windmast_input_3min$U) !="double"){
        windmast_input_3min$U=as.numeric(windmast_input_3min$U) }

if (typeof(windmast_input_3min$V) !="double"){
        windmast_input_3min$V=as.numeric(windmast_input_3min$V) }

WD_WS_table = data.frame(uv2wdws(windmast_input_3min$U, windmast_input_3min$V))


###################### WIND GRAPH #########################
windRose(WD_WS_table, ws = "ws", wd = "wd",
         breaks = c(0,1,2,4,6,12),
         auto.text = FALSE,
         paddle = FALSE,
         grid.line = 5,
         key = list(lables = c(">0 - 1",
                               ">1 - 2",
                               ">2 - 4",
                               ">4 - 6",
                               ">6 - 12")),
         key.header = "02.09.2021 - 06.10.2021",
         key.footer = "Wind_speed (m/s)",
         key.position = "bottom",
         par.settings=list(axis.line=list(col="lightgray")),
         col = c("#4f4f4f", "#0a7cb9", "#f9be00", "#ff7f2f", "#d7153a"))


###################### Wind_WD_WS DATA #####################################
wind_WD_WS_data <- cbind.data.frame(windmast_input_3min, WD_WS_table) %>% select(DateTime_WI3min, wd, ws) 
names(wind_WD_WS_data) = c("DateTime_WI3min", "wind_direction", "wind_speed")
wind_WD_WS_data$DateTime_WI3min <- as.factor(wind_WD_WS_data$DateTime_WI3min)

url <- 'http://snowfence.umn.edu/Components/winddirectionanddegreeswithouttable3.htm'
page <- read_html(url)
directions_raw <- page %>% html_node('td table') %>% html_table(header = TRUE)
directions <- directions_raw %>% 
        set_names(~tolower(sub(' Direction', '', .x))) %>% 
        slice(-1) %>% 
        separate(degree, c('degree_min', 'degree_max'), sep = '\\s+-\\s+', convert = TRUE)

wind_WD_WS_data <- wind_WD_WS_data  %>% mutate(wd_cardinal = cut(wind_direction, 
                                                             breaks = c(0, directions$degree_max, 360), 
                                                             labels = c(directions$cardinal, 'N')))

write.table(wind_WD_WS_data, "wind_WD_WS_data.txt")


########################## DWD DATA ########################################
DWD_input <- read.csv("DWD_20200509_20211109_01803.txt", header=TRUE, sep=";")
DWD_input <- select(DWD_input, MESS_DATUM, DD_10, FF_10)
DWD_input$MESS_DATUM <- ymd_hm(DWD_input$MESS_DATUM)
DWD_input$MESS_DATUM <- as.character(DWD_input$MESS_DATUM)
DWD_input <- filter(DWD_input, MESS_DATUM >= "2021-10-06 11:20:00", MESS_DATUM <= "2021-11-06 11:21:00")
DWD_input$MESS_DATUM <- ymd_hms(DWD_input$MESS_DATUM)


###################### DWD GRAPH ##########################################
windRose(DWD_input, ws = "FF_10", wd = "DD_10",
         breaks = c(0,1,2,4,6,12),
         auto.text = FALSE,
         paddle = FALSE,
         grid.line = 5,
         key = list(lables = c(">0 - 1",
                               ">1 - 2",
                               ">2 - 4",
                               ">4 - 6",
                               ">6 - 12")),
         key.header = "06.10.2021 - 06.11.2021",
         key.footer = "Wind_speed (m/s)",
         key.position = "bottom",
         par.settings=list(axis.line=list(col="lightgray")),
         col = c("#4f4f4f", "#0a7cb9", "#f9be00", "#ff7f2f", "#d7153a"))


########################## DWD Interpolation ##############################
DWD_interpolated <-DWD_input %>%
        full_join(data.frame(MESS_DATUM = seq(
                from = min(.$MESS_DATUM),
                to = max(.$MESS_DATUM),
                by = 'min'))) %>%
        arrange(MESS_DATUM) %>%
        mutate(DD_10 = approx(DD_10, n = n())$y, FF_10 = approx(FF_10, n = n())$y )

DWD_interpolated$MESS_DATUM <- ymd_hms(DWD_interpolated$MESS_DATUM)
DWD_interpolated$MESS_DATUM = round_date(DWD_interpolated$MESS_DATUM, "3 minutes")
DWD_interpolated<- DWD_interpolated %>% 
        group_by(MESS_DATUM) %>% 
        summarise(wind_direction = mean(DD_10), wind_speed = mean(FF_10))
DWD_interpolated$MESS_DATUM <-as.factor(DWD_interpolated$MESS_DATUM)

url <- 'http://snowfence.umn.edu/Components/winddirectionanddegreeswithouttable3.htm'
page <- read_html(url)
directions_raw <- page %>% html_node('td table') %>% html_table(header = TRUE)
directions <- directions_raw %>% 
        set_names(~tolower(sub(' Direction', '', .x))) %>% 
        slice(-1) %>% 
        separate(degree, c('degree_min', 'degree_max'), sep = '\\s+-\\s+', convert = TRUE)

DWD_interpolated <- DWD_interpolated  %>% mutate(wd_cardinal = cut(wind_direction, 
                                                     breaks = c(0, directions$degree_max, 360), 
                                                     labels = c(directions$cardinal, 'N')))

write.table(DWD_interpolated, "DWD_interpolated.txt")


