library(tidyverse)
library(data.table)
library(lubridate)

#Load data sets

oct21 <- read.csv("C:\\Users\\mable\\Documents\\Portfolio Projects\\001. GDA_CAPSTONE_Cyclistic\\raw_data\\202110-divvy-tripdata.csv")
nov21 <- read.csv("C:\\Users\\mable\\Documents\\Portfolio Projects\\001. GDA_CAPSTONE_Cyclistic\\raw_data\\202111-divvy-tripdata.csv")
dec21 <- read.csv("C:\\Users\\mable\\Documents\\Portfolio Projects\\001. GDA_CAPSTONE_Cyclistic\\raw_data\\202112-divvy-tripdata.csv")
jan22 <- read.csv("C:\\Users\\mable\\Documents\\Portfolio Projects\\001. GDA_CAPSTONE_Cyclistic\\raw_data\\202201-divvy-tripdata.csv")
feb22 <- read.csv("C:\\Users\\mable\\Documents\\Portfolio Projects\\001. GDA_CAPSTONE_Cyclistic\\raw_data\\202202-divvy-tripdata.csv")
mar22 <- read.csv("C:\\Users\\mable\\Documents\\Portfolio Projects\\001. GDA_CAPSTONE_Cyclistic\\raw_data\\202203-divvy-tripdata.csv")
apr22 <- read.csv("C:\\Users\\mable\\Documents\\Portfolio Projects\\001. GDA_CAPSTONE_Cyclistic\\raw_data\\202204-divvy-tripdata.csv")
may22 <- read.csv("C:\\Users\\mable\\Documents\\Portfolio Projects\\001. GDA_CAPSTONE_Cyclistic\\raw_data\\202205-divvy-tripdata.csv")
jun22 <- read.csv("C:\\Users\\mable\\Documents\\Portfolio Projects\\001. GDA_CAPSTONE_Cyclistic\\raw_data\\202206-divvy-tripdata.csv")
jul22 <- read.csv("C:\\Users\\mable\\Documents\\Portfolio Projects\\001. GDA_CAPSTONE_Cyclistic\\raw_data\\202207-divvy-tripdata.csv")
aug22 <- read.csv("C:\\Users\\mable\\Documents\\Portfolio Projects\\001. GDA_CAPSTONE_Cyclistic\\raw_data\\202208-divvy-tripdata.csv")
sep22 <- read.csv("C:\\Users\\mable\\Documents\\Portfolio Projects\\001. GDA_CAPSTONE_Cyclistic\\raw_data\\202209-divvy-publictripdata.csv")

# checking if structure of all data set is similar for merge 
str(oct21)
str(nov21)
str(dec21)
str(jan22)
str(feb22)
str(mar22)
str(apr22)
str(may22)
str(jun22)
str(jul22)
str(aug22)
str(sep22)

# merging all data sets 

all_trips <- bind_rows(oct21, nov21, dec21, jan22, feb22, mar22, apr22, may22, jun22, jul22,aug22, sep22)
str(all_trips)
# start and end time were in chr string type, we change it to date type here 
all_trips$started_at <- ymd_hms(all_trips$started_at)
all_trips$ended_at <- ymd_hms(all_trips$ended_at)

str(all_trips)

# add new column that contains trip duration in secs 

all_trips$trip_length <- (as.double(as.character(difftime(all_trips$ended_at, all_trips$started_at, units = "secs"))))/60

str(all_trips)

# additional columns to help with grouping during analysis

all_trips$date <- as.Date(all_trips$started_at)
all_trips$month <- format(all_trips$started_at,"%m")
all_trips$day <- format(all_trips$started_at,"%d")
all_trips$day_of_week <- format(all_trips$started_at,"%A")
all_trips$time_of_day <- format(all_trips$started_at,"%H:%M:%S")

str(all_trips)

# cleaning
# remove all NA rows
all_trips_clean <- drop_na(all_trips)
# remove rows with blank station names
all_trips_clean <- all_trips_clean %>%
  filter(!(is.na(start_station_name) | start_station_name == ""))%>%
  filter(!(is.na(end_station_name) | end_station_name == ""))

# remove duplicates
all_trips_clean <- distinct(all_trips_clean)
# remove negative trip length and trip lengths that are more than 1 day 
all_trips_clean <- all_trips_clean %>%
  filter(!(trip_length < 1))%>%
  filter(!(trip_length>1440))

str(all_trips_clean)

# Arrange weekdays in order 
all_trips_clean$day_of_week <- ordered(
  all_trips_clean$day_of_week, 
  levels = c(
    "Monday", "Tuesday", "Wednesday", "Thursday",
    "Friday", "Saturday", "Sunday"
  )
)

all_trips_clean$time_of_day <- as.POSIXct(all_trips_clean$time_of_day, format = "%H:%M")

fwrite(all_trips_clean, 
       "C:\\Users\\mable\\Documents\\Portfolio Projects\\001. GDA_CAPSTONE_Cyclistic\\Analysis\\all_trips_clean.csv", 
       col.names = TRUE, row.names = FALSE)


