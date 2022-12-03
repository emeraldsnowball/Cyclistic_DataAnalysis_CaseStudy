library(tidyverse)
library(data.table)
library(lubridate)

all_trips_clean <- fread("C:\\Users\\mable\\Documents\\Portfolio Projects\\001. GDA_CAPSTONE_Cyclistic\\Analysis\\all_trips_clean_bu.csv")

# start

all_trips_clean$day_of_week <- ordered(
  all_trips_clean$day_of_week, 
  levels = c(
    "Monday", "Tuesday", "Wednesday", "Thursday",
    "Friday", "Saturday", "Sunday"
  )
)

all_trips_clean$time_of_day <- as.POSIXct(all_trips_clean$time_of_day, format = "%H:%M")

table(all_trips_clean$member_casual)

summary(all_trips_clean$trip_length)


aggregate(trip_length ~ member_casual, all_trips_clean, sum)

all_trips_clean %>%
  group_by(member_casual)%>%
  summarise(min(trip_length), max(trip_length), median(trip_length), mean(trip_length))

#aggregate(all_trips_clean$trip_length ~ all_trips_clean$member_casual + all_trips_clean$day_of_week, FUN = median)

setNames(aggregate(all_trips_clean$trip_length ~ all_trips_clean$member_casual + all_trips_clean$day_of_week, FUN = median), c('trip_length', 'member_casual', 'day_of_week'))

all_trips_clean %>%
  group_by(member_casual, day_of_week)%>%
  summarise(total_rides = n(), avg_trip = mean(trip_length))%>%
  arrange(day_of_week)

str(all_trips_clean)


# visuals 

pie(table(all_trips_clean$member_casual))
pie(aggregate(trip_length ~ member_casual, all_trips_clean, sum)$trip_length, labels = all_trips_clean$member_casual)

#rides per month
all_trips_clean %>%
  group_by(member_casual, month)%>%
  summarise(total_rides = n(), 'avg_duration(mins)' = mean(trip_length))%>%
  arrange(member_casual)%>%
  ggplot(aes(x=month, y = total_rides, fill = member_casual))+
  geom_col(position = "dodge")+
  labs(x = "Month", y = "Total Number of Rides", title = "Rides per Month", fill = "Type of Membership")+
  scale_y_continuous(breaks = c(100000, 200000, 300000, 400000), labels = c("100k", "200k", "300k", "400k"))+
  scale_x_discrete(limits = 1:12, labels = month.abb)


#Average Trip length by Customer Type and Day of Week
all_trips_clean %>%   
  group_by(member_casual, day_of_week) %>% 
  summarise(average_trip_length = mean(trip_length)) %>% 
  ggplot(aes(x=day_of_week, y = average_trip_length, fill = member_casual))+
  geom_col(position = "dodge") + 
  labs (x="Day of Week", y="Average Trip Length(min)", title = "Average Trip Length by Day of Week per customer type", fill = "Type of Membership")


all_trips_clean %>%    #total rides broken down by weekday
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n() ) %>% 
  arrange(member_casual, day_of_week) %>% 
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = member_casual)) + 
  geom_col(position = "dodge") + 
  labs(x= 'Day of Week', y='Total Number of Rides', title='Rides per Day of Week', fill = 'Type of Membership') +
  scale_y_continuous(breaks = c(250000, 400000, 550000), labels = c("250K", "400K", "550K"))

#Looking at number of trips over a 24 hour day
all_trips_clean%>%
  group_by(member_casual, time_of_day)%>%
  summarise(total_trips = n())%>%
  ggplot(aes(x = time_of_day, y = total_trips, color = member_casual, group = member_casual))+
  geom_line()+
  scale_x_datetime(date_breaks = "1 hour", date_labels = "%H:%M", expand = c(0,0))+
  labs(title ="Total Trips Throughout the Day", x = "Time", y = "Total Trips")




