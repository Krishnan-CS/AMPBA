# DSUR Assignment

rm(list = ls())

# Libraries
library(nycflights13)
library(ggplot2)
library(dplyr)

help(flights)
data(package = "nycflights13")

# Load data
data("flights")
data("airlines")

# First check dep_delay column
summary(flights$dep_delay)
ggplot(flights, aes(x = dep_delay)) +
  geom_boxplot(na.rm = TRUE)

# Problem 1 - Departure delay based on airlines

# Check carrier for null values before analysis
table(flights$carrier, useNA = 'ifany')
flights$carrier <- factor(flights$carrier)

# Create table for mean and median delay grouped by carrier
# Join it with airlines
carrier.Delay <- flights %>%
  group_by(carrier) %>%
  summarise(Mean_Delay = mean(dep_delay, na.rm = TRUE), 
            Median_Delay = median(dep_delay, , na.rm = TRUE))

carrier.Delay <- airlines %>% inner_join(carrier.Delay)

# Problem 2 - Departure delay based on airport

# Check origin for null values before analysis
table(flights$origin, useNA = 'ifany')
flights$origin <- factor(flights$origin)

# Create table with mean delay and number of flights from each airport
airport.Delay <- flights %>%
  group_by(origin) %>%
  summarise(Num_Flights = n(), 
            Mean_Delay = mean(dep_delay, na.rm = TRUE))

# Create a box plot without outliers
ggplot(flights, aes(x = origin, y = dep_delay)) +
  geom_boxplot(na.rm = TRUE, outliers = FALSE) +
  xlab('Origin Airport') + ylab('Departure Delay') +
  ggtitle('Box plot of departure delay for each airport')

# Create a new dataset for data processing. 
# Create a new column with log transformation of the dep_delay
# Set all zeros and negative values to 1, to avoid -inf / NaNs 
flights_expanded <- flights %>%
  mutate(Log_Dep_Delay = ifelse(dep_delay < 1,
                                1, log(dep_delay)))
summary(flights_expanded$Log_Dep_Delay)

ggplot(flights_expanded, aes(x = origin, y = Log_Dep_Delay)) +
  geom_boxplot(na.rm = TRUE) +
  xlab('Origin Airport') + ylab('Log(Departure Delay)') + 
  ggtitle('Departure delay by airport') +
  geom_hline(aes( yintercept = log(5), colour = '5 min delay' )) + 
  geom_hline(aes( yintercept = log(10), colour = '10 min delay' )) +
  geom_hline(aes( yintercept = log(20), colour = '20 min delay' ))

# Problem 3

# Check month for null values before analysis
summary(flights$month)

# Create a month-wise mean delay table
month.Delay <- flights %>%
  group_by(month) %>%
  summarise(Mean_Delay = mean(dep_delay, na.rm = TRUE))

# Create a line plot for months 1 to 12
ggplot(month.Delay, aes(x = month, y=Mean_Delay)) +
  geom_line() +
  ggtitle('Mean Departure Delay by Month') +
  scale_x_continuous(breaks = 1:12, labels = 1:12)

# Create a table with mean delays grouped by month and date
month_Day.Delay <- flights %>%
  group_by(month, day) %>%
  summarise(Mean_Delay = mean(dep_delay, na.rm = TRUE))

# Plot the data by day and month
# This turned out to be too messy to read
ggplot(month_Day.Delay, aes(x = day, y = Mean_Delay, color = factor(month))) +
  geom_line() +
  labs(title = "Flight Delays by Day and Month",
       x = "Day",
       y = "Delay (minutes)",
       color = "Month")

# Plot the trend for June, July and December
# Those months had a higher mean delay.
ggplot(data = filter(month_Day.Delay, month == c(6,7,12) ), aes(x = day, y = Mean_Delay, color = factor(month))) +
  geom_line() +
  labs(title = "Flight Delays by Day and Month",
       x = "Day",
       y = "Delay (minutes)",
       color = "Month")

# Analyze weather and holiday patterns
data("weather")

flights_expanded <- flights_expanded %>%
  left_join(weather, by=c('year','month','day','hour','origin') )

# Study correlation between departure delay and a few weather parameters
corr_cols <- flights_expanded %>%
  select(dep_delay, temp, precip, visib, wind_speed, wind_gust, pressure)
corr_delay_weather <- cor(corr_cols, use = 'complete.obs')

# Problem 4

# Check hour for null values before analysis
summary(flights$hour)

# Create a table with hour-wise mean departure delay
hour.delay <- flights %>%
  group_by(hour) %>%
  summarise(numFlights = n(), Mean_Dep_Delay = mean(dep_delay, na.rm = TRUE))

# Create a line plot from the above data
ggplot(hour.delay, aes(x = hour, y = Mean_Dep_Delay)) +
  geom_line(na.rm = TRUE) +
  ggtitle('Mean departure delay by hour') +
  scale_x_continuous(breaks = 0:23, labels = 0:23) +
  xlab('Departure Hour') + ylab('Mean Departure Delay')

# Problem 5

# Check the distance column before analysis
summary(flights$distance)

# Calculate the correlation between distance and departure delay 
corr_dist_delay <- flights %>%
  select(distance, dep_delay) %>%
  summarise(Dist_Delay_Correlation = cor(distance, dep_delay, use = 'complete.obs'))

# Create the scatter plot between distance and departure delay
ggplot(flights, aes(x = distance, y = dep_delay)) +
  geom_point(na.rm = TRUE) +
  xlab('Distance') + ylab('Departure Delay') +
  ggtitle('Scatter plot of distance vs departure delay')

# Calculate the correlation between distance and arrival delay 
corr_dist_arr_delay <- flights %>%
  select(distance, arr_delay) %>%
  summarise(Dist_Delay_Correlation = cor(distance, arr_delay, use = 'complete.obs'))

# Create the scatter plot between distance and arrival delay
ggplot(flights, aes(x = distance, y = arr_delay)) +
  geom_point(na.rm = TRUE) +
  xlab('Distance') + ylab('Arrival Delay') +
  ggtitle('Scatter plot of distance vs arrival delay')

# Problem 6

# Check the air time column before analysis
summary(flights$air_time)

# Create a table for effective travel time
time_dist.byairline <- flights %>%
  filter(!is.na(air_time)) %>%
  group_by(carrier) %>%
  summarise(Avg_Air_Time = mean(air_time),
            Avg_Distance = mean(distance),
            Eff_Travel_Time=sum(air_time) / sum(distance))

time_dist.byairline <- airlines %>% inner_join(time_dist.byairline)

# Problem 7

# Check arrival delay
summary(flights$arr_delay)

# Assuming arrival delay of 10 mins or less, is tolerable
# How often do airlines maintain this standard?
carrier.arr_delay <- flights %>%
  group_by(carrier) %>%
  summarise(num_flights_daily = n() / 365, 
            num_destinations = n_distinct(dest),
            Mean_Arr_Delay = mean(arr_delay, na.rm = TRUE),
            Percentage_OnTime_Arr = (sum(arr_delay <= 10, na.rm = TRUE) / n()) * 100 ) %>%
  inner_join(airlines) %>%
  select(carrier, name, num_flights_daily, num_destinations, Mean_Arr_Delay, Percentage_OnTime_Arr) %>%
  arrange(Mean_Arr_Delay)

# Problem 8

# Data for evenings 17 to 21 hours
flights_evening <- filter(flights, hour %in% c(17:21))

month.evening_delay <- flights_evening %>%
  group_by(month) %>%
  summarise(Mean_Evening_Delay = mean(dep_delay, na.rm = TRUE))

month.day_evening_delay <- flights_evening %>%
  group_by(month, day) %>%
  summarise(Num_flights = n(),
            Mean_Evening_Delay = mean(dep_delay, na.rm = TRUE))

# Check the correlation between number of flights and delay
ggplot(month.day_evening_delay, aes(x = Num_flights, y = Mean_Evening_Delay)) +
  geom_point()
cor(month.day_evening_delay$Num_flights, month.day_evening_delay$Mean_Evening_Delay, use = 'complete.obs')

# Mean evening delay is over 40 for June and July!
JuneJuly.evening_delay <- filter(flights_evening, month %in% c(6,7) )

JuneJuly.daily.delay <- JuneJuly.evening_delay %>%
  group_by(month, day) %>%
  summarise(Num_flights = n(), 
            Mean_Evening_Delay = mean(dep_delay, na.rm = TRUE))

# Draw a line plot for daily evening delays
ggplot(JuneJuly.daily.delay, aes(x = day, y = Mean_Evening_Delay, colour = factor(month))) +
  geom_line() + geom_point(size = 2) +
  scale_x_continuous(labels = 0:31, breaks = 0:31) +
  ggtitle('Mean evening delay for June and July')
  

# Check the correlation between number of flights and delay
ggplot(JuneJuly.daily.delay, aes(x = Num_flights, y = Mean_Evening_Delay)) +
  geom_point()
cor(JuneJuly.daily.delay$Num_flights, JuneJuly.daily.delay$Mean_Evening_Delay)

# Data for carriers with higher delays
airlines.higher_Dep_Delay <- flights %>%
  group_by(carrier) %>%
  summarise(Mean_Dep_Delay = mean(dep_delay, na.rm = TRUE)) %>%
  arrange(-Mean_Dep_Delay) %>%
  top_n(6) %>% pull(carrier)

flights_highDelay <- filter(flights, carrier %in% airlines.higher_Dep_Delay)

# Check hourly flights delays for this group
hour.delay_filteredList <- flights_highDelay %>%
  group_by(hour, carrier) %>%
  summarise(Num_flights = n(), 
            Mean_Dep_Delay = mean(dep_delay, na.rm = TRUE))

# y scale limits were set for better visualization.
# Some values get removed from the plot because of these limits
ggplot(hour.delay_filteredList, aes(x = hour, y = Mean_Dep_Delay, colour = carrier)) +
  geom_line() + geom_point(size=2) +
  scale_y_continuous(limits = c(0,50)) +
  ggtitle('Mean hourly departure delay for carriers with higher delays')

