# import libraries
library(ggplot2) #visualization matplotlib in case of python
library(ggthemes) #add-on with ggplot
library(dplyr) #data manipulation pandas in case of pandas
library(lubridate) #date time
library(scales) #graphical scalling
library(tidyr) #tidy data
library(DT)#take formatted results

######################################################
           # Exploratory Data #
######################################################

# reading the data # 6 months of data
apr.data <- read.csv("uber-raw-data-apr14.csv")
may.data <- read.csv("uber-raw-data-may14.csv")
jun.data <- read.csv("uber-raw-data-jun14.csv")
jul.data <- read.csv("uber-raw-data-jul14.csv")
aug.data <- read.csv("uber-raw-data-aug14.csv")
sep.data <- read.csv("uber-raw-data-sep14.csv")

# combine all the data
data.2014 <- rbind(apr.data, may.data, jun.data, jul.data, aug.data, sep.data)

# visualize the data
head(data.2014)

# structure
str(data.2014)

# summary
summary(data.2014)


# start analysis
data.2014$Date.Time <- as.POSIXct(data.2014$Date.Time, format = "%m/%d/%Y %H:%M:%S")

#summary statistics
summary(data.2014)

# extracting time from date time
data.2014$Time <- format(as.POSIXct(data.2014$Date.Time, format = "%m/%d%/Y %H:%M:%S"), format = "%H:%M:%S")


data.2014$Date.Time <-ymd_hms(data.2014$Date.Time) # formatting
data.2014$day <- format(day(data.2014$Date.Time)) # day
data.2014$month <- format(month(data.2014$Date.Time, label = TRUE)) # month
data.2014$year <- format(year(data.2014$Date.Time)) # year
data.2014$day.of.week <- format(wday(data.2014$Date.Time, label = TRUE)) # day of week

# hour minute second

data.2014$hour <- factor(hour(hms(data.2014$Time))) # we want these as factors
data.2014$minute <- factor(minute(hms(data.2014$Time)))
data.2014$second <- factor(second(hms(data.2014$Time)))


head(data.2014)

# visualization

# plot trips by hours in a day

hour.data <- data.2014 %>%
  group_by(hour) %>%
  summarise(Total = n())   #grouping the data wrt hour and count

# tabular
datatable(hour.data)


# visualize the data 
ggplot(hour.data, aes(x=hour, y=Total)) +
  geom_bar(stat = "identity", fill = "black", color = "blue") +
  ggtitle("Trips by Hour") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma)


# FINDING 1: most operations happen during 3pm to 9 pm (15 to 21 hour marks)


month_hour_data <- data.2014 %>%
  group_by(month, hour) %>%
  summarize(Total = n())

# tabular
datatable(month_hour_data)


# plot
ggplot(month_hour_data, aes(hour, Total, fill = month)) +
  geom_bar(stat = "identity") +
  ggtitle("Uber Trips by Month & Hour") +
  theme(legend.position = "none") +
  scale_y_continuous(label = comma)

# FINDING 2: september has the most rides

sept.hour <- data.2014 %>%
  group_by(hour,month) %>%
  filter(month == "Sep") %>%
  summarise(Total = n())

ggplot(sept.hour, aes(hour, Total, fill = hour)) +
  geom_bar(stat = "identity") +
  ggtitle("Uber Trips by Month & Hour for September") +
  theme(legend.position = "none") +
  scale_y_continuous(label = comma)
#around 6pm was 70000 in september


apr.hour <- data.2014 %>%
  group_by(hour,month) %>%
  filter(month == "Apr") %>%
  summarise(Total = n())

ggplot(apr.hour, aes(hour, Total, fill = hour)) +
  geom_bar(stat = "identity") +
  ggtitle("Uber Trips by Month & Hour for April") +
  theme(legend.position = "none") +
  scale_y_continuous(label = comma)
# around 50000 rides on 5pm in april
# if you know about time series analysis, you will spot a trend here



# plot the data grouped by day

day.data <- data.2014 %>%
  group_by(day) %>%
  summarise(Total = n())   #grouping the data wrt day and count

# tabular
datatable(day.data)

# visualization
ggplot(day.data, aes(day, Total)) +
  geom_bar(stat = "identity", fill = "hotpink", color = "black") +
  ggtitle("Trips by Day") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma)

#mostly uniform distribution but remember all months don't have 31 days

#month and day grouping
month_day_data <- data.2014 %>%
  group_by(month,day) %>%
  summarise(Total = n())

datatable(month_day_data)

# plot

ggplot(month_day_data, aes(day, Total, fill = month)) +
  geom_bar(stat = "identity") +
  ggtitle("Trips by Day and Month") +
  scale_y_continuous(labels = comma)

# FINDING 3: data worth of 183 days

# september data

sept.day <- data.2014 %>%
  group_by(day,month) %>%
  filter(month =="Sep")%>%
  summarise(Total = n())

ggplot(sept.day, aes(day, Total, fill=day)) +
  geom_bar(stat = "identity") +
  ggtitle("Trips by Day and Month for September") +
  scale_y_continuous(labels = comma)
#13 has highest number of rides

#monthly trend
month.data <- data.2014 %>%
  group_by(month) %>%
  summarise(Total = n()) 

datatable(month.data)

#august and september has highest number of rides out of this 6 month period. why does this happen? is it because of their marketing strategies or something else

ggplot(month.data, aes(month, Total, fill=month)) +
  geom_bar(stat = "identity") +
  ggtitle("Trips by Month") +
  scale_y_continuous(labels = comma)
#no trend seen from this graph


# month weekday
month_weekday_data <- data.2014 %>%
  group_by(month, day.of.week) %>%
  summarise(Total = n())

datatable(month_weekday_data)

ggplot(month_weekday_data, aes(month, Total, fill = day.of.week)) +
  geom_bar(stat = "identity") +
  ggtitle("Trips by Month and Weekday") +
  scale_y_continuous(labels = comma)


# analyzing bases

ggplot(data.2014, aes(Base)) +
  geom_bar(fill = "hotpink") +
  scale_y_continuous(labels = comma) +
  ggtitle("Trips by Bases")

#B02512 and B02764 are not very profitable for the company

#trip based on bases and months
ggplot(data.2014, aes(Base, fill = month)) +
  geom_bar(position = "dodge") +
  scale_y_continuous(labels = comma) +
  ggtitle("Trips by Bases and Month") 
#any events taking place in sept for bo2764 and events in apr and may in bo2617 should be observed


#trips based on bases with days of week
ggplot(data.2014, aes(Base, fill = day.of.week)) +
  geom_bar(position = "dodge") +
  scale_y_continuous(labels = comma) +
  ggtitle("Trips by Bases and Days of Week")


# creating heat map
day_and_hour <- data.2014 %>%
  group_by(day, hour) %>%
  dplyr::summarize(Total = n())

datatable(day_and_hour)

ggplot(day_and_hour, aes(day, hour, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map of Trips by Day and Hour")

# you can see hours 15 through 21 are the most profitable hours for uber

# crating a geo distribution

min.lat <- 40.5574
max.lat <- 40.9176
min.long <- -74.15
max.long <- -73.7004 #found in summary stats
ggplot(data.2014, aes(x=Lon, y=Lat, color = Base)) +
  geom_point(size = 1) +
  scale_x_continuous(limits=c(min.long, max.long)) +
  scale_y_continuous(limits=c(min.lat, max.lat)) +
  theme_map() +
  ggtitle("NYC (lat-long chart) MAP BASED ON UBER Ribes 2014 (April - September) By Base")

# rm(list = ls()) #clears environment
