
 
################################     DOWNLOADING PACKAGES   #########################

#install.packages("ggthemes")
#install.packages("lubridate")
#install.packages("tidyr")
#install.packages("DT")
#install.packages("scales")
#install.packages("forcats")
#install.packages("animation")
#install.packages("hexbin")

##########################      LOADING THE PACKAGES       ############################
library(ggplot2)    
library(lubridate)
library(tidyr)            #used to tidy the data
library(DT)
library(scales)           #graphical scales-to map the data to corresct scales
library(ggthemes)       
library(dplyr)
library(forcats)
library(animation)
library(tidyverse)
library(hexbin)


#remove.packages("tidyverse")

##### ############     CREATING A VECTOR OF COLORS FOR PLOTTING FUNCTIONS       ###################



colors = c("#CC1011", "#665555", "#05a399", "#cfcaca", "#f5e840", "#0683c9", "#e075b0")


##########################       READING THE DATA FROM THE FILES      ####################3


apr_data <- read.csv("/Users/vaibhav/Desktop/UBER_DATA/uber-raw-data-apr14.csv")
may_data <- read.csv("/Users/vaibhav/Desktop/UBER_DATA/uber-raw-data-may14.csv")
jun_data <- read.csv("/Users/vaibhav/Desktop/UBER_DATA/uber-raw-data-jun14.csv")
jul_data <- read.csv("/Users/vaibhav/Desktop/UBER_DATA/uber-raw-data-jul14.csv")
aug_data <- read.csv("/Users/vaibhav/Desktop/UBER_DATA/uber-raw-data-aug14.csv")
sep_data <- read.csv("/Users/vaibhav/Desktop/UBER_DATA/uber-raw-data-sep14.csv")  

####################       COMBINING ALL THE FILES TOGETHER     #########################

data_2014 <- rbind(apr_data,may_data, jun_data, jul_data, aug_data, sep_data)

glimpse(data_2014)
table(data_2014)
View(data_2014)

########################       FORMATTING OF DATE TIME    ###############################

data_2014$Date.Time <- as.POSIXct(data_2014$Date.Time, format = "%m/%d/%Y %H:%M:%S")
data_2014$Time <- format(as.POSIXct(data_2014$Date.Time, format = "%m/%d/%Y %H:%M:%S"), format="%H:%M:%S")
data_2014$Date.Time <- ymd_hms(data_2014$Date.Time)


#######################     DECLARING FACTORS    ###################################
data_2014$day <- factor(day(data_2014$Date.Time))
data_2014$month <- factor(month(data_2014$Date.Time, label = TRUE))
data_2014$year <- factor(year(data_2014$Date.Time))
data_2014$dayofweek <- factor(wday(data_2014$Date.Time, label = TRUE))  
data_2014$hour <- factor(hour(hms(data_2014$Time)))
data_2014$minute <- factor(minute(hms(data_2014$Time)))
data_2014$second <- factor(second(hms(data_2014$Time)))



###############################################################################################


data_2014 <- data_2014 %>% mutate(Date.Time = mdy_hms(Date.Time),
                                  Weekday = wday(Date.Time, label=T, abbr=F),
                                  Minute = minute(Date.Time),
                                  Base = fct_recode(Base,
                                                    "Unter" = "B02512" ,
                                                    "Hinter" = "B02598",
                                                    "Weiter" = "B02617",
                                                    "Schmecken" = "B02682",
                                                    "Danach-NY" = "B02764")) %>% as_tibble()



####################      PLOTTING THE NUMBER OF TRIPS EVERY HOUR      ################

hour_data <- data_2014 %>%
  group_by(hour) %>%
  dplyr::summarize(Total = n()) 
datatable(hour_data)


ggplot(hour_data, aes(hour, Total)) + 
  geom_bar( stat = "identity", fill = "steelblue", color = "red") +
  ggtitle("Trips Every Hour") 



####################      PLOTTING THE NUMBER OF TRIPS EVERY HOUR AND MONTH   ################


month_hour <- data_2014 %>%
  group_by(month, hour) %>%
  dplyr::summarize(Total = n())



ggplot(month_hour, aes(hour, Total, fill = month)) + 
  geom_bar( stat = "identity") +
  ggtitle("Trips by Hour and Month") 


####################      PLOTTING DATA BY TRIPS EVERY HOUR      ####################


ggplot(hour_data, aes(hour, Total)) + 
  geom_bar( stat = "identity", fill = "steelblue", color = "red") +
  ggtitle("Trips Every Hour") +
  scale_y_continuous(labels = comma)


####################      PLOTTING DATA BY TRIPS EVERY DAY      ####################



day_group <- data_2014 %>%
  group_by(day) %>%
  dplyr::summarize(Total = n()) 
datatable(day_group)

ggplot(day_group, aes(day, Total)) + 
  geom_bar( stat = "identity", fill = "steelblue") +
  ggtitle("Trips Every Day") 




####################      PLOTTING DATA BY TRIPS EVERY MONTH      ####################

month_group <- data_2014 %>%
  group_by(month) %>%
  dplyr::summarize(Total = n()) 
datatable(month_group)

ggplot( month_group, aes(month, Total, fill = month)) + 
  geom_bar( stat = "identity") +
  ggtitle("Trips by Month") +
  scale_fill_manual(values = colors)


####################      PLOTTING THE NUMBER OF TRIPS EVERY DAY AND MONTH   ################



month_weekday <- data_2014 %>%
  group_by(month, dayofweek) %>%
  dplyr::summarize(Total = n())


ggplot(month_weekday, aes(month, Total, fill = dayofweek)) + 
  geom_bar( stat = "identity", position = "dodge") +
  ggtitle("Trips by Day and Month") +
  scale_fill_manual(values = colors)


####################      PLOTTING  OF TRIPS BY BASE  ################


ggplot(data_2014, aes(Base)) + 
  geom_bar(fill = "darkred") +
  ggtitle("Trips by Bases")




####################      PLOTTING  OF TRIPS BY BASES AND MONTH  ################

ggplot(data_2014, aes(Base, fill = month)) + 
  geom_bar(position = "dodge") +
  ggtitle("Trips by Bases and Month") +
  scale_fill_manual(values = colors)






#####################     HEAT MAP FOR MONTH AND DAY  ###############################


ggplot(month_weekday, aes(dayofweek, month, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map by Month and Day of Week")



####################     HEAT MAP FOR MONTH TRIPS  ################################

ggplot( month_group, aes(month, Total, fill = month)) + 
  geom_tile( stat = "identity") +
  ggtitle("Trips by Month") +
  scale_fill_manual(values = colors)





##############      HEX MAP OF GEOGRAPHICAL LOCATION OF UBER RIDES    ######################
ggplot(data_2014) + 
  geom_hex(aes(x = Lat, y = Lon), bins = 80) + 
  ggtitle('Hex Map of Geographical Location of Uber Rides') + 
  xlab('Latitude (Degrees North)')  +
  ylab('Longitude (Degrees East)')






