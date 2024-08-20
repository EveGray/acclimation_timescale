### Environmental Temp and RH data summary statistics 

### Load packages
install.packages("dplyr")
library(dplyr)

### Load data
chamber_3_data <- read.csv("chamber_3_data.csv")
chamber_4_data <- read.csv("chamber_4_data.csv")
chamber_5_data <- read.csv("chamber_5_data.csv")

### Split the Date.Time column into Date and Time
chamber_3_data$Date <- as.Date(chamber_3_data$Date.Time, format="%m/%d/%Y %H:%M")
chamber_3_data$Time <- format(as.POSIXct(chamber_3_data$Date.Time, format="%m/%d/%Y %H:%M"), "%H:%M")

chamber_4_data$Date <- as.Date(chamber_4_data$Date.Time, format="%m/%d/%Y %H:%M")
chamber_4_data$Time <- format(as.POSIXct(chamber_4_data$Date.Time, format="%m/%d/%Y %H:%M"), "%H:%M")

chamber_5_data$Date <- as.Date(chamber_5_data$Date.Time, format="%m/%d/%Y %H:%M")
chamber_5_data$Time <- format(as.POSIXct(chamber_5_data$Date.Time, format="%m/%d/%Y %H:%M"), "%H:%M")

### Filter data into day and night for each chamber
chamber_3_night <- chamber_3_data %>% filter(Time >= "01:10" & Time <= "09:00")
chamber_3_day <- chamber_3_data %>% filter(Time < "01:10" | Time > "09:00")

chamber_4_night <- chamber_4_data %>% filter(Time >= "01:10" & Time <= "09:00")
chamber_4_day <- chamber_4_data %>% filter(Time < "01:10" | Time > "09:00")

chamber_5_night <- chamber_5_data %>% filter(Time >= "01:10" & Time <= "09:00")
chamber_5_day <- chamber_5_data %>% filter(Time < "01:10" | Time > "09:00")

### Calculate daytime mean and SD for RH and Temp 
chamber_3_day_stats <- chamber_3_day %>% 
  summarise(Temp_Mean = mean(Temperature), Temp_SD = sd(Temperature), RH_Mean = mean(RH), RH_SD = sd(RH))

chamber_4_day_stats <- chamber_4_day %>% 
  summarise(Temp_Mean = mean(Temperature), Temp_SD = sd(Temperature), RH_Mean = mean(RH), RH_SD = sd(RH))

chamber_5_day_stats <- chamber_5_day %>% 
  summarise(Temp_Mean = mean(Temperature), Temp_SD = sd(Temperature), RH_Mean = mean(RH), RH_SD = sd(RH))

### Calculate nighttime summary statistics mean and SD for RH and Temp 
chamber_3_night_stats <- chamber_3_night %>% 
  summarise(Temp_Mean = mean(Temperature), Temp_SD = sd(Temperature), RH_Mean = mean(RH), RH_SD = sd(RH))

chamber_4_night_stats <- chamber_4_night %>% 
  summarise(Temp_Mean = mean(Temperature), Temp_SD = sd(Temperature), RH_Mean = mean(RH), RH_SD = sd(RH))

chamber_5_night_stats <- chamber_5_night %>% 
  summarise(Temp_Mean = mean(Temperature), Temp_SD = sd(Temperature), RH_Mean = mean(RH), RH_SD = sd(RH))


print(chamber_3_day_stats)
print(chamber_4_day_stats)
print(chamber_5_day_stats)
print(chamber_3_night_stats)
print(chamber_4_night_stats)
print(chamber_5_night_stats)
