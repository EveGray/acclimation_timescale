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

### Combine daytime data from all chambers
combined_day <- bind_rows(chamber_3_day, chamber_4_day, chamber_5_day)

### Combine nighttime data from all chambers
combined_night <- bind_rows(chamber_3_night, chamber_4_night, chamber_5_night)

### Calculate daytime mean and SD for all chambers
day_stats <- combined_day %>% 
  summarise(Temp_Mean = mean(Temperature, na.rm = TRUE), Temp_SD = sd(Temperature, na.rm = TRUE), 
            RH_Mean = mean(RH, na.rm = TRUE), RH_SD = sd(RH, na.rm = TRUE))

### Calculate nighttime mean and SD for all chambers
night_stats <- combined_night %>% 
  summarise(Temp_Mean = mean(Temperature, na.rm = TRUE), Temp_SD = sd(Temperature, na.rm = TRUE), 
            RH_Mean = mean(RH, na.rm = TRUE), RH_SD = sd(RH, na.rm = TRUE))

### Print summary statistics for daytime and nighttime
print(day_stats)
print(night_stats)