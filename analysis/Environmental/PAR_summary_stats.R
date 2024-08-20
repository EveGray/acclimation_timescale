### PAR summary data for daily average and daily max

install.packages("dplyr")
install.packages("readxl")
library(dplyr)
library(readxl)

# Load excel data
par <- "ts_par_data.xlsx"
ts_par_data <- read_excel(par)

# Separate the Date_time column into Date and Time columns
ts_par_data <- ts_par_data %>%
  mutate(Date = as.Date(sub("T.*", "", Date_time)),
         Time = sub(".*T", "", Date_time))

# Filter data to get the daily max values (time between 11:20 and 22:45)
day_max_data <- ts_par_data %>%
  filter(Time >= "11:20" & Time <= "22:45")

# Calculate mean and standard deviation for low and high light 
low_light_stats <- ts_par_data %>%
  summarise(across(ends_with("l"), 
                   list(mean = ~mean(.[. != 0], na.rm = TRUE),
                        sd = ~sd(.[. != 0], na.rm = TRUE))))

high_light_stats <- ts_par_data %>%
  summarise(across(ends_with("h"), 
                   list(mean = ~mean(.[. != 0], na.rm = TRUE),
                        sd = ~sd(.[. != 0], na.rm = TRUE))))

# Calculate mean and standard deviation for low and high during "max" hours
day_max_low_light_stats <- day_max_data %>%
  summarise(across(ends_with("l"), 
                   list(mean = ~mean(.[. != 0], na.rm = TRUE),
                        sd = ~sd(.[. != 0], na.rm = TRUE))))

day_max_high_light_stats <- day_max_data %>%
  summarise(across(ends_with("h"), 
                   list(mean = ~mean(.[. != 0], na.rm = TRUE),
                        sd = ~sd(.[. != 0], na.rm = TRUE))))

print(low_light_stats)
print(high_light_stats)

print(day_max_low_light_stats)
print(day_max_high_light_stats)

