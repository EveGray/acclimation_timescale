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
         Time = sub(".*\\s", "", Date_time))

## real is 07:30 and 09:00 but the sensor is off by an hour and a half
# Filter data to get the morning values (time between 07:30 and 09:00)
morning_par <- ts_par_data %>%
  filter(Time >= "09:00:00" & Time <= "10:25:00")

head(morning_par)
## real is 09:00 and 22:00 but the sensor is off by an hour and a half
# Filter data to get max values (time between 09:00 and 22:00)
max_par <- ts_par_data %>%
  filter(Time >= "10:30:00" & Time <= "23:30:00")

head(max_par)
## real is 22:00 and 23:30 but the sensor is off by an hour and a half
# Filter data to get afternoon values (time between 22:00 and 23:30)
evening_par <- ts_par_data %>%
  filter((Time >= "23:35:00" & Time <= "23:59:59") | (Time >= "00:00:00" & Time <= "01:00:00"))

head(evening_par)

# Calculate the mean and standard deviation for morning low and high light
morning_low_light <- morning_par %>%
  summarise(across(ends_with("l"), 
                   list(mean = ~mean(.[. != 0], na.rm = TRUE),
                        sd = ~sd(.[. != 0], na.rm = TRUE))))
head(morning_low_light)

morning_high_light <- morning_par %>%
  summarise(across(ends_with("h"), 
                   list(mean = ~mean(.[. != 0], na.rm = TRUE),
                        sd = ~sd(.[. != 0], na.rm = TRUE))))

# Calculate the mean and standard deviation for low and high during "max" hours
max_low_light <- max_par %>%
  summarise(across(ends_with("l"), 
                   list(mean = ~mean(.[. != 0], na.rm = TRUE),
                        sd = ~sd(.[. != 0], na.rm = TRUE))))

max_high_light <- max_par %>%
  summarise(across(ends_with("h"), 
                   list(mean = ~mean(.[. != 0], na.rm = TRUE),
                        sd = ~sd(.[. != 0], na.rm = TRUE))))

# Calculate the mean and standard deviation for morning low and high light
evening_low_light <- evening_par %>%
  summarise(across(ends_with("l"), 
                   list(mean = ~mean(.[. != 0], na.rm = TRUE),
                        sd = ~sd(.[. != 0], na.rm = TRUE))))

evening_high_light <- evening_par %>%
  summarise(across(ends_with("h"), 
                   list(mean = ~mean(.[. != 0], na.rm = TRUE),
                        sd = ~sd(.[. != 0], na.rm = TRUE))))

ramping_data_low <- data.frame(
  "Ramp" = c("Morning (0730 - 0900)", "Max (0900 - 2200)", "Evening (2200 - 2330)"),
  "Percentage" = c("33%", "67%", "33%"),
  "Average PAR" = c(morning_low_light[[1]], max_low_light[[1]], evening_low_light[[1]]),
  "SD" = c(morning_low_light[[2]], max_low_light[[2]], evening_low_light[[2]])
)

# Create a flextable for low light
ft_ramp_low <- flextable(ramping_data_low)

# Formatting
ft_ramp_low <- theme_box(ft_ramp_low) # Add borders to the table
ft_ramp_low <- autofit(ft_ramp_low) # Auto fit the table to the content

# Display the flextable for low light
ft_ramp_low


ramping_data_high <- data.frame(
  "Ramp" = c("Morning (0730 - 0900)", "Max (0900 - 2200)", "Evening (2200 - 2330)"),
  "Percentage" = c("67%", "100%", "67%"),
  "Average PAR" = c(morning_high_light[[1]], max_high_light[[1]], evening_high_light[[1]]),
  "SD" = c(morning_high_light[[2]], max_high_light[[2]], evening_high_light[[2]])
)

# Create a flextable for high light
ft_ramp_high <- flextable(ramping_data_high)

# Formatting
ft_ramp_high <- theme_box(ft_ramp_high) # Add borders to the table
ft_ramp_high <- autofit(ft_ramp_high) # Auto fit the table to the content

# Display the flextable for low light
ft_ramp_high
