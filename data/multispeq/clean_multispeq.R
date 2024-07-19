# clean_multispeq.R
## file to clean the multispeq data

## read in data (slightly cleaned)
multispeq_data <- read.csv('multi_speq_data_raw_all.csv')
head(multispeq_data)
colnames(multispeq_data)

## label light/dark based on PAR for those that are incorrectly labeled
dark_highlight <- subset(multispeq_data, Type == 'Dark' & Light.Intensity..PAR. > 10) # 2 oddballs
dark_highlight$Light.Intensity..PAR. ## 2 oddballs

multispeq_data$Type[multispeq_data$Type == 'Dark' & multispeq_data$Light.Intensity..PAR. > 10] <- "Light"

light_lowlight <- subset(multispeq_data, Type == 'Light' & Light.Intensity..PAR. < 200 & qL > 0.5)
light_lowlight$Light.Intensity..PAR. ## 7 oddballs

multispeq_data$Type[multispeq_data$Type == 'Light' & multispeq_data$Light.Intensity..PAR. < 200 & multispeq_data$qL > 0.5] <- "Dark"

multispeq_data$Type[multispeq_data$Type == 'Dark' & multispeq_data$qL < 0.5] <- "Light"

## check data
plot(subset(multispeq_data, Type == "Light")$qL) # should be below 0.5
plot(subset(multispeq_data, Type == "Dark")$qL) # should be above 0.5

## output clean data
write.csv(multispeq_data, 'multi_speq_data_cleaned.csv')
