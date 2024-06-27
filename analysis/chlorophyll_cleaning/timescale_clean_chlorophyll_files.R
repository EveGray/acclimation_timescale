## Libraries
library(tidyverse)

## Read in .txt files and do some light cleaning
plate1 <- read.delim("Gray_chlorophyll_plate1.txt", header = TRUE) %>%
  dplyr::select(id = Name, 
                abs_649 = chlorophyll_absorbance.649,
                abs_665 = chlorophyll_absorbance.665)

plate2 <- read.delim("Gray_chlorophyll_plate2.txt", header = TRUE) %>%
  dplyr::select(id = Name, 
                abs_649 = chlorophyll_absorbance.649,
                abs_665 = chlorophyll_absorbance.665)

#######################################
## Plate 1
#######################################
## Calculate mean of blanks
chlorophyll_blanks_plate1 <- plate1 %>%
  filter(id == "BLK") %>%
  summarize(blank_649 = mean(abs_649),
            blank_665 = mean(abs_665)) %>%
  mutate(blank_649 = round(blank_649, digits = 3),
         blank_665 = round(blank_665, digits = 3))

## Add means of the blanks to plate1 raw file
plate1$blank_649 <- chlorophyll_blanks_plate1$blank_649
plate1$blank_665 <- chlorophyll_blanks_plate1$blank_665

## Calculate blank-corrected a649 and a665
plate1 <- plate1 %>%
  mutate(abs_649_corrected = abs_649 - blank_649,
         abs_665_corrected = abs_665 - blank_665) %>%
  mutate(abs_649_corrected = round(abs_649_corrected, digits = 3),
         abs_665_corrected = round(abs_665_corrected, digits = 3))

#######################################
## Plate 2
#######################################
## Calculate mean of blanks
chlorophyll_blanks_plate2 <- plate2 %>%
  filter(id == "BLK") %>%
  summarize(blank_649 = mean(abs_649),
            blank_665 = mean(abs_665)) %>%
  mutate(blank_649 = round(blank_649, digits = 3),
         blank_665 = round(blank_665, digits = 3))

## Add means of the blanks to plate2
plate2$blank_649 <- chlorophyll_blanks_plate2$blank_649
plate2$blank_665 <- chlorophyll_blanks_plate2$blank_665

## Calculate blank-corrected a649 and a665
plate2 <- plate2 %>%
  mutate(abs_649_corrected = abs_649 - blank_649,
         abs_665_corrected = abs_665 - blank_665) %>%
  mutate(abs_649_corrected = round(abs_649_corrected, digits = 3),
         abs_665_corrected = round(abs_665_corrected, digits = 3))


## Write files
# write.csv(plate1, "Gray_chlorophyll_plate1_cleaned.csv", row.names = F)
# write.csv(plate2, "Gray_chlorophyll_plate2_cleaned.csv", row.names = F)

