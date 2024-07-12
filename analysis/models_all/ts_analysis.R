# ts_analysis.R
## script to analyze timescale experiment data

## load libraries
library(tidyverse)
library(lme4)
library(emmeans)
library(car)

## load data
multipeq_data <- read.csv('data/multispeq/multi_speq_data_cleaned.csv')
licor_photo_data <- read.csv('data/aci_output/all_curve_fits.csv')
licor_resp_data <- read.csv('data/licor/licor_cleaned/tsrd_merged_all.csv')
struc_data <- read.csv('data/Structual/ts_structural_data.csv')

## multispeq data analysis
colnames(multipeq_data)

### separate out staring and ending light treatments
multipeq_data$starting_trt <- NA
multipeq_data$starting_trt[multipeq_data$Treatment == 'LC' | multipeq_data$Treatment == 'LH'] <- 'low'
multipeq_data$starting_trt[multipeq_data$Treatment == 'HC' | multipeq_data$Treatment == 'HL'] <- 'high'

multipeq_data$ending_trt <- NA
multipeq_data$ending_trt[multipeq_data$Treatment == 'LC' | multipeq_data$Treatment == 'HL'] <- 'low'
multipeq_data$ending_trt[multipeq_data$Treatment == 'HC' | multipeq_data$Treatment == 'LH'] <- 'high'

### assign group numbers
multipeq_data <- multipeq_data %>%
  mutate(group = case_when(
    Second.number %in% c(9,10,11,12,13,15,16,25,26,27,29,30,31) ~ "group1",
    Second.number %in% c(1,2,5,6,7,41,43,44,45,47,48) ~ "group2",
    Second.number %in% c(18,19,20,21,22,24,33,34,35,37,38,40) ~ "group3",
  ))

### make a date adjustment to account for new years
multipeq_data$date_multiyear <- multipeq_data$Date
multipeq_data$date_multiyear[multipeq_data$Date < 300] <- multipeq_data$Date[multipeq_data$Date < 300] + 365

### now adjust to days since first measurement
multipeq_data$first_msmt_date <- NA
multipeq_data$first_msmt_date[multipeq_data$group == 'group1'] <- 352
multipeq_data$first_msmt_date[multipeq_data$group == 'group2'] <- 358
multipeq_data$first_msmt_date[multipeq_data$group == 'group3'] <- 360

multipeq_data$days_since_first <- multipeq_data$date_multiyear - multipeq_data$first_msmt_date

### create category column for dates
multipeq_data$days_since_first_factor <- as.factor(multipeq_data$days_since_first)

### create dfs with just light or just dark acclimated data
multipeq_data_light <- subset(multipeq_data, Type == "Light")
multipeq_data_dark <- subset(multipeq_data, Type == "Dark")

### question 1: How does non photo chemical quenching change overtime in the old
###  leaf?
hist(multipeq_data_light$NPQt) # take a look at the dark acclimated NPQt data.
NPQt_lmer <- lmer(NPQt ~ starting_trt * ending_trt * days_since_first + (1|Chamber), 
                  data = subset(multipeq_data_light, New == 'N'))
plot(resid(NPQt_lmer) ~ fitted(NPQt_lmer))
summary(NPQt_lmer)
Anova(NPQt_lmer)
emmeans(NPQt_lmer, ~starting_trt)
emmeans(NPQt_lmer, ~ending_trt)
emtrends(NPQt_lmer, ~1, var = 'days_since_first')
emtrends(NPQt_lmer, ~starting_trt, var = 'days_since_first')
emtrends(NPQt_lmer, ~ending_trt, var = 'days_since_first')
emmeans(NPQt_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 0))
emmeans(NPQt_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 10))

### Result Q1:
### Significant effect across start and end trt, time, and all combos. Specifically, 
### NPQt remains high in HC and increases slowly. LC NPQt stays low throughout. 
### HL starts high, gets lower and stays low. LH starts low and increases a lot. 

### question 2(a): How does quantum efficiency change overtime in the old
###  leaf?
hist(multipeq_data_light$FvP_over_FmP) # take a look at the dark acclimated FvP_over_FmP data.
FvP_over_FmP_lmer <- lmer(FvP_over_FmP ~ starting_trt * ending_trt * days_since_first + (1|Chamber), 
                          data = subset(multipeq_data_light, New == 'N'))
plot(resid(FvP_over_FmP_lmer) ~ fitted(FvP_over_FmP_lmer))
summary(FvP_over_FmP_lmer)
Anova(FvP_over_FmP_lmer)
emmeans(FvP_over_FmP_lmer, ~starting_trt)
emmeans(FvP_over_FmP_lmer, ~ending_trt)
emtrends(FvP_over_FmP_lmer, ~1, var = 'days_since_first')
emtrends(FvP_over_FmP_lmer, ~starting_trt, var = 'days_since_first')
emtrends(FvP_over_FmP_lmer, ~ending_trt, var = 'days_since_first')
emmeans(FvP_over_FmP_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 0))
emmeans(FvP_over_FmP_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 10))

### Result Q2(a):
### Significant effect across start and end trt, time, and the combo of starting 
### and ending treatment.  
### 

### question 2(b): How does non photo chemical quenching change overtime in the new
###  leaf?
hist(multipeq_data_dark$FvP_over_FmP) # take a look at the light acclimated FvP_over_FmP data.
FvP_over_FmP_lmer <- lmer(FvP_over_FmP ~ starting_trt * ending_trt * days_since_first + (1|Chamber), 
                          data = subset(multipeq_data_light, New == 'N'))
plot(resid(FvP_over_FmP_lmer) ~ fitted(FvP_over_FmP_lmer))
summary(FvP_over_FmP_lmer)
Anova(FvP_over_FmP_lmer)
emmeans(FvP_over_FmP_lmer, ~starting_trt)
emmeans(FvP_over_FmP_lmer, ~ending_trt)
emtrends(FvP_over_FmP_lmer, ~1, var = 'days_since_first')
emtrends(FvP_over_FmP_lmer, ~starting_trt, var = 'days_since_first')
emtrends(FvP_over_FmP_lmer, ~ending_trt, var = 'days_since_first')
emmeans(FvP_over_FmP_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 0))
emmeans(FvP_over_FmP_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 10))

### Result Q2(b):

### question 3(a): How does SPAD change overtime in the old
###  leaf?
hist(multipeq_data_light$SPAD) # take a look at the dark acclimated FvP_over_FmP data.
SPAD_lmer <- lmer(SPAD ~ starting_trt * ending_trt * days_since_first + (1|Chamber), 
                  data = subset(multipeq_data_light, New == 'N'))
plot(resid(SPAD_lmer) ~ fitted(SPAD_lmer))
summary(SPAD_lmer)
Anova(SPAD_lmer)
emmeans(SPAD_lmer, ~starting_trt)
emmeans(SPAD_lmer, ~ending_trt)
emtrends(SPAD_lmer, ~1, var = 'days_since_first')
emtrends(SPAD_lmer, ~starting_trt, var = 'days_since_first')
emtrends(SPAD_lmer, ~ending_trt, var = 'days_since_first')
emmeans(SPAD_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 0))
emmeans(SPAD_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 7))

### Result 3(a):

### question 3(b): How does SPAD change overtime in the new
###  leaf?
hist(multipeq_data_light$SPAD) # take a look at the dark acclimated FvP_over_FmP data.
SPAD_lmer <- lmer(SPAD ~ starting_trt * ending_trt * days_since_first + (1|Chamber), 
                  data = subset(multipeq_data_light, New == 'Y'))
plot(resid(SPAD_lmer) ~ fitted(SPAD_lmer))
summary(SPAD_lmer)
Anova(SPAD_lmer)
emmeans(SPAD_lmer, ~starting_trt)
emmeans(SPAD_lmer, ~ending_trt)
emtrends(SPAD_lmer, ~1, var = 'days_since_first')
emtrends(SPAD_lmer, ~starting_trt, var = 'days_since_first')
emtrends(SPAD_lmer, ~ending_trt, var = 'days_since_first')
emmeans(SPAD_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 18))
emmeans(SPAD_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 21))

### Result 3(b):
### No significant interactions except for a weak one with ending and time. 

### question 4(a): How does qL change overtime in the old
###  leaf?
hist(multipeq_data_light$qL) # take a look at the dark acclimated FvP_over_FmP data.
qL_lmer <- lmer(qL ~ starting_trt * ending_trt * days_since_first + (1|Chamber), 
                data = subset(multipeq_data_light, New == 'N'))
plot(resid(qL_lmer) ~ fitted(qL_lmer))
summary(qL_lmer)
Anova(qL_lmer)
emmeans(qL_lmer, ~starting_trt)
emmeans(qL_lmer, ~ending_trt)
emtrends(qL_lmer, ~1, var = 'days_since_first')
emtrends(qL_lmer, ~starting_trt, var = 'days_since_first')
emtrends(qL_lmer, ~ending_trt, var = 'days_since_first')
emmeans(qL_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 0))
emmeans(qL_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 10))

### Result 4(a):
### Signifficant interactions in starting, and time, with ending also being sig
### but one degree less. The combo that has signiffiance is strting/days since first.

### question 4(b): How does qL change overtime in the new
###  leaf?
hist(multipeq_data_dark$qL) # take a look at the dark acclimated FvP_over_FmP data.
qL_lmer <- lmer(qL ~ starting_trt * ending_trt * days_since_first + (1|Chamber), 
                data = subset(multipeq_data_light, New == 'Y'))
plot(resid(qL_lmer) ~ fitted(qL_lmer))
summary(qL_lmer)
Anova(qL_lmer)
emmeans(qL_lmer, ~starting_trt)
emmeans(qL_lmer, ~ending_trt)
emtrends(qL_lmer, ~1, var = 'days_since_first')
emtrends(qL_lmer, ~starting_trt, var = 'days_since_first')
emtrends(qL_lmer, ~ending_trt, var = 'days_since_first')
emmeans(qL_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 18))
emmeans(qL_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 20))

### Result 4(b):
### weak interactions in starting and the combo of starting/days since first.

### question 2: how does linear electron flow vary with time and treatment? Just looking at old leaves
hist(multipeq_data_light$LEF) # take a look at the light acclimated phi2 data, looks okay!
LEF_lmer <- lmer(LEF ~ starting_trt * ending_trt * days_since_first + (1|Chamber), 
                  data = subset(multipeq_data_light, New == 'N'))
plot(resid(LEF_lmer) ~ fitted(LEF_lmer))
summary(LEF_lmer)
Anova(LEF_lmer)
emmeans(LEF_lmer, ~starting_trt*ending_trt)
emtrends(LEF_lmer, ~1, var = 'days_since_first')
emtrends(LEF_lmer, ~starting_trt, var = 'days_since_first')

emtrends(LEF_lmer, ~ending_trt, var = 'days_since_first') 
emmeans(LEF_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 1))
emmeans(LEF_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 2))
emmeans(LEF_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 20))

hist(multipeq_data_light$leaf_thickness) # take a look at the light acclimated leaf thicknes data, looks okay!
leafw_lmer <- lmer(leaf_thickness ~ starting_trt * ending_trt * days_since_first + (1|Chamber), 
                  data = subset(multipeq_data_light, New == 'Y'))
plot(resid(leafw_lmer) ~ fitted(leafw_lmer))
summary(leafw_lmer)
Anova(leafw_lmer)
emmeans(leafw_lmer, ~starting_trt*ending_trt)
emtrends(leafw_lmer, ~1, var = 'days_since_first')
emtrends(leafw_lmer, ~starting_trt, var = 'days_since_first')
emtrends(leafw_lmer, ~ending_trt, var = 'days_since_first') 
emmeans(leafw_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 21))
emmeans(leafw_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 10))

#Licor data analysis

### Separate id intro individual parts
 licor_photo_data <- licor_photo_data %>%
  extract(id, into = c("chamber", "treatment", "first_number", "second_number"),
          regex = "([a-zA-Z]{4})\\.([a-zA-Z]{2})\\.(\\d)\\.(\\d+)", convert = TRUE, remove = FALSE) %>%
  mutate(across(c(chamber, treatment, first_number, second_number), as.character))

### separate out staring and ending light treatments
licor_photo_data$starting_trt <- NA
licor_photo_data$starting_trt[licor_photo_data$treatment == 'lc' | licor_photo_data$treatment == 'lh'] <- 'low'
licor_photo_data$starting_trt[licor_photo_data$treatment == 'hc' | licor_photo_data$treatment == 'hl'] <- 'high'

licor_photo_data$ending_trt <- NA
licor_photo_data$ending_trt[licor_photo_data$treatment == 'lc' | licor_photo_data$treatment == 'hl'] <- 'low'
licor_photo_data$ending_trt[licor_photo_data$treatment == 'hc' | licor_photo_data$treatment == 'lh'] <- 'high'

### assign group numbers
licor_photo_data <- licor_photo_data %>%
  mutate(group = case_when(
    second_number %in% c(9,10,11,12,13,15,16,25,26,27,29,30,31) ~ "group1",
    second_number %in% c(1,2,5,6,7,41,43,44,45,47,48) ~ "group2",
    second_number %in% c(18,19,20,21,22,24,33,34,35,37,38,40) ~ "group3",
     ))

### Make a date column
licor_photo_data <- licor_photo_data %>%
  separate(unique_id, into = c("id", "date"), sep = "_", remove = FALSE)

licor_photo_data <- licor_photo_data %>%
  mutate(date = case_when(
    date == "2023-12-18" ~ "352",
    date == "2023-12-21" ~ "355",
    date == "2023-12-24" ~ "358",
    date == "2023-12-25" ~ "359",
    date == "2023-12-26" ~ "360",
    date == "2023-12-27" ~ "361",
    date == "2023-12-28" ~ "362",
    date == "2023-12-30" ~ "364",
    date == "2023-12-31" ~ "365",
    date == "2024-01-01" ~ "1",
    date == "2024-01-02" ~ "2",
    date == "2024-01-03" ~ "3",
    date == "2024-01-04" ~ "4",
    date == "2024-01-05" ~ "5",
    date == "2024-01-07" ~ "7",
    date == "2024-01-09" ~ "9",
    date == "2024-01-10" ~ "10",
    date == "2024-01-11" ~ "11",
    date == "2024-01-12" ~ "12",
    date == "2024-01-14" ~ "14",
    date == "2024-01-15" ~ "15",
    date == "2024-01-16" ~ "16",
    date == "2024-01-17" ~ "17",
    date == "2024-01-18" ~ "18",
    date == "2024-01-19" ~ "19",
    date == "2024-01-21" ~ "21",
    TRUE ~ date))

### make a date adjustment to account for new years
licor_photo_data$date_multiyear <- licor_photo_data$date

### Make sure columns are numeric 

if (!is.numeric(licor_photo_data$date_multiyear)) {
  licor_photo_data$date_multiyear <- as.numeric(as.character(licor_photo_data$date_multiyear))
}

if (!is.numeric(licor_photo_data$date)) {
  licor_photo_data$date <- as.numeric(as.character(licor_photo_data$date))
}

### Account for new years

licor_photo_data$date_multiyear[licor_photo_data$date < 300] <- 
  licor_photo_data$date[licor_photo_data$date < 300] + 365

### now adjust to days since first measurement
licor_photo_data$first_msmt_date <- NA
licor_photo_data$first_msmt_date[licor_photo_data$group == 'group1'] <- 352
licor_photo_data$first_msmt_date[licor_photo_data$group == 'group2'] <- 358
licor_photo_data$first_msmt_date[licor_photo_data$group == 'group3'] <- 360

licor_photo_data$days_since_first <- licor_photo_data$date_multiyear - licor_photo_data$first_msmt_date

### create category column for dates
licor_photo_data$days_since_first_factor <- as.factor(licor_photo_data$days_since_first)

### Seperate old and new measuremnts 
licor_photo_data <- licor_photo_data %>%
  mutate(New = case_when(
    date %in% c(352, 358, 360, 355, 361, 364, 359, 365, 2, 362, 3, 5, 1, 7, 9, 4, 10, 12) ~ "N",
    date %in% c(11, 14, 16, 15, 17, 19, 18, 21) ~ "Y",
  ))

### Test data frame on vcmax data

hist(licor_photo_data$vcmax_tleaf) 
vcmax_tleaf_lmer <- lmer(vcmax_tleaf ~ starting_trt * ending_trt * days_since_first + (1|chamber), 
                   data = subset(licor_photo_data, New == 'N'))
plot(resid(vcmax_tleaf_lmer) ~ fitted(vcmax_tleaf_lmer))
summary(vcmax_tleaf_lmer)
Anova(vcmax_tleaf_lmer)
emmeans(vcmax_tleaf_lmer, ~starting_trt*ending_trt)
emtrends(vcmax_tleaf_lmer, ~1, var = 'days_since_first')
emtrends(vcmax_tleaf_lmer, ~starting_trt, var = 'days_since_first')
emtrends(vcmax_tleaf_lmer, ~ending_trt, var = 'days_since_first') 
emmeans(vcmax_tleaf_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 0))
emmeans(vcmax_tleaf_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 10))

### Resp data analysis

licor_resp_data <- licor_resp_data %>%
  extract(id, into = c("chamber", "treatment", "first_number", "second_number"),
          regex = "([a-zA-Z]{4})\\.([a-zA-Z]{2})\\.(\\d)\\.(\\d+)", convert = TRUE, remove = FALSE) %>%
  mutate(across(c(chamber, treatment, first_number, second_number), as.character))

### separate out staring and ending light treatments
licor_resp_data$starting_trt <- NA
licor_resp_data$starting_trt[licor_resp_data$treatment == 'lc' | licor_resp_data$treatment == 'lh'] <- 'low'
licor_resp_data$starting_trt[licor_resp_data$treatment == 'hc' | licor_resp_data$treatment == 'hl'] <- 'high'

licor_resp_data$ending_trt <- NA
licor_resp_data$ending_trt[licor_resp_data$treatment == 'lc' | licor_resp_data$treatment == 'hl'] <- 'low'
licor_resp_data$ending_trt[licor_resp_data$treatment == 'hc' | licor_resp_data$treatment == 'lh'] <- 'high'

### assign group numbers
licor_resp_data <- licor_resp_data %>%
  mutate(group = case_when(
    second_number %in% c(9,10,11,12,13,15,16,25,26,27,29,30,31) ~ "group1",
    second_number %in% c(1,2,5,6,7,41,43,44,45,47,48) ~ "group2",
    second_number %in% c(18,19,20,21,22,24,33,34,35,37,38,40) ~ "group3",
  ))

licor_resp_data <- licor_resp_data %>%
  separate(date, into = c("Date", "time"), sep = " ", remove = FALSE)

### Change dates to Julian dates

licor_resp_data <- licor_resp_data %>%
  mutate(Date = case_when(
    Date == "12/18/2023" ~ "352",
    Date == "12/21/2023" ~ "355",
    Date == "12/24/2023" ~ "358",
    Date == "12/25/2023" ~ "359",
    Date == "12/26/2023" ~ "360",
    Date == "12/27/2023" ~ "361",
    Date == "12/28/2023" ~ "362",
    Date == "12/30/2023" ~ "364",
    Date == "12/31/2023" ~ "365",
    Date == "1/1/2024" ~ "1",
    Date == "1/2/2024" ~ "2",
    Date == "1/3/2024" ~ "3",
    Date == "1/4/2024" ~ "4",
    Date == "1/5/2024" ~ "5",
    Date == "1/7/2024" ~ "7",
    Date == "1/9/2024" ~ "9",
    Date == "1/10/2024" ~ "10",
    Date == "1/11/2024" ~ "11",
    Date == "1/12/2024" ~ "12",
    Date == "1/14/2024" ~ "14",
    Date == "1/15/2024" ~ "15",
    Date == "1/16/2024" ~ "16",
    Date == "1/17/2024" ~ "17",
    Date == "1/18/2024" ~ "18",
    Date == "1/19/2024" ~ "19",
    Date == "1/21/2024" ~ "21",
    TRUE ~ Date))

### make a date adjustment to account for new years
licor_resp_data$date_multiyear <- licor_resp_data$Date

### Check to make sure the column is numeric 

if (!is.numeric(licor_resp_data$date_multiyear)) {
  licor_resp_data$date_multiyear <- as.numeric(as.character(licor_resp_data$date_multiyear))
}

if (!is.numeric(licor_resp_data$Date)) {
  licor_resp_data$Date <- as.numeric(as.character(licor_resp_data$Date))
}

licor_resp_data$date_multiyear[licor_resp_data$Date < 300] <- 
  licor_resp_data$Date[licor_resp_data$Date < 300] + 365

### now adjust to days since first measurement
licor_resp_data$first_msmt_date <- NA
licor_resp_data$first_msmt_date[licor_resp_data$group == 'group1'] <- 352
licor_resp_data$first_msmt_date[licor_resp_data$group == 'group2'] <- 358
licor_resp_data$first_msmt_date[licor_resp_data$group == 'group3'] <- 360

licor_resp_data$days_since_first <- licor_resp_data$date_multiyear - licor_resp_data$first_msmt_date

### create category column for dates
licor_resp_data$days_since_first_factor <- as.factor(licor_resp_data$days_since_first)

### Separate old and new measurements 
licor_resp_data <- licor_resp_data %>%
  mutate(New = case_when(
    Date %in% c(352, 358, 360, 355, 361, 364, 359, 365, 2, 362, 3, 5, 1, 7, 9, 4, 10, 12) ~ "N",
    Date %in% c(11, 14, 16, 15, 17, 19, 18, 21) ~ "Y",
  ))

### Test data frame on A values

hist(licor_resp_data$A) 
resp_lmer <- lmer(A ~ starting_trt * ending_trt * days_since_first + (1|chamber), 
                         data = subset(licor_resp_data, New == 'N'))
plot(resid(resp_lmer) ~ fitted(resp_lmer))
summary(resp_lmer)
Anova(resp_lmer)
emmeans(resp_lmer, ~starting_trt*ending_trt)
emtrends(resp_lmer, ~1, var = 'days_since_first')
emtrends(resp_lmer, ~starting_trt, var = 'days_since_first')
emtrends(resp_lmer, ~ending_trt, var = 'days_since_first') 
emmeans(resp_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 0))
emmeans(resp_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 7))

#Structural data analysis

### Separate out ID intro parts
struc_data <- struc_data %>%
  extract(ID, into = c("chamber", "treatment", "first_number", "second_number"),
          regex = "([a-zA-Z]{4})\\.([a-zA-Z]{2})\\.(\\d)\\.(\\d+)", convert = TRUE, remove = FALSE) %>%
  mutate(across(c(chamber, treatment, first_number, second_number), as.character))

### separate out staring and ending light treatments
struc_data$starting_trt <- NA
struc_data$starting_trt[struc_data$treatment == 'lc' | struc_data$treatment == 'lh'] <- 'low'
struc_data$starting_trt[struc_data$treatment == 'hc' | struc_data$treatment == 'hl'] <- 'high'

struc_data$ending_trt <- NA
struc_data$ending_trt[struc_data$treatment == 'lc' | struc_data$treatment == 'hl'] <- 'low'
struc_data$ending_trt[struc_data$treatment == 'hc' | struc_data$treatment == 'lh'] <- 'high'
