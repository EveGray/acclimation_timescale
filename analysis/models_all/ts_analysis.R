# ts_analysis.R
## script to analyze timescale experiment data

install.packages('patchwork')

## load libraries
library(tidyverse)
library(lme4)
library(emmeans)
library(car)
library(multcomp)
library(ggplot2)
library(patchwork)
library(dplyr)

## load data
multipeq_data <- read.csv('../../data/multispeq/multi_speq_data_cleaned.csv')
licor_photo_data <- read.csv('../../data/aci_output/all_curve_fits.csv')
licor_resp_data <- read.csv('../../data/licor/licor_cleaned/tsrd_merged_all.csv')
struc_data <- read.csv('../../data/Structual/ts_structural_data.csv')
nutrient_data <- read.csv("ts_cn_data.csv")

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
    Second.number %in% c(9,10,11,13,15,16,25,26,27,29,30,31) ~ "group1",
    Second.number %in% c(1,2,5,6,7,41,43,44,45,47,48) ~ "group2",
    Second.number %in% c(18,19,20,21,22,24,33,34,35,37,38,40) ~ "group3",
  ))

### make a date adjustment to account for new years
multipeq_data$date_multiyear <- multipeq_data$Date

if (!is.numeric(multipeq_data$Date)) {
  multipeq_data$Date <- as.numeric(as.character(multipeq_data$Date))
}

if (!is.numeric(multipeq_data$date_multiyear)) {
  multipeq_data$date_multiyear <- as.numeric(as.character(multipeq_data$date_multiyear))
}

multipeq_data$date_multiyear[multipeq_data$Date < 300] <- multipeq_data$Date[multipeq_data$Date < 300] + 365

### now adjust to days since first measurement
multipeq_data$first_msmt_date <- NA
multipeq_data$first_msmt_date[multipeq_data$group == 'group1'] <- 352
multipeq_data$first_msmt_date[multipeq_data$group == 'group2'] <- 358
multipeq_data$first_msmt_date[multipeq_data$group == 'group3'] <- 360

multipeq_data$days_since_first <- multipeq_data$date_multiyear - multipeq_data$first_msmt_date

### create category column for dates
multipeq_data$days_since_first_factor <- as.factor(multipeq_data$days_since_first)

multipeq_data <- multipeq_data %>%
  mutate(Date = as.character(Date)) %>%
  mutate(cal_date = case_when(
    Date == "352" ~ "2023-12-18",
    Date == "353" ~ "2023-12-19",
    Date == "354" ~ "2023-12-20",
    Date == "355" ~ "2023-12-21",
    Date == "356" ~ "2023-12-22",
    Date == "357" ~ "2023-12-23",
    Date == "358" ~ "2023-12-24",
    Date == "359" ~ "2023-12-25",
    Date == "360" ~ "2023-12-26",
    Date == "361" ~ "2023-12-27",
    Date == "362" ~ "2023-12-28",
    Date == "363" ~ "2023-12-29",
    Date == "364" ~ "2023-12-30",
    Date == "365" ~ "2023-12-31",
    Date == "1" ~ "2024-01-01",
    Date == "2" ~ "2024-01-02",
    Date == "3" ~ "2024-01-03",
    Date == "4" ~ "2024-01-04",
    Date == "5" ~ "2024-01-05",
    Date == "6" ~ "2024-01-06",
    Date == "7" ~ "2024-01-07",
    Date == "8" ~ "2024-01-08",
    Date == "9" ~ "2024-01-09",
    Date == "10" ~ "2024-01-10",
    Date == "11" ~ "2024-01-11",
    Date == "12" ~ "2024-01-12",
    Date == "13" ~ "2024-01-13",
    Date == "14" ~ "2024-01-14",
    Date == "15" ~ "2024-01-15",
    Date == "16" ~ "2024-01-16",
    Date == "17" ~ "2024-01-17",
    Date == "18" ~ "2024-01-18",
    Date == "19" ~ "2024-01-19",
    Date == "20" ~ "2024-01-20",
    Date == "21" ~ "2024-01-21",
    TRUE ~ Date  
  ))

multipeq_data$date <- as.Date(multipeq_data$cal_date, format = "%Y-%m-%d")

### create dfs with just light or just dark acclimated data
multipeq_data_light <- subset(multipeq_data, Type == "Light")
multipeq_data_dark <- subset(multipeq_data, Type == "Dark")


# Old leaf NPQt data model
hist(multipeq_data_light$NPQt) 
NPQt_lmer <- lmer(NPQt ~ starting_trt * ending_trt * days_since_first + (1|Chamber) + (1|Second.number), 
                  data = subset(multipeq_data_light, New == 'N'))
plot(resid(NPQt_lmer) ~ fitted(NPQt_lmer))
summary(NPQt_lmer)
Anova(NPQt_lmer)
emmeans(NPQt_lmer, ~starting_trt)
emmeans(NPQt_lmer, ~ending_trt)
emmeans(NPQt_lmer, ~starting_trt * ending_trt)
emtrends(NPQt_lmer, ~1, var = 'days_since_first')
emtrends(NPQt_lmer, ~starting_trt, var = 'days_since_first')
emtrends(NPQt_lmer, ~ending_trt * starting_trt, var = 'days_since_first')
emmeans(NPQt_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 0))

# New leaf NPQt data model
hist(multipeq_data_light$NPQt) 
NPQt_lmer <- lmer(NPQt ~ starting_trt * ending_trt + (1|Chamber), 
                  data = subset(multipeq_data_light, New == 'Y'))
plot(resid(NPQt_lmer) ~ fitted(NPQt_lmer))
summary(NPQt_lmer)
Anova(NPQt_lmer)
emmeans(NPQt_lmer, ~starting_trt * ending_trt)
emmeans(NPQt_lmer, ~ending_trt)
cld(emmeans(NPQt_lmer, ~starting_trt * ending_trt))

# Old leaf PhiNPQ data model
hist(multipeq_data_light$PhiNPQ) 
PhiNPQ_lmer <- lmer(log(PhiNPQ) ~ starting_trt * ending_trt * days_since_first + (1|Chamber) + (1|Second.number), 
                  data = subset(multipeq_data_light, New == 'N'))
plot(resid(PhiNPQ_lmer) ~ fitted(PhiNPQ_lmer))
summary(PhiNPQ_lmer)
Anova(PhiNPQ_lmer)
emmeans(PhiNPQ_lmer, ~starting_trt)
emmeans(PhiNPQ_lmer, ~ending_trt)
emmeans(PhiNPQ_lmer, ~starting_trt * ending_trt)
emtrends(PhiNPQ_lmer, ~1, var = 'days_since_first')
emtrends(PhiNPQ_lmer, ~starting_trt, var = 'days_since_first')
emtrends(PhiNPQ_lmer, ~ending_trt * starting_trt, var = 'days_since_first')
emmeans(PhiNPQ_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 0))

# New leaf PhiNPQ data model
hist(multipeq_data_light$PhiNPQ) 
PhiNPQ_lmer <- lmer(PhiNPQ_lmer ~ starting_trt * ending_trt + (1|Chamber), 
                  data = subset(multipeq_data_light, New == 'Y'))
plot(resid(PhiNPQ_lmer) ~ fitted(PhiNPQ_lmer))
summary(PhiNPQ_lmer)
Anova(PhiNPQ_lmer)
emmeans(PhiNPQ_lmer, ~starting_trt)
emmeans(PhiNPQ_lmer, ~ending_trt)
emmeans(PhiNPQ_lmer, ~starting_trt * ending_trt)

# Old leaf FvP_over_FmP data
hist(multipeq_data_light$FvP_over_FmP) 
FvP_over_FmP_lmer <- lmer(FvP_over_FmP ~ starting_trt * ending_trt * days_since_first + (1|Chamber) + (1|Second.number), 
                          data = subset(multipeq_data_light, New == 'N'))
plot(resid(FvP_over_FmP_lmer) ~ fitted(FvP_over_FmP_lmer))
summary(FvP_over_FmP_lmer)
Anova(FvP_over_FmP_lmer)
emmeans(FvP_over_FmP_lmer, ~starting_trt * ending_trt)
emmeans(FvP_over_FmP_lmer, ~ending_trt)
emtrends(FvP_over_FmP_lmer, ~1, var = 'days_since_first')
emtrends(FvP_over_FmP_lmer, ~starting_trt, var = 'days_since_first')
emtrends(FvP_over_FmP_lmer, ~starting_trt * ending_trt, var = 'days_since_first')
emmeans(FvP_over_FmP_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 0))

# New leaf FvP_over_FmP data
hist(multipeq_data_light$FvP_over_FmP) 
FvP_over_FmP_lmer <- lmer(FvP_over_FmP ~ starting_trt * ending_trt + (1|Chamber), 
                          data = subset(multipeq_data_light, New == 'Y'))
plot(resid(FvP_over_FmP_lmer) ~ fitted(FvP_over_FmP_lmer))
summary(FvP_over_FmP_lmer)
Anova(FvP_over_FmP_lmer)
emmeans(FvP_over_FmP_lmer, ~starting_trt * ending_trt)
emmeans(FvP_over_FmP_lmer, ~ending_trt)
cld(emmeans(FvP_over_FmP_lmer, ~starting_trt * ending_trt))

# Old leaf thickness data
hist(multipeq_data_light$leaf_thickness)
leaf_thickness <- lmer(leaf_thickness ~ starting_trt * ending_trt * days_since_first + (1|Chamber) + (1|Second.number), 
                          data = subset(multipeq_data_light, New == 'N'))
plot(resid(leaf_thickness) ~ fitted(leaf_thickness))
summary(leaf_thickness)
Anova(leaf_thickness)
emmeans(leaf_thickness, ~starting_trt * ending_trt)
emmeans(leaf_thickness, ~ending_trt)
emtrends(leaf_thickness, ~1, var = 'days_since_first')
emtrends(leaf_thickness, ~starting_trt, var = 'days_since_first')
emtrends(leaf_thickness, ~starting_trt * ending_trt, var = 'days_since_first')
emmeans(leaf_thickness, ~starting_trt*ending_trt, at =list(days_since_first = 0))

# New leaf thickness data
hist(multipeq_data_light$leaf_thickness)
leaf_thickness <- lmer(leaf_thickness ~ starting_trt * ending_trt + (1|Chamber), 
                       data = subset(multipeq_data_light, New == 'N'))
plot(resid(leaf_thickness) ~ fitted(leaf_thickness))
summary(leaf_thickness)
Anova(leaf_thickness)
emmeans(leaf_thickness, ~starting_trt)
emmeans(leaf_thickness, ~ending_trt)
emmeans(leaf_thickness, ~starting_trt * ending_trt)
cld(emmeans(leaf_thickness, ~starting_trt * ending_trt))

# Old leaf SPAD data
hist(multipeq_data_light$SPAD) 
SPAD_lmer <- lmer(log(SPAD) ~ starting_trt * ending_trt * days_since_first + (1|Chamber)+ (1|Second.number), 
                  data = subset(multipeq_data_light, New == 'N'))
plot(resid(SPAD_lmer) ~ fitted(SPAD_lmer))
summary(SPAD_lmer)
Anova(SPAD_lmer)
emmeans(SPAD_lmer, ~starting_trt * ending_trt)
emmeans(SPAD_lmer, ~starting_trt)
emmeans(SPAD_lmer, ~ending_trt)
emtrends(SPAD_lmer, ~1, var = 'days_since_first')
emtrends(SPAD_lmer, ~starting_trt, var = 'days_since_first')
emtrends(SPAD_lmer, ~ending_trt, var = 'days_since_first')
emmeans(SPAD_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 0))

# New leaf SPAD data
hist(multipeq_data_light$SPAD) 
SPAD_lmer <- lmer(log(SPAD) ~ starting_trt * ending_trt + (1|Chamber), 
                  data = subset(multipeq_data_light, New == 'Y'))
plot(resid(SPAD_lmer) ~ fitted(SPAD_lmer))
summary(SPAD_lmer)
Anova(SPAD_lmer)
emmeans(SPAD_lmer, ~starting_trt)
emmeans(SPAD_lmer, ~ending_trt)
cld(emmeans(SPAD_lmer, ~starting_trt * ending_trt))

# Old leaf Phi2 data
hist(multipeq_data_light$Phi2) 
Phi2_lmer <- lmer(Phi2 ~ starting_trt * ending_trt * days_since_first + (1|Chamber) + (1|Second.number), 
                  data = subset(multipeq_data_light, New == 'N'))
plot(resid(Phi2_lmer) ~ fitted(Phi2_lmer))
summary(Phi2_lmer)
Anova(Phi2_lmer)
emmeans(Phi2_lmer, ~starting_trt * ending_trt)
emmeans(Phi2_lmer, ~ending_trt)
emtrends(Phi2_lmer, ~1, var = 'days_since_first')
emtrends(Phi2_lmer, ~starting_trt, var = 'days_since_first')
emtrends(Phi2_lmer, ~starting_trt * ending_trt, var = 'days_since_first')
emmeans(Phi2_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 0))

# New leaf Phi2 data
hist(multipeq_data_light$Phi2) 
Phi2_lmer <- lmer(Phi2 ~ starting_trt * ending_trt + (1|Chamber), 
                  data = subset(multipeq_data_light, New == 'Y'))
plot(resid(Phi2_lmer) ~ fitted(Phi2_lmer))
summary(Phi2_lmer)
Anova(Phi2_lmer)
emmeans(Phi2_lmer, ~starting_trt * ending_trt)
emmeans(Phi2_lmer, ~ending_trt)
cld(emmeans(Phi2_lmer, ~starting_trt * ending_trt))

# Old leaf qL data
hist(multipeq_data_light$qL) 
qL_lmer <- lmer(qL ~ starting_trt * ending_trt * days_since_first + (1|Chamber) + (1|Second.number), 
                  data = subset(multipeq_data_light, New == 'N'))
plot(resid(qL_lmer) ~ fitted(qL_lmer))
summary(qL_lmer)
Anova(qL_lmer)
emmeans(qL_lmer, ~starting_trt * ending_trt)
emmeans(qL_lmer, ~ending_trt)
emtrends(qL_lmer, ~1, var = 'days_since_first')
emtrends(qL_lmer, ~starting_trt, var = 'days_since_first')
emtrends(qL_lmer, ~starting_trt * ending_trt, var = 'days_since_first')
emmeans(qL_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 7))

# New leaf Phi2 data
hist(multipeq_data_light$qL) 
qL_lmer <- lmer(qL ~ starting_trt * ending_trt + (1|Chamber), 
                  data = subset(multipeq_data_light, New == 'Y'))
plot(resid(qL_lmer) ~ fitted(qL_lmer))
summary(qL_lmer)
Anova(qL_lmer)
emmeans(qL_lmer, ~starting_trt)
emmeans(qL_lmer, ~ending_trt)
emmeans(qL_lmer, ~starting_trt * ending_trt)


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
  mutate(julian_date = case_when(
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
licor_photo_data$date_multiyear <- licor_photo_data$julian_date

### Make sure columns are numeric 

if (!is.numeric(licor_photo_data$date_multiyear)) {
  licor_photo_data$date_multiyear <- as.numeric(as.character(licor_photo_data$date_multiyear))
}

if (!is.numeric(licor_photo_data$julian_date)) {
  licor_photo_data$julian_date <- as.numeric(as.character(licor_photo_data$julian_date))
}

### Account for new years

licor_photo_data$date_multiyear[licor_photo_data$julian_date < 300] <- 
  licor_photo_data$julian_date[licor_photo_data$julian_date < 300] + 365

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
    julian_date %in% c(352, 358, 360, 355, 361, 364, 359, 365, 2, 362, 3, 5, 1, 7, 9, 4, 10, 12) ~ "N",
    julian_date %in% c(11, 14, 16, 15, 17, 19, 18, 21) ~ "Y",
  ))

licor_photo_data$date <- as.Date(licor_photo_data$date, format = "%Y-%m-%d")

### Test data frame on old leaf jmax data
hist(licor_photo_data$jmax_tleaf) 
jmax_tleaf_lmer <- lmer(log(jmax_tleaf) ~ starting_trt * ending_trt * days_since_first+ 
                           (1|chamber) + (1|id), 
                   data = subset(licor_photo_data, New == 'N')) # this is the model setup to use for old leaves 
plot(resid(jmax_tleaf_lmer) ~ fitted(jmax_tleaf_lmer))
summary(jmax_tleaf_lmer)
Anova(jmax_tleaf_lmer)
emmeans(jmax_tleaf_lmer, ~starting_trt)
emmeans(jmax_tleaf_lmer, ~starting_trt * ending_trt)
emtrends(jmax_tleaf_lmer, ~1, var = 'days_since_first')
emmeans(jmax_tleaf_lmer, ~1, at = list(days_since_first = 0))
emtrends(jmax_tleaf_lmer, ~starting_trt, var = 'days_since_first')
emtrends(jmax_tleaf_lmer, ~ending_trt, var = 'days_since_first') 
emtrends(jmax_tleaf_lmer, ~starting_trt * ending_trt, var = 'days_since_first') # M
data.frame( ts_data <- cld(emmeans(jmax_tleaf_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 0)))) ### B

### Test data frame on new leaf jmax data
hist(licor_photo_data$jmax_tleaf) 
jmax_tleaf_lmer <- lmer(log(jmax_tleaf) ~ starting_trt * ending_trt + 
                          (1|chamber), 
                        data = subset(licor_photo_data, New == 'Y'))
plot(resid(jmax_tleaf_lmer) ~ fitted(jmax_tleaf_lmer))
summary(jmax_tleaf_lmer)
Anova(jmax_tleaf_lmer)
emmeans(jmax_tleaf_lmer, ~starting_trt)
emmeans(jmax_tleaf_lmer, ~ending_trt)
emmeans(jmax_tleaf_lmer, ~starting_trt * ending_trt)

### Test data frame on old leaf vcmax data
hist(licor_photo_data$vcmax_tleaf) 
vcmax_tleaf_lmer <- lmer(log(vcmax_tleaf) ~ starting_trt * ending_trt * days_since_first+ 
                          (1|chamber) + (1|id), 
                        data = subset(licor_photo_data, New == 'N')) 
plot(resid(vcmax_tleaf_lmer) ~ fitted(vcmax_tleaf_lmer))
summary(vcmax_tleaf_lmer)
Anova(vcmax_tleaf_lmer)
emmeans(vcmax_tleaf_lmer, ~starting_trt)
emmeans(vcmax_tleaf_lmer, ~ending_trt)
emmeans(vcmax_tleaf_lmer, ~ending_trt * starting_trt)
emtrends(vcmax_tleaf_lmer, ~1, var = 'days_since_first')
emmeans(vcmax_tleaf_lmer, ~1, at = list(days_since_first = 14))
emtrends(vcmax_tleaf_lmer, ~starting_trt, var = 'days_since_first')
emtrends(vcmax_tleaf_lmer, ~ending_trt, var = 'days_since_first') 
emtrends(vcmax_tleaf_lmer, ~starting_trt * ending_trt, var = 'days_since_first')
emmeans(vcmax_tleaf_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 7))

### Test data frame on new leaf vcmax data
vcmax_tleaf_lmer <- lmer(log(vcmax_tleaf) ~ starting_trt * ending_trt + 
                          (1|chamber), 
                        data = subset(licor_photo_data, New == 'Y')) 
plot(resid(vcmax_tleaf_lmer) ~ fitted(vcmax_tleaf_lmer))
summary(vcmax_tleaf_lmer)
Anova(vcmax_tleaf_lmer)
emmeans(vcmax_tleaf_lmer, ~starting_trt)
emmeans(vcmax_tleaf_lmer, ~ending_trt)
emmeans(vcmax_tleaf_lmer, ~starting_trt * ending_trt)


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
  mutate(julian_date = case_when(
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
licor_resp_data$date_multiyear <- licor_resp_data$julian_date

### Check to make sure the column is numeric 

if (!is.numeric(licor_resp_data$date_multiyear)) {
  licor_resp_data$date_multiyear <- as.numeric(as.character(licor_resp_data$date_multiyear))
}

if (!is.numeric(licor_resp_data$julian_date)) {
  licor_resp_data$julian_date <- as.numeric(as.character(licor_resp_data$julian_date))
}

licor_resp_data$date_multiyear[licor_resp_data$julian_date < 300] <- 
  licor_resp_data$julian_date[licor_resp_data$julian_date < 300] + 365

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
    julian_date %in% c(352, 358, 360, 355, 361, 364, 359, 365, 2, 362, 3, 5, 1, 7, 9, 4, 10, 12) ~ "N",
    julian_date %in% c(11, 14, 16, 15, 17, 19, 18, 21) ~ "Y",
  ))

licor_resp_data$date_multiyear <- licor_resp_data$julian_date


### Test data frame on old resp values

hist(licor_resp_data$A) 
resp_lmer <- lmer(A ~ starting_trt * ending_trt * days_since_first + (1|chamber)
                  + (1|id), 
                         data = subset(licor_resp_data, New == 'N'))
plot(resid(resp_lmer) ~ fitted(resp_lmer))
summary(resp_lmer)
Anova(resp_lmer)
emmeans(resp_lmer, ~starting_trt*ending_trt)
emmeans(resp_lmer, ~ending_trt)
emtrends(resp_lmer, ~1, var = 'days_since_first')
emtrends(resp_lmer, ~starting_trt, var = 'days_since_first')
emtrends(resp_lmer, ~ending_trt, var = 'days_since_first') 
emmeans(resp_lmer, ~starting_trt, at =list(days_since_first = 1))
emmeans(resp_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 1))
cld(emmeans(resp_lmer, ~starting_trt*ending_trt, at =list(days_since_first =4)))

### Test data frame on new resp data

hist(licor_resp_data$A) 
resp_lmer <- lmer(A ~ starting_trt * ending_trt + (1|chamber), 
                  data = subset(licor_resp_data, New == 'Y'))
plot(resid(resp_lmer) ~ fitted(resp_lmer))
summary(resp_lmer)
Anova(resp_lmer)
emmeans(resp_lmer, ~starting_trt)
emmeans(resp_lmer, ~ending_trt)
emmeans(resp_lmer, ~starting_trt * ending_trt)
cld(emmeans(resp_lmer, ~starting_trt*ending_trt))

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

struc_data <- struc_data %>%
  mutate(Chl_a_b_ratio = chlA.mmolm2 / chlB.mmolm2)

struc_data <- struc_data %>%
  mutate(
    focal_weight_g = as.numeric(focal_weight_g),
    chloro_weight_g = as.numeric(chloro_weight_g),
    total_area_focal_cm = as.numeric(total_area_focal_cm),
    total_area_chloro_cm = as.numeric(total_area_chloro_cm)
  )

# Calculate LMA for focal weight and area
struc_data <- struc_data %>%
  mutate(LMA_focal = focal_weight_g / total_area_focal_cm)

# Calculate LMA for chlorophyll weight and area
struc_data <- struc_data %>%
  mutate(LMA_chloro = chloro_weight_g / total_area_chloro_cm)

# Ensure LMA columns are numeric
struc_data <- struc_data %>%
  mutate(
    LMA_focal = as.numeric(LMA_focal),
    LMA_chloro = as.numeric(LMA_chloro)
  )

hc <- filter(struc_data, treatment == "hc")
lc <- filter(struc_data, treatment == "lc")
hl <- filter(struc_data, treatment == "hl")
lh <- filter(struc_data, treatment == "lh")

### Chlorophyll ab ratio in new leaves
ab_lmer <- lmer(Chl_a_b_ratio ~ starting_trt * ending_trt + 
                           (1|chamber), 
                         data = struc_data) 
plot(resid(ab_lmer) ~ fitted(ab_lmer))
summary(ab_lmer)
Anova(ab_lmer)
emmeans(ab_lmer, ~starting_trt)
emmeans(ab_lmer, ~ending_trt)
emmeans(ab_lmer, ~starting_trt * ending_trt)

### LMA in new leaves
lma_lmer <- lmer(log(LMA_focal) ~ starting_trt * ending_trt + 
                  (1|chamber), 
                data = struc_data) 
plot(resid(lma_lmer) ~ fitted(lma_lmer))
summary(lma_lmer)
Anova(lma_lmer)
emmeans(lma_lmer, ~starting_trt)
emmeans(lma_lmer, ~ending_trt)
emmeans(lma_lmer, ~starting_trt * ending_trt)

### Total chlorophyll in new leaves
total_lmer <- lmer(struc_data$total_area_chloro_cm ~ starting_trt * ending_trt + 
                   (1|chamber), 
                 data = struc_data)
plot(resid(total_lmer) ~ fitted(total_lmer))
summary(total_lmer)
Anova(total_lmer)
emmeans(total_lmer, ~starting_trt)
emmeans(total_lmer, ~ending_trt)
emmeans(total_lmer, ~starting_trt * ending_trt)

###Nutrient data
cn_data <- nutrient_data %>%
  extract(Sample_ID, into = c("chamber", "treatment", "first_number", "second_number"),
          regex = "([A-Za-z]+)_([A-Za-z]+)_(\\d)_(\\d+)", convert = TRUE, remove = FALSE) %>%
  mutate(across(c(chamber, treatment, first_number, second_number), as.character))

cn_data$starting_trt <- NA
cn_data$starting_trt[cn_data$treatment == 'lc' | cn_data$treatment == 'lh'] <- 'low'
cn_data$starting_trt[cn_data$treatment == 'hc' | cn_data$treatment == 'hl'] <- 'high'

cn_data$ending_trt <- NA
cn_data$ending_trt[cn_data$treatment == 'lc' | cn_data$treatment == 'hl'] <- 'low'
cn_data$ending_trt[cn_data$treatment == 'hc' | cn_data$treatment == 'lh'] <- 'high'

n_lmer <- lmer(Total_N ~ starting_trt * ending_trt + 
                     (1|chamber), 
                   data = cn_data) 
plot(resid(n_lmer) ~ fitted(n_lmer))
summary(n_lmer)
Anova(n_lmer)
emmeans(n_lmer, ~starting_trt)
emmeans(n_lmer, ~ending_trt)
emmeans(n_lmer, ~starting_trt * ending_trt)

c_lmer <- lmer(Total_C ~ starting_trt * ending_trt + 
                 (1|chamber), 
               data = cn_data) 
plot(resid(c_lmer) ~ fitted(n_lmer))
summary(c_lmer)
Anova(c_lmer)
emmeans(c_lmer, ~starting_trt)
emmeans(c_lmer, ~ending_trt)
emmeans(c_lmer, ~starting_trt * ending_trt)

cld(emmeans(ab_lmer, ~starting_trt*ending_trt))
Anova(c_lmer)
cld(emmeans(c_lmer, ~starting_trt*ending_trt))

### New leaf boxplots

###SPAD
ggplot(subset(multipeq_data_light, New == "Y"), 
       aes(x = factor(Treatment, levels = c("HC", "LH", "LC", "HL")), y = SPAD, fill = Treatment)) +
  geom_boxplot(size = 0.5) +
  labs(title = "New SPAD by treatment",
       x = "Treatment",
       y = "SPAD",
       fill = "") +
  theme_bw(base_size = 18) +
  theme(panel.border = element_rect(size = 1),  
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        panel.background = element_rect(fill = "white"),
        strip.background = element_blank(),  
        strip.text = element_text(size = 12)) +
  scale_fill_manual(values = c("HC" = "orangered4", "LH" = "lightcoral", "LC" = "royalblue4", "HL" = "skyblue2"))

###Fv'/Fm'
ggplot(subset(multipeq_data_light, New == "Y"), 
       aes(x = factor(Treatment, levels = c("HC", "LH", "LC", "HL")), y = FvP_over_FmP, fill = Treatment)) +
  geom_boxplot(size = 0.5) +
  labs(title = "New Fv'/Fm' by treatment",
       x = "Treatment",
       y = "Fv'/Fm'",
       fill = "") +
  theme_bw(base_size = 18) +
  theme(panel.border = element_rect(size = 1),  
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        panel.background = element_rect(fill = "white"),
        strip.background = element_blank(),  
        strip.text = element_text(size = 12)) +
  scale_fill_manual(values = c("HC" = "orangered4", "LH" = "lightcoral", "LC" = "royalblue4", "HL" = "skyblue2"))

###Phi2
ggplot(subset(multipeq_data_light, New == "Y"), 
       aes(x = factor(Treatment, levels = c("HC", "LH", "LC", "HL")), y = Phi2, fill = Treatment)) +
  geom_boxplot(size = 0.5) +
  labs(title = "New PhiPSII by treatment",
       x = "Treatment",
       y = "PhiPSII",
       fill = "") +
  theme_bw(base_size = 18) +
  theme(panel.border = element_rect(size = 1),  
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        panel.background = element_rect(fill = "white"),
        strip.background = element_blank(),  
        strip.text = element_text(size = 12)) +
  scale_fill_manual(values = c("HC" = "orangered4", "LH" = "lightcoral", "LC" = "royalblue4", "HL" = "skyblue2"))

###Phi2
ggplot(subset(multipeq_data_light, New == "Y"), 
       aes(x = factor(Treatment, levels = c("HC", "LH", "LC", "HL")), y = Phi2, fill = Treatment)) +
  geom_boxplot(size = 0.5) +
  labs(title = "New PhiPSII by treatment",
       x = "Treatment",
       y = "PhiPSII",
       fill = "") +
  theme_bw(base_size = 18) +
  theme(panel.border = element_rect(size = 1),  
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        panel.background = element_rect(fill = "white"),
        strip.background = element_blank(),  
        strip.text = element_text(size = 12)) +
  scale_fill_manual(values = c("HC" = "orangered4", "LH" = "lightcoral", "LC" = "royalblue4", "HL" = "skyblue2"))

###NPQt
ggplot(subset(multipeq_data_light, New == "Y"), 
       aes(x = factor(Treatment, levels = c("HC", "LH", "LC", "HL")), y = NPQt, fill = Treatment)) +
  geom_boxplot(size = 0.5) +
  labs(title = "New NPQt by treatment",
       x = "Treatment",
       y = "NPQt",
       fill = "") +
  theme_bw(base_size = 18) +
  theme(panel.border = element_rect(size = 1),  
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        panel.background = element_rect(fill = "white"),
        strip.background = element_blank(),  
        strip.text = element_text(size = 12)) +
  scale_fill_manual(values = c("HC" = "orangered4", "LH" = "lightcoral", "LC" = "royalblue4", "HL" = "skyblue2"))

###Leaf Thickness
ggplot(subset(multipeq_data_light, New == "Y"), 
       aes(x = factor(Treatment, levels = c("HC", "LH", "LC", "HL")), y = leaf_thickness, fill = Treatment)) +
  geom_boxplot(size = 0.5) +
  labs(title = "New Leaf Thickness by treatment",
       x = "Treatment",
       y = "Leaf Thickness",
       fill = "") +
  theme_bw(base_size = 18) +
  theme(panel.border = element_rect(size = 1),  
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        panel.background = element_rect(fill = "white"),
        strip.background = element_blank(),  
        strip.text = element_text(size = 12)) +
  scale_fill_manual(values = c("HC" = "orangered4", "LH" = "lightcoral", "LC" = "royalblue4", "HL" = "skyblue2"))

###chl content
ggplot(struc_data, 
       aes(x = factor(treatment, levels = c("hc", "lh", "lc", "hl")), y = chl_content.mmolm2, fill = treatment)) +
  geom_boxplot(size = 0.5) +
  labs(title = "New Leaf total chl content by treatment",
       x = "Treatment",
       y = "Total chl content",
       fill = "") +
  theme_bw(base_size = 18) +
  theme(panel.border = element_rect(size = 1),  
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        panel.background = element_rect(fill = "white"),
        strip.background = element_blank(),  
        strip.text = element_text(size = 12)) +
  scale_fill_manual(values = c("hc" = "orangered4", "lh" = "lightcoral", "lc" = "royalblue4", "hl" = "skyblue2"))

###LMA
ggplot(struc_data, 
       aes(x = factor(treatment, levels = c("hc", "lh", "lc", "hl")), y = LMA_focal, fill = treatment)) +
  geom_boxplot(size = 0.5) +
  labs(title = "",
       x = "Treatment",
       y = "Leaf mass per area (LMA)",
       fill = "") +
  theme_bw(base_size = 18) +
  theme(panel.border = element_rect(size = 1),  
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        panel.background = element_rect(fill = "white"),
        strip.background = element_blank(),  
        strip.text = element_text(size = 12)) +
  scale_fill_manual(values = c("hc" = "orangered4", "lh" = "lightcoral", "lc" = "royalblue4", "hl" = "skyblue2"))

###Vcmax
ggplot(subset(licor_photo_data, New == "Y"), 
       aes(x = factor(treatment, levels = c("hc", "lh", "lc", "hl")), y = vcmax_tleaf, fill = treatment)) +
  geom_boxplot(size = 0.5) +
  labs(title = "",
       x = "Treatment",
       y = expression(V[cmax] ~ "(" ~ mu ~ mol ~ m^-2 ~ s^-1 ~ ")"),
       fill = "") +
  theme_bw(base_size = 18) +
  theme(panel.border = element_rect(size = 1),  
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        panel.background = element_rect(fill = "white"),
        strip.background = element_blank(),  
        strip.text = element_text(size = 12)) +
  scale_fill_manual(values = c("hc" = "orangered4", "lh" = "lightcoral", "lc" = "royalblue4", "hl" = "skyblue2"))

###Jmax
ggplot(subset(licor_photo_data, New == "Y"), 
       aes(x = factor(treatment, levels = c("hc", "lh", "lc", "hl")), y = jmax_tleaf, fill = treatment)) +
  geom_boxplot(size = 0.5) +
  labs(title = "",
       x = "Treatment",
       y = expression(J[max] ~ "(" ~ mu ~ mol ~ m^-2 ~ s^-1 ~ ")"),
       fill = "") +
  theme_bw(base_size = 18) +
  theme(panel.border = element_rect(size = 1),  
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        panel.background = element_rect(fill = "white"),
        strip.background = element_blank(),  
        strip.text = element_text(size = 12)) +
  scale_fill_manual(values = c("hc" = "orangered4", "lh" = "lightcoral", "lc" = "royalblue4", "hl" = "skyblue2"))

###Resp
ggplot(subset(licor_resp_data, New == "Y"), 
       aes(x = factor(treatment, levels = c("hc", "lh", "lc", "hl")), y = abs(A), fill = treatment)) +
  geom_boxplot(size = 0.5) +
  labs(title = "",
       x = "Treatment",
       y = expression(R[d] ~ "(" ~ mu ~ mol ~ m^-2 ~ s^-1 ~ ")"),
       fill = "") +
  theme_bw(base_size = 18) +
  theme(panel.border = element_rect(size = 1),  
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        panel.background = element_rect(fill = "white"),
        strip.background = element_blank(),  
        strip.text = element_text(size = 12)) +
  scale_fill_manual(values = c("hc" = "orangered4", "lh" = "lightcoral", "lc" = "royalblue4", "hl" = "skyblue2"))

# Create groups to split graphs

licor_photo_data <- licor_photo_data %>%
  mutate(broad_group = case_when(
    treatment %in% c("hc", "lh") ~ "hc and lh",
    treatment %in% c("lc", "hl") ~ "lc and hl"))

licor_resp_data <- licor_resp_data %>%
  mutate(broad_group = case_when(
    treatment %in% c("hc", "lh") ~ "hc and lh",
    treatment %in% c("lc", "hl") ~ "lc and hl"))


### Vcmax est means plot
xtrend = seq(0, max(subset(licor_photo_data, New == 'N')$days_since_first), 1) # X

##HC
hc_y = -0.0496*xtrend + 2.26

###LH
lh_y = -0.0633*xtrend + 3.19

###HL
hl_y = -0.0551*xtrend + 2.75

###LC
lc_y =  -0.0689*xtrend + 3.46

trends <- as.data.frame(cbind(xtrend, hc_y, hl_y, lc_y, lh_y))

melted_data <- trends %>%
  pivot_longer(cols = starts_with("h") | starts_with("l"), 
               names_to = "treatment", 
               values_to = "value") 

melted_data <- trends %>%
  pivot_longer(cols = c(hc_y, hl_y, lc_y, lh_y), 
               names_to = "treatment", 
               values_to = "value")
# Define the treatment groups
melted_data <- melted_data %>%
  mutate(group = case_when(
    treatment == "hc_y" ~ "hc",
    treatment == "hl_y" ~ "hl",
    treatment == "lc_y" ~ "lc",
    treatment == "lh_y" ~ "lh"))

melted_data <- melted_data %>%
  mutate(broad_group = case_when(
    group %in% c("hc", "lh") ~ "hc and lh",
    group %in% c("hl", "lc") ~ "lc and hl"
  ))

ggplot() +
  geom_point(data = licor_photo_data %>% filter(New == "N"), 
             aes(x = days_since_first, y = (vcmax_tleaf), shape = treatment, color = treatment), 
             size = 1, alpha = 0.5) +
  geom_line(data = melted_data, aes( x = xtrend, y = exp(value), color = group), size = 1.2) +
  labs(title = "",
       x = "Days since baseline",
       y = expression(V[cmax] ~ "(" ~ mu ~ mol ~ m^-2 ~ s^-1 ~ ")"),
       shape = expression(V[cmax] ~ "data"),
       color = "Estimated means") +
  scale_shape_manual(values = c("hc" = 3, "lh" = 16, "lc" = 4, "hl" = 15)) +
  scale_color_manual(values = c("hc" = "orangered4", "lh" = "lightcoral", "lc" = "royalblue4", "hl" = "skyblue2")) +
  theme_bw(base_size = 18) +  
  theme(panel.border = element_rect(size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        strip.background = element_blank(),
        strip.text = element_text(size = 12),
        panel.spacing.x = unit(15, "pt"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.title = element_text(size = 14)) +
  facet_wrap(~ broad_group, scales = "fixed") 


### jmax est means plot

##HC
hc_y_j =  -0.0377*xtrend + 2.81

###LH
lh_y_j =  -0.0599*xtrend + 3.77

###HL
hl_y_j =  -0.0507*xtrend + 3.33

###LC
lc_y_j =  -0.0643*xtrend + 4.01

trends_j <- as.data.frame(cbind(xtrend, hc_y_j, hl_y_j, lc_y_j, lh_y_j))

melted_j_data <- trends_j %>%
  pivot_longer(cols = starts_with("h") | starts_with("l"), 
               names_to = "treatment", 
               values_to = "value") 

melted_j_data <- trends_j %>%
  pivot_longer(cols = c(hc_y_j, hl_y_j, lc_y_j, lh_y_j), 
               names_to = "treatment", 
               values_to = "value")
# Define the treatment groups
melted_j_data <- melted_j_data %>%
  mutate(group = case_when(
    treatment == "hc_y_j" ~ "hc",
    treatment == "hl_y_j" ~ "hl",
    treatment == "lc_y_j" ~ "lc",
    treatment == "lh_y_j" ~ "lh"))

melted_j_data <- melted_j_data %>%
  mutate(broad_group = case_when(
    group %in% c("hc", "lh") ~ "hc and lh",
    group %in% c("hl", "lc") ~ "lc and hl"
  ))

ggplot() +
  geom_point(data = licor_photo_data %>% filter(New == "N"), 
             aes(x = days_since_first, y = (jmax_tleaf), shape = treatment, color = treatment), 
             size = 1, alpha = 0.5) +
  geom_line(data = melted_j_data, aes( x = xtrend, y = exp(value), color = group), size = 1.2) +
  labs(title = "",
       x = "Days since baseline",
       y = expression(J[max] ~ "(" ~ mu ~ mol ~ m^-2 ~ s^-1 ~ ")"),
       shape = expression(J[max] ~ "data"),
       color = "Estimated means") +
  scale_shape_manual(values = c("hc" = 3, "lh" = 16, "lc" = 4, "hl" = 15)) +
  scale_color_manual(values = c("hc" = "orangered4", "lh" = "lightcoral", "lc" = "royalblue4", "hl" = "skyblue2")) +
  theme_bw(base_size = 18) +  
  theme(panel.border = element_rect(size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        strip.background = element_blank(),
        strip.text = element_text(size = 12),
        panel.spacing.x = unit(15, "pt"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.title = element_text(size = 14)) +
  facet_wrap(~ broad_group, scales = "fixed") 
#+
 # facet_wrap(~ broad_group, scales = "free_y")

### Resp est mean plot

##HC
hc_y_r = 0.0301*xtrend + -1.08

###LH
lh_y_r =   0.0331*xtrend + -1.17

###HL
hl_y_r =  0.0409*xtrend + -1.22

###LC
lc_y_r =   0.0409*xtrend + -1.27

trends_r <- as.data.frame(cbind(xtrend, hc_y_r, hl_y_r, lc_y_r, lh_y_r))

melted_r_data <- trends_r %>%
  pivot_longer(cols = starts_with("h") | starts_with("l"), 
               names_to = "treatment", 
               values_to = "value") 

melted_r_data <- trends_r %>%
  pivot_longer(cols = c(hc_y_r, hl_y_r, lc_y_r, lh_y_r), 
               names_to = "treatment", 
               values_to = "value")
# Define the treatment groups
melted_r_data <- melted_r_data %>%
  mutate(group = case_when(
    treatment == "hc_y_r" ~ "hc",
    treatment == "hl_y_r" ~ "hl",
    treatment == "lc_y_r" ~ "lc",
    treatment == "lh_y_r" ~ "lh"))

melted_r_data <- melted_r_data %>%
  mutate(broad_group = case_when(
    group %in% c("hc", "lh") ~ "hc and lh",
    group %in% c("hl", "lc") ~ "lc and hl"
  ))


ggplot() +
  geom_point(data = licor_resp_data %>% filter(New == "N", !is.na(broad_group)), 
             aes(x = days_since_first, y = abs(A), shape = treatment, color = treatment), 
             size = 1, alpha = 0.5) + 
  geom_line(data = melted_r_data %>% filter(!is.na(group)), 
            aes(x = xtrend, y = abs(value), color = group), size = 1.2) +
  labs(title = "",
       x = "Days since baseline",
       y = expression(R[d] ~ "(" ~ mu ~ mol ~ m^-2 ~ s^-1 ~ ")"),
       shape = expression(R[d] ~ "data"),
       color = "Estimated means") +
  scale_shape_manual(values = c("hc" = 3, "lh" = 16, "lc" = 4, "hl" = 15)) +
  scale_color_manual(values = c("hc" = "orangered4", "lh" = "lightcoral", "lc" = "royalblue4", "hl" = "skyblue2")) +
  theme_bw(base_size = 18) +  
  theme(panel.border = element_rect(size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        strip.background = element_blank(),
        strip.text = element_text(size = 12),
        panel.spacing.x = unit(15, "pt"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.title = element_text(size = 14)) +
  facet_wrap(~ broad_group, scales = "fixed")


### SPAD est means plot
xtrend_fluro = seq(0, max(17)) # X

##HC
hc_y_s =  -0.02645*xtrend + 3.78

###LH
lh_y_s =    -0.01852*xtrend + 3.75

###HL
hl_y_s =     -0.00892*xtrend + 3.69

###LC
lc_y_s =     -0.00366*xtrend + 3.66

trends_s <- as.data.frame(cbind(xtrend_fluro, hc_y_s, hl_y_s, lc_y_s, lh_y_s))

melted_s_data <- trends_s %>%
  pivot_longer(cols = starts_with("h") | starts_with("l"), 
               names_to = "Treatment", 
               values_to = "value") 

melted_s_data <- trends_s %>%
  pivot_longer(cols = c(hc_y_s, hl_y_s, lc_y_s, lh_y_s), 
               names_to = "Treatment", 
               values_to = "value")
# Define the treatment groups
melted_s_data <- melted_s_data %>%
  mutate(group = case_when(
    Treatment == "hc_y_s" ~ "HC",
    Treatment == "hl_y_s" ~ "HL",
    Treatment == "lc_y_s" ~ "LC",
    Treatment == "lh_y_s" ~ "LH"))

melted_s_data <- melted_s_data %>%
  mutate(broad_group = case_when(
    group %in% c("HC", "LH") ~ "HC and LH",
    group %in% c("HL", "LC") ~ "LC and HL"
  ))

multipeq_data_light <- multipeq_data_light %>%
  mutate(broad_group = case_when(
    Treatment %in% c("HC", "LH") ~ "HC and LH",
    Treatment %in% c("HL", "LC") ~ "LC and HL"
  ))


ggplot() +
  geom_point(data = multipeq_data_light %>% filter(New == "N"), 
             aes(x = days_since_first, y = (SPAD), shape = Treatment, color = Treatment), 
             size = 1, alpha = 0.5) +
  geom_line(data = melted_s_data, aes( x = xtrend_fluro, y = exp(value), color = group), size = 1.2) +
  labs(title = "SPAD in old leaves over time",
       x = "Days since baseline",
       y = "SPAD",
       shape = "SPAD data",
       color = "Estimated means") +
  scale_shape_manual(values = c("HC" = 3, "LH" = 16, "LC" = 4, "HL" = 15)) +
  scale_color_manual(values = c("HC" = "orangered4", "LH" = "lightcoral", "LC" = "royalblue4", "HL" = "skyblue2")) +
  theme_bw(base_size = 18) +  
  theme(panel.border = element_rect(size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        strip.background = element_blank(),
        strip.text = element_text(size = 12),
        panel.spacing.x = unit(15, "pt"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.title = element_text(size = 14)) +
  facet_wrap(~ broad_group, scales = "fixed")

## NPQt
##HC
xtrend_npq = seq(0, max(17)) # X


hc_y_npq =     0.611*xtrend_npq + 3.97

###LH
lh_y_npq =     0.178*xtrend_npq + 2.75

###HL
hl_y_npq =      0.244*xtrend_npq + 1.07

###LC
lc_y_npq =     0.192*xtrend_npq + 0.31

trends_npq <- as.data.frame(cbind(xtrend_npq, hc_y_npq, hl_y_npq, lc_y_npq, lh_y_npq))

melted_npq_data <- trends_npq %>%
  pivot_longer(cols = starts_with("h") | starts_with("l"), 
               names_to = "Treatment", 
               values_to = "value") 

melted_npq_data <- trends_npq %>%
  pivot_longer(cols = c(hc_y_npq, hl_y_npq, lc_y_npq, lh_y_npq), 
               names_to = "Treatment", 
               values_to = "value")
# Define the treatment groups
melted_npq_data <- melted_npq_data %>%
  mutate(group = case_when(
    Treatment == "hc_y_npq" ~ "HC",
    Treatment == "hl_y_npq" ~ "HL",
    Treatment == "lc_y_npq" ~ "LC",
    Treatment == "lh_y_npq" ~ "LH"))

melted_npq_data <- melted_npq_data %>%
  mutate(broad_group = case_when(
    group %in% c("HC", "LH") ~ "HC and LH",
    group %in% c("HL", "LC") ~ "LC and HL"
  ))


ggplot() +
  geom_point(data = multipeq_data_light %>% filter(New == "N" & days_since_first <= max(xtrend_npq)), 
             aes(x = days_since_first, y = (NPQt), shape = Treatment, color = Treatment), 
             size = 1, alpha = 0.5) +
  geom_line(data = melted_npq_data, aes( x = xtrend_npq, y = value, color = group), size = 1.2) +
  labs(title = "                        NPQt in old leaves over time",
       x = "Days since baseline",
       y = "NPQt",
       shape = "NPQt data",
       color = "Estimated means") +
  scale_shape_manual(values = c("HC" = 3, "LH" = 16, "LC" = 4, "HL" = 15)) +
  scale_color_manual(values = c("HC" = "orangered4", "LH" = "lightcoral", "LC" = "royalblue4", "HL" = "skyblue2")) +
  theme_bw(base_size = 18) +  
  theme(panel.border = element_rect(size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        strip.background = element_blank(),
        strip.text = element_text(size = 12),
        panel.spacing.x = unit(15, "pt"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.title = element_text(size = 14))  +
  facet_wrap(~ broad_group, scales = "fixed")

### FvP est means plot
xtrend_fluro = seq(0, max(17)) # X

##HC
hc_y_qe =    -0.0114*xtrend_fluro + 0.472

###LH
lh_y_qe =    -0.0124*xtrend_fluro + 0.601

###HL
hl_y_qe =     -0.0112*xtrend_fluro +  0.705

###LC
lc_y_qe =     -0.0138*xtrend_fluro + 0.799

trends_qe <- as.data.frame(cbind(xtrend_fluro, hc_y_qe, hl_y_qe, lc_y_qe, lh_y_qe))

melted_qe_data <- trends_qe %>%
  pivot_longer(cols = starts_with("h") | starts_with("l"), 
               names_to = "Treatment", 
               values_to = "value") 

melted_qe_data <- trends_qe %>%
  pivot_longer(cols = c(hc_y_qe, hl_y_qe, lc_y_qe, lh_y_qe), 
               names_to = "Treatment", 
               values_to = "value")
# Define the treatment groups
melted_qe_data <- melted_qe_data %>%
  mutate(group = case_when(
    Treatment == "hc_y_qe" ~ "HC",
    Treatment == "hl_y_qe" ~ "HL",
    Treatment == "lc_y_qe" ~ "LC",
    Treatment == "lh_y_qe" ~ "LH"))

melted_qe_data <- melted_qe_data %>%
  mutate(broad_group = case_when(
    group %in% c("HC", "LH") ~ "HC and LH",
    group %in% c("HL", "LC") ~ "LC and HL"
  ))


ggplot() +
  geom_point(data = multipeq_data_light %>% filter(New == "N" & days_since_first <= max(xtrend_fluro)), 
             aes(x = days_since_first, y = (FvP_over_FmP), shape = Treatment, color = Treatment), 
             size = 1, alpha = 0.5) +
  geom_line(data = melted_qe_data, aes( x = xtrend_fluro, y = value, color = group), size = 1.2) +
  labs(title = "                        Fv'/Fm' in old leaves over time",
       x = "Days since baseline",
       y = "Fv'/Fm'",
       shape = "Fv'/Fm' data",
       color = "Estimated means") +
  scale_shape_manual(values = c("HC" = 3, "LH" = 16, "LC" = 4, "HL" = 15)) +
  scale_color_manual(values = c("HC" = "orangered4", "LH" = "lightcoral", "LC" = "royalblue4", "HL" = "skyblue2")) +
  theme_bw(base_size = 18) +  
  theme(panel.border = element_rect(size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        strip.background = element_blank(),
        strip.text = element_text(size = 12),
        panel.spacing.x = unit(15, "pt"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.title = element_text(size = 14))  +
  facet_wrap(~ broad_group, scales = "fixed")

### Phi2 est means plot
xtrend_fluro = seq(0, max(17)) # X

##HC
hc_y_p2 =    -0.00258*xtrend_fluro + 0.0915

###LH
lh_y_p2 =    -0.00822*xtrend_fluro + 0.1681

###HL
hl_y_p2 =     -0.00584*xtrend_fluro +  0.1550

###LC
lc_y_p2 =     -0.01208*xtrend_fluro + 0.2743

trends_p2 <- as.data.frame(cbind(xtrend_fluro, hc_y_p2, hl_y_p2, lc_y_p2, lh_y_p2))

melted_p2_data <- trends_p2 %>%
  pivot_longer(cols = starts_with("h") | starts_with("l"), 
               names_to = "Treatment", 
               values_to = "value") 

melted_p2_data <- trends_p2 %>%
  pivot_longer(cols = c(hc_y_p2, hl_y_p2, lc_y_p2, lh_y_p2), 
               names_to = "Treatment", 
               values_to = "value")
# Define the treatment groups
melted_p2_data <- melted_p2_data %>%
  mutate(group = case_when(
    Treatment == "hc_y_p2" ~ "HC",
    Treatment == "hl_y_p2" ~ "HL",
    Treatment == "lc_y_p2" ~ "LC",
    Treatment == "lh_y_p2" ~ "LH"))

melted_p2_data <- melted_p2_data %>%
  mutate(broad_group = case_when(
    group %in% c("HC", "LH") ~ "HC and LH",
    group %in% c("HL", "LC") ~ "LC and HL"
  ))

ggplot() +
  geom_point(data = multipeq_data_light %>% filter(New == "N" & days_since_first <= max(xtrend_fluro)), 
             aes(x = days_since_first, y = (Phi2), shape = Treatment, color = Treatment), 
             size = 1, alpha = 0.5) +
  geom_line(data = melted_p2_data, aes( x = xtrend_fluro, y = value, color = group), size = 1.2) +
  labs(title = "                        Phi2 in old leaves over time",
       x = "Days since baseline",
       y = "PhiPSII",
       shape = "PhiPSII data",
       color = "Estimated means") +
  scale_shape_manual(values = c("HC" = 3, "LH" = 16, "LC" = 4, "HL" = 15)) +
  scale_color_manual(values = c("HC" = "orangered4", "LH" = "lightcoral", "LC" = "royalblue4", "HL" = "skyblue2")) +
  theme_bw(base_size = 18) +  
  theme(panel.border = element_rect(size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        strip.background = element_blank(),
        strip.text = element_text(size = 12),
        panel.spacing.x = unit(15, "pt"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.title = element_text(size = 14))  +
  facet_wrap(~ broad_group, scales = "fixed")

### Leaf thickness est means plot
xtrend_fluro = seq(0, max(17)) # X

##HC
hc_y_lt =     -0.00807*xtrend_fluro + 0.476

###LH
lh_y_lt =     -0.00922*xtrend_fluro +  0.477

###HL
hl_y_lt =     -0.00719*xtrend_fluro + 0.438

###LC
lc_y_lt =     -0.00642*xtrend_fluro + 0.449

trends_lt <- as.data.frame(cbind(xtrend_fluro, hc_y_lt, hl_y_lt, lc_y_lt, lh_y_lt))

melted_lt_data <- trends_lt %>%
  pivot_longer(cols = starts_with("h") | starts_with("l"), 
               names_to = "Treatment", 
               values_to = "value") 

melted_lt_data <- trends_lt %>%
  pivot_longer(cols = c(hc_y_lt, hl_y_lt, lc_y_lt, lh_y_lt), 
               names_to = "Treatment", 
               values_to = "value")
# Define the treatment groups
melted_lt_data <- melted_lt_data %>%
  mutate(group = case_when(
    Treatment == "hc_y_lt" ~ "HC",
    Treatment == "hl_y_lt" ~ "HL",
    Treatment == "lc_y_lt" ~ "LC",
    Treatment == "lh_y_lt" ~ "LH"))

melted_lt_data <- melted_lt_data %>%
  mutate(broad_group = case_when(
    group %in% c("HC", "LH") ~ "HC and LH",
    group %in% c("HL", "LC") ~ "LC and HL"
  ))

ggplot() +
  geom_point(data = multipeq_data_light %>% filter(New == "N" & days_since_first <= max(xtrend_fluro)), 
             aes(x = days_since_first, y = (leaf_thickness), shape = Treatment, color = Treatment), 
             size = 1, alpha = 0.5) +
  geom_line(data = melted_lt_data, aes( x = xtrend_fluro, y = value, color = group), size = 1.2) +
  labs(title = "                        Leaf thickness in old leaves over time",
       x = "Days since baseline",
       y = "Leaf Thickness",
       shape = "Leaf Thickness data",
       color = "Estimated means") +
  scale_shape_manual(values = c("HC" = 3, "LH" = 16, "LC" = 4, "HL" = 15)) +
  scale_color_manual(values = c("HC" = "orangered4", "LH" = "lightcoral", "LC" = "royalblue4", "HL" = "skyblue2")) +
  theme_bw(base_size = 18) +  
  theme(panel.border = element_rect(size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        strip.background = element_blank(),
        strip.text = element_text(size = 12),
        panel.spacing.x = unit(15, "pt"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.title = element_text(size = 14))  +
  facet_wrap(~ broad_group, scales = "fixed")

### Good plot one-panel
ggplot(subset(licor_photo_data, New == "N"), aes(x = days_since_first, y = vcmax_tleaf)) +
  geom_point(aes(shape = treatment), size = 2, alpha = 0.8) +  # Larger points with some transparency
  geom_smooth(aes(color = treatment), method = "lm", se = FALSE, size = 1.5) +  # Trend lines with confidence intervals
  labs(title = "Old leaf Vcmax by date across all treatments",
       x = "Days since Baseline",
       y = "Vcmax at 25°C",
       shape = "Treatment",
       color = "Treatment") +
  scale_shape_manual(values = c("hc" = 15, "lh" = 16, "lc" = 17, "hl" = 18)) +  # Custom shapes for treatments
  scale_color_manual(values = c("hc" = "orangered4", "lh" = "lightcoral", "lc" = "royalblue4", "hl" = "skyblue2")) +  # Custom colors for lines
  theme_bw(base_size = 18) +  
  theme(panel.border = element_rect(size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        strip.background = element_blank(),
        strip.text = element_text(size = 12),
        panel.spacing.x = unit(15, "pt"),
        axis.text.x = (element_text(size = 12)),
        axis.text.y = (element_text(size = 12)))

### Plot for trends with Multispeq data
ggplot(subset(multipeq_data_light, New == "N"), aes(x = days_since_first, y = NPQt)) +
  geom_point(aes(shape = Treatment), size = 2, alpha = 0.8) +  
  geom_smooth(aes(color = Treatment), method = "loess", se = TRUE, size = 1.5) +  # Trend lines with confidence intervals
  labs(title = "Old leaf SPAD by date across all treatments",
       x = "Date",
       y = "SPAD",
       shape = "Treatment",
       color = "Treatment") +
  scale_shape_manual(values = c("HC" = 15, "LH" = 16, "LC" = 17, "HL" = 18)) +  # Custom shapes for treatments
  scale_color_manual(values = c("HC" = "orangered4", "LH" = "lightcoral", "LC" = "royalblue4", "HL" = "skyblue2")) +  # Custom colors for lines
  theme_bw(base_size = 18) +  
  theme(panel.border = element_rect(size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        strip.background = element_blank(),
        strip.text = element_text(size = 12),
        panel.spacing.x = unit(15, "pt"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12)) +
  facet_wrap(~ Treatment, scales = "free_y")


### Plotting Phi2,PhiNO, PhiNPQ not combined
multipeq_data_long <- pivot_longer(
  subset(multipeq_data_light, New == "N"),
  cols = c(Phi2, PhiNPQ),
  names_to = "Variable",
  values_to = "Value"
)

ggplot(multipeq_data_long, aes(x = days_since_first, y = Value)) +
  geom_point(aes(shape = Treatment), size = 2, alpha = 0.8) +
  geom_smooth(aes(color = Treatment), method = "loess", se = TRUE, size = 1.5) +
  labs(title = "Old leaf allocation of incoming light to different processes",
       x = "Days Since First",
       y = "Value",
       shape = "Treatment",
       color = "Treatment") +
  scale_shape_manual(values = c("HC" = 15, "LH" = 16, "LC" = 17, "HL" = 18)) +
  scale_color_manual(values = c("HC" = "orangered4", "LH" = "lightcoral", "LC" = "royalblue4", "HL" = "skyblue2")) +
  theme_bw(base_size = 18) +
  theme(panel.border = element_rect(size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        strip.background = element_blank(),
        strip.text = element_text(size = 12),
        panel.spacing.x = unit(15, "pt"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.background = element_rect(fill = "transparent")) +
  facet_grid(Variable ~ Treatment, scales = "free_y")


### Plotting Phi2,PhiNO, PhiNPQ combined by LC/HL and HC/LH
multipeq_data_long$trt_group <- ifelse(multipeq_data_long$Treatment %in% c("HC", "LH"), "HC and LH", "LC and HL")

multipeq_data_long <- multipeq_data_long %>%
  mutate(Variable = factor(Variable, levels = c("Phi2", "PhiNPQ")))

ggplot(multipeq_data_long, aes(x = days_since_first, y = Value)) +
  geom_point(aes(shape = Treatment), size = 1, alpha = 0.5) +
  geom_smooth(aes(color = Treatment), method = "loess", se = TRUE, size = 1.5) +
  labs(title = "Old leaf allocation of incoming light to different processes",
       x = "Days Since First",
       y = "",
       shape = "Treatment",
       color = "Treatment") +
  scale_shape_manual(values = c("HC" = 15, "LH" = 16, "LC" = 17, "HL" = 18)) +
  scale_color_manual(values = c("HC" = "orangered4", "LH" = "lightcoral", "LC" = "royalblue4", "HL" = "skyblue2")) +
  theme_bw(base_size = 18) +
  theme(panel.border = element_rect(size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        strip.background = element_blank(),
        strip.text = element_text(size = 12),
        panel.spacing.x = unit(15, "pt"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.background = element_rect(fill = "transparent")) +
  facet_grid(Variable ~ trt_group, scales = "free_y")

###SPAD

ggplot(subset(multipeq_data_light, New == "N"), aes(x = days_since_first, y = NPQt)) +
  geom_point(aes(shape = Treatment), size = 1, alpha = 0.5) +
  geom_smooth(aes(color = Treatment), method = "loess", se = TRUE, size = 1.5) +
  labs(title = "Old leaf qL",
       x = "Days Since First",
       y = "qL",
       shape = "Treatment",
       color = "Treatment") +
  scale_shape_manual(values = c("HC" = 15, "LH" = 16, "LC" = 17, "HL" = 18)) +
  scale_color_manual(values = c("HC" = "orangered4", "LH" = "lightcoral", "LC" = "royalblue4", "HL" = "skyblue2")) +
  theme_bw(base_size = 18) +
  theme(panel.border = element_rect(size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        strip.background = element_blank(),
        strip.text = element_text(size = 12),
        panel.spacing.x = unit(15, "pt"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.background = element_rect(fill = "transparent")) +
  facet_grid(scales = "free_y")


### Plot for trends with Licor data
ggplot(subset(licor_photo_data, New == "N"), aes(x = days_since_first, y = vcmax_tleaf)) +
  geom_point(aes(shape = treatment), size = 2, alpha = 0.8) +  
  geom_smooth(aes(color = treatment), method = "loess", se = TRUE, size = 1.5) +  # Trend lines with confidence intervals
  labs(title = "Old leaf vcmax by date across all treatments",
       x = "Days since first",
       y = "Vcmax at 25°C",
       shape = "Treatment",
       color = "Treatment") +
  scale_shape_manual(values = c("hc" = 15, "lh" = 16, "lc" = 17, "hl" = 18)) +  # Custom shapes for treatments
  scale_color_manual(values = c("hc" = "orangered4", "lh" = "lightcoral", "lc" = "royalblue4", "hl" = "skyblue2")) +  # Custom colors for lines
  theme_bw(base_size = 18) +  
  theme(panel.border = element_rect(size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        strip.background = element_blank(),
        strip.text = element_text(size = 12),
        panel.spacing.x = unit(15, "pt"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12)) +
  facet_wrap(~ treatment, scales = "free_y")

licor_photo_data$trt_group <- ifelse(licor_photo_data$treatment %in% c("hc", "lh"), "hc and lh", "lc and hl")


ggplot(subset(licor_photo_data, New == "N"), aes(x = days_since_first, y = vcmax_tleaf)) +
  geom_point(aes(shape = treatment), size = 1, alpha = 0.5) +
  geom_smooth(aes(color = treatment), method = "loess", se = TRUE, size = 1.5) +
  labs(title = "Old leaf jmax",
       x = "Days Since First",
       y = "vcmax",
       shape = "Treatment",
       color = "Treatment") +
  scale_shape_manual(values = c("hc" = 15, "lh" = 16, "lc" = 17, "hl" = 18)) +
  scale_color_manual(values = c("hc" = "orangered4", "lh" = "lightcoral", "lc" = "royalblue4", "hl" = "skyblue2")) +
  theme_bw(base_size = 18) +
  theme(panel.border = element_rect(size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        strip.background = element_blank(),
        strip.text = element_text(size = 12),
        panel.spacing.x = unit(15, "pt"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.background = element_rect(fill = "transparent")) +
  facet_grid(~ trt_group, scales = "free_y")

###Trying to use emtrends

ggplot(subset(licor_photo_data, New == "N"), aes(x = days_since_first, y = vcmax_tleaf)) +
  geom_point(aes(shape = treatment), size = 1, alpha = 0.5) +
  labs(title = "Old leaf jmax",
       x = "Days Since First",
       y = "vcmax",
       shape = "Treatment",
       color = "Treatment") +
  scale_shape_manual(values = c("hc" = 15, "lh" = 16, "lc" = 17, "hl" = 18)) +
  scale_color_manual(values = c("hc" = "orangered4", "lh" = "lightcoral", "lc" = "royalblue4", "hl" = "skyblue2")) +
  theme_bw(base_size = 18) +
  theme(panel.border = element_rect(size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        strip.background = element_blank(),
        strip.text = element_text(size = 12),
        panel.spacing.x = unit(15, "pt"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.background = element_rect(fill = "transparent")) +
  facet_grid(~ trt_group, scales = "free_y")

### Test resp data

licor_resp_data$trt_group <- ifelse(licor_resp_data$treatment %in% c("hc", "lh"), "hc and lh", "lc and hl")

ggplot(subset(licor_resp_data, New == "N"), aes(x = days_since_first, y = A)) +
  geom_point(aes(shape = treatment), size = 1, alpha = 0.5) +
  geom_smooth(aes(color = treatment), method = "loess", se = TRUE, size = 1.5) +
  labs(title = "",
       x = "Days Since First",
       y = "Resp",
       shape = "Treatment",
       color = "Treatment") +
  scale_shape_manual(values = c("hc" = 15, "lh" = 16, "lc" = 17, "hl" = 18)) +
  scale_color_manual(values = c("hc" = "orangered4", "lh" = "lightcoral", "lc" = "royalblue4", "hl" = "skyblue2")) +
  theme_bw(base_size = 18) +
  theme(panel.border = element_rect(size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        strip.background = element_blank(),
        strip.text = element_text(size = 12),
        panel.spacing.x = unit(15, "pt"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.background = element_rect(fill = "transparent")) +
  facet_grid(~ trt_group, scales = "free_y")

### Test struc data
ggplot2(struc_data) aes(x = treatment, y = vcmax_tleaf) +
  geom_point(aes(shape = treatment), size = 2, alpha = 0.8) +  
  geom_smooth(aes(color = treatment), method = "loess", se = TRUE, size = 1.5) +  # Trend lines with confidence intervals
  labs(title = "Old leaf vcmax by date across all treatments",
       x = "Days since first",
       y = "Vcmax at 25°C",
       shape = "Treatment",
       color = "Treatment") +
  scale_shape_manual(values = c("hc" = 15, "lh" = 16, "lc" = 17, "hl" = 18)) +  # Custom shapes for treatments
  scale_color_manual(values = c("hc" = "orangered4", "lh" = "lightcoral", "lc" = "royalblue4", "hl" = "skyblue2")) +  # Custom colors for lines
  theme_bw(base_size = 18) +  
  theme(panel.border = element_rect(size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        strip.background = element_blank(),
        strip.text = element_text(size = 12),
        panel.spacing.x = unit(15, "pt"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12)) +
  facet_wrap(~ treatment, scales = "free_y")
