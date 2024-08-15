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

## load data
multipeq_data <- read.csv('../../data/multispeq/multi_speq_data_cleaned.csv')
licor_photo_data <- read.csv('../../data/aci_output/all_curve_fits.csv')
licor_resp_data <- read.csv('../../data/licor/licor_cleaned/tsrd_merged_all.csv')
struc_data <- read.csv('../../data/Structual/ts_structural_data.csv')

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

hist(multipeq_data_light$SPAD) 
SPAD_lmer <- lmer((log(SPAD)) ~ starting_trt * ending_trt * days_since_first + 
                           (1|Chamber) + (1|Second.number), 
                         data = subset(multipeq_data_light, New == 'N')) # this is the model setup to use for old leaves (ngs)
plot(resid(SPAD_lmer) ~ fitted(SPAD_lmer))
summary(SPAD_lmer)
Anova(SPAD_lmer)
emmeans(SPAD_lmer, ~starting_trt)
emmeans(SPAD_lmer, ~ending_trt)
emtrends(SPAD_lmer, ~1, var = 'days_since_first')
emmeans(SPAD_lmer, ~1, at = list(days_since_first = 0))
emtrends(SPAD_lmer, ~starting_trt, var = 'days_since_first')
emtrends(SPAD_lmer, ~ending_trt, var = 'days_since_first') 
emtrends(SPAD_lmer, ~starting_trt * ending_trt, var = 'days_since_first') # M
data.frame( tsm_data <- cld(emmeans(SPAD_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 0)))) ### B
cld(emmeans(qL_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 15)))
emmeans(SPAD_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 10))

xtrend = seq(0, max(subset(licor_photo_data, New == 'N')$days_since_first), 1) # X

##HC
hc_y_s =  -0.02645*xtrend + 3.78

###LH
lh_y_s =    -0.01852*xtrend + 3.75

###HL
hl_y_s =     -0.00892*xtrend + 3.69

###LC
lc_y_s =     -0.00366*xtrend + 3.66

trends_s <- as.data.frame(cbind(xtrend, hc_y_s, hl_y_s, lc_y_s, lh_y_s))

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

data.frame( tsm_data0 <- cld(emmeans(SPAD_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 0)))) ### B
tsm_data0 <- tsm_data0 %>%
  mutate(days_since_first = 0)
data.frame( tsm_data1 <- cld(emmeans(SPAD_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 1)))) ### B
tsm_data1 <- tsm_data1 %>%
  mutate(days_since_first = 1)
data.frame( tsm_data2 <- cld(emmeans(SPAD_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 2)))) ### B
tsm_data2 <- tsm_data2 %>%
  mutate(days_since_first = 2)
data.frame( tsm_data3 <- cld(emmeans(SPAD_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 3)))) ### B
tsm_data3 <- tsm_data3 %>%
  mutate(days_since_first = 3)
data.frame( tsm_data4 <- cld(emmeans(SPAD_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 4)))) ### B
tsm_data4 <- tsm_data4 %>%
  mutate(days_since_first = 4)
data.frame( tsm_data5 <- cld(emmeans(SPAD_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 5)))) ### B
tsm_data5 <- tsm_data5 %>%
  mutate(days_since_first = 5)
data.frame( tsm_data6 <- cld(emmeans(SPAD_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 6)))) ### B
tsm_data6 <- tsm_data6 %>%
  mutate(days_since_first = 6)
data.frame( tsm_data7 <- cld(emmeans(SPAD_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 7)))) ### B
tsm_data7 <- tsm_data7 %>%
  mutate(days_since_first = 7)
data.frame( tsm_data8 <- cld(emmeans(SPAD_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 8)))) ### B
tsm_data8 <- tsm_data8 %>%
  mutate(days_since_first = 8)
data.frame( tsm_data9 <- cld(emmeans(SPAD_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 9)))) ### B
tsm_data9 <- tsm_data9 %>%
  mutate(days_since_first = 9)
data.frame( tsm_data10 <- cld(emmeans(SPAD_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 10)))) ### B
tsm_data10 <- tsm_data10 %>%
  mutate(days_since_first = 10)
data.frame( tsm_data11 <- cld(emmeans(SPAD_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 11)))) ### B
tsm_data11 <- tsm_data11 %>%
  mutate(days_since_first = 11)
data.frame( tsm_data12 <- cld(emmeans(SPAD_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 12)))) ### B
tsm_data12 <- tsm_data12 %>%
  mutate(days_since_first = 12)
data.frame( tsm_data13 <- cld(emmeans(SPAD_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 13)))) ### B
tsm_data13 <- tsm_data13 %>%
  mutate(days_since_first = 13)
data.frame( tsm_data14 <- cld(emmeans(SPAD_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 14)))) ### B
tsm_data14 <- tsm_data14 %>%
  mutate(days_since_first = 14)
data.frame( tsm_data15 <- cld(emmeans(SPAD_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 15)))) ### B
tsm_data15 <- tsm_data15 %>%
  mutate(days_since_first = 15)
data.frame( tsm_data16 <- cld(emmeans(SPAD_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 16)))) ### B
tsm_data16 <- tsm_data16 %>%
  mutate(days_since_first = 16)
data.frame( tsm_data17 <- cld(emmeans(SPAD_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 17)))) ### B
tsm_data17 <- tsm_data17 %>%
  mutate(days_since_first = 17)

tsm_data_all <- rbind(tsm_data0, tsm_data1, tsm_data2, tsm_data3, tsm_data4, 
                     tsm_data5, tsm_data6, tsm_data7, tsm_data8, tsm_data9, 
                     tsm_data10, tsm_data11, tsm_data12, tsm_data13, tsm_data14, 
                     tsm_data15, tsm_data16, tsm_data17)

tsm_data_all <- tsm_data_all %>%
  mutate(treatment = case_when(
    starting_trt == "low" & ending_trt == "low" ~ "lc",
    starting_trt == "high" & ending_trt == "high" ~ "hc",
    starting_trt == "low" & ending_trt == "high" ~ "lh",
    starting_trt == "high" & ending_trt == "low" ~ "hl"
  ))

ggplot(tsm_data_all, aes(x = days_since_first, y = .group, color = treatment)) +
  geom_line() +
  geom_point(size = 2, alpha = 0.8) +
  labs(title = "Change in FvP_over_FmP group",
       x = "Days Since First",
       y = "group",
       color = "Treatment") +
  theme_bw(base_size = 18) +
  theme(panel.border = element_rect(size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        strip.background = element_blank(),
        strip.text = element_text(size = 12),
        panel.spacing.x = unit(15, "pt"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12))

### question 1: How does non photo chemical quenching change overtime in the old
###  leaf?
hist(multipeq_data_light$NPQt) # take a look at the dark acclimated NPQt data.
NPQt_lmer <- lmer(log(NPQt) ~ starting_trt * ending_trt * days_since_first + (1|Chamber), 
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

### question 3(b): How does SPAD change overtime in the new
###  leaf?
hist(multipeq_data_light$SPAD) # take a look at the dark acclimated FvP_over_FmP data.
Phi2_lmer <- lmer(SPAD ~ starting_trt * ending_trt + (1|Chamber), 
                  data = subset(multipeq_data_light, New == 'Y'))
plot(resid(Phi2_lmer) ~ fitted(Phi2_lmer))
summary(Phi2_lmer)
Anova(Phi2_lmer)
emmeans(Phi2_lmer, ~starting_trt)
emmeans(Phi2_lmer, ~ending_trt)
emtrends(Phi2_lmer, ~1, var = 'days_since_first')
emtrends(Phi2_lmer, ~starting_trt, var = 'days_since_first')
emtrends(Phi2_lmer, ~ending_trt, var = 'days_since_first')
emmeans(Phi2_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 18))
emmeans(Phi2_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 21))

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

### Test data frame on vcmax data

hist(licor_photo_data$jmax_tleaf) 
jmax_tleaf_lmer <- lmer(log(jmax_tleaf) ~ starting_trt * ending_trt * days_since_first+ 
                           (1|chamber) + (1|id), 
                   data = subset(licor_photo_data, New == 'N')) # this is the model setup to use for old leaves (ngs)
plot(resid(jmax_tleaf_lmer) ~ fitted(jmax_tleaf_lmer))
summary(vcmax_tleaf_lmer)
Anova(jmax_tleaf_lmer)
emmeans(jmax_tleaf_lmer, ~starting_trt)
emmeans(jmax_tleaf_lmer, ~ending_trt)
emtrends(jmax_tleaf_lmer, ~1, var = 'days_since_first')
emmeans(jmax_tleaf_lmer, ~1, at = list(days_since_first = 0))
emtrends(jmax_tleaf_lmer, ~starting_trt, var = 'days_since_first')
emtrends(jmax_tleaf_lmer, ~ending_trt, var = 'days_since_first') 
emtrends(jmax_tleaf_lmer, ~starting_trt * ending_trt, var = 'days_since_first') # M
data.frame( ts_data <- cld(emmeans(vcmax_tleaf_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 14)))) ### B
new_leaf <- emmeans(vcmax_tleaf_lmer, ~starting_trt*ending_trt)
emmeans(vcmax_tleaf_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 10))


new_leaf <- cld(new_leaf, Letters = letters)

ts_data0 <- emmeans(vcmax_tleaf_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 0))
ts_data0 <- cld(ts_data0, Letter = letters)
ts_data0 <- ts_data0 %>%
  mutate(days_since_first = 0)
ts_data1 <- emmeans(vcmax_tleaf_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 1))
ts_data1 <- cld(ts_data1, Letter = letters)
ts_data1 <- ts_data1 %>%
  mutate(days_since_first = 1)
ts_data2 <- emmeans(vcmax_tleaf_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 2))
ts_data2 <- cld(ts_data2, Letter = letters)
ts_data2 <- ts_data2 %>%
  mutate(days_since_first = 2)
ts_data3 <- emmeans(vcmax_tleaf_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 3))
ts_data3 <- cld(ts_data3, Letter = letters)
ts_data3 <- ts_data3 %>%
  mutate(days_since_first = 3)
ts_data4 <- emmeans(vcmax_tleaf_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 4))
ts_data4 <- cld(ts_data4, Letter = letters)
ts_data4 <- ts_data4 %>%
  mutate(days_since_first = 4)
ts_data5 <- emmeans(vcmax_tleaf_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 5))
ts_data5 <- cld(ts_data5, Letter = letters)
ts_data5 <- ts_data5 %>%
  mutate(days_since_first = 5)
ts_data6 <- emmeans(vcmax_tleaf_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 6))
ts_data6 <- cld(ts_data6, Letter = letters)
ts_data6 <- ts_data6 %>%
  mutate(days_since_first = 6)
ts_data7 <- emmeans(vcmax_tleaf_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 7))
ts_data7 <- cld(ts_data7, Letter = letters)
ts_data7 <- ts_data7 %>%
  mutate(days_since_first = 7)
ts_data8 <- emmeans(vcmax_tleaf_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 8))
ts_data8 <- cld(ts_data8, Letter = letters)
ts_data8 <- ts_data8 %>%
  mutate(days_since_first = 8)
ts_data9 <- emmeans(vcmax_tleaf_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 9))
ts_data9 <- cld(ts_data9, Letter = letters)
ts_data9 <- ts_data9 %>%
  mutate(days_since_first = 9)
ts_data10 <- emmeans(vcmax_tleaf_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 10))
ts_data10 <- cld(ts_data10, Letter = letters)
ts_data10 <- ts_data10 %>%
  mutate(days_since_first = 10)
ts_data11 <- emmeans(vcmax_tleaf_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 11))
ts_data11 <- cld(ts_data11, Letter = letters)
ts_data11 <- ts_data11 %>%
  mutate(days_since_first = 11)
ts_data12 <- emmeans(vcmax_tleaf_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 12))
ts_data12 <- cld(ts_data12, Letter = letters)
ts_data12 <- ts_data12 %>%
  mutate(days_since_first = 12)
ts_data13 <- emmeans(vcmax_tleaf_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 13))
ts_data13 <- cld(ts_data13, Letter = letters)
ts_data13 <- ts_data13 %>%
  mutate(days_since_first = 13)
ts_data14 <- emmeans(vcmax_tleaf_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 14))
ts_data14 <- cld(ts_data14, Letter = letters)
ts_data14 <- ts_data14 %>%
  mutate(days_since_first = 14)
ts_data15 <- emmeans(vcmax_tleaf_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 15))
ts_data15 <- cld(ts_data15, Letter = letters)
ts_data15 <- ts_data15 %>%
  mutate(days_since_first = 15)
ts_data16 <- emmeans(vcmax_tleaf_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 16))
ts_data16 <- cld(ts_data16, Letter = letters)
ts_data16 <- ts_data16 %>%
  mutate(days_since_first = 16)
ts_data17 <- emmeans(vcmax_tleaf_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 17))
ts_data17 <- cld(ts_data17, Letter = letters)
ts_data17 <- ts_data17 %>%
  mutate(days_since_first = 17)


ts_data_all <- rbind(ts_data0, ts_data1, ts_data2, ts_data3, ts_data4, 
                       ts_data5, ts_data6, ts_data7, ts_data8, ts_data9, 
                       ts_data10, ts_data11, ts_data12, ts_data13, ts_data14, 
                       ts_data15, ts_data16, ts_data17)

ts_data17 <- ts_data17 %>%
  mutate(treatment = case_when(
    starting_trt == "low" & ending_trt == "low" ~ "lc",
    starting_trt == "high" & ending_trt == "high" ~ "hc",
    starting_trt == "low" & ending_trt == "high" ~ "lh",
    starting_trt == "high" & ending_trt == "low" ~ "hl"
  ))

write.csv(ts_data_all, "ts_data_all.csv")

ts_data_all <- ts_data_all %>%
  mutate(days_since_first = as.factor(days_since_first))

filtered_data <- ts_data_all %>%
  filter(treatment %in% c("lc", "lh")) %>%
  mutate(days_since_first = as.factor(days_since_first))


# Create the boxplot with confidence intervals
ggplot(ts_data_all, days_since_first == 0) 
       aes(x = treatment, y = emmean, fill = treatment) +
  geom_boxplot(alpha = 0.6) +
  labs(title = "Boxplot of Estimated Marginal Means with Confidence Intervals",
       x = "Treatment",
       y = "Estimated Marginal Means (emmeans)",
       fill = "Treatment") +
  theme_minimal()

plot1 <- ts_data_all %>%
  filter(days_since_first == 0 ) %>%
  ggplot(aes(x = treatment, y = emmean, fill = treatment)) +
  geom_boxplot(alpha = 0.6) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2, position = position_dodge(0.75)) +
  geom_point(position = position_dodge(0.75), size = 2) +
  labs(title = "Boxplot of Estimated Marginal Means with Confidence Intervals",
       x = "Treatment",
       y = "Estimated Marginal Means (emmeans)",
       fill = "Treatment") +
  theme_minimal()

print(plot)
library(multcompView)
multcompView::multcompBoxplot()

multcompView::multcompBoxplot(ts_data_all)

ggplot(ts_data12, aes(x = days_since_first, y = emmean, color = treatment)) +
  geom_line() +
  geom_point(size = 2, alpha = 0.8) +
  labs(title = "Change in emmean Over Time by Treatment",
       x = "Days Since First",
       y = "emmean",
       color = "Treatment") +
  theme_bw(base_size = 18) +
  theme(panel.border = element_rect(size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        strip.background = element_blank(),
        strip.text = element_text(size = 12),
        panel.spacing.x = unit(15, "pt"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12))

# Display the plot

write.csv(ts_data_all, file = "ts_data_all.csv")
## testing out creating data frames for plotting lines

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

vcmax_tleaf_lmer <- lmer(log(vcmax_tleaf) ~ starting_trt * ending_trt + 
                           (1|chamber) + (1|id), 
                         data = subset(licor_photo_data, New == 'Y')) # this is the model setup to use for old leaves (ngs)
plot(resid(vcmax_tleaf_lmer) ~ fitted(vcmax_tleaf_lmer))
summary(vcmax_tleaf_lmer)
Anova(vcmax_tleaf_lmer)
emmeans(vcmax_tleaf_lmer, ~starting_trt*ending_trt)
emmeans(vcmax_tleaf_lmer, ~ending_trt)

### Test data frame on jmax data

hist(licor_photo_data$jmax_tleaf) 
jmax_tleaf_lmer <- lmer(log(jmax_tleaf) ~ starting_trt * ending_trt * days_since_first + 
                           (1|chamber) + (1|id), 
                         data = subset(licor_photo_data, New == 'N')) # this is the model setup to use for old leaves (ngs)
plot(resid(jmax_tleaf_lmer) ~ fitted(jmax_tleaf_lmer))
summary(jmax_tleaf_lmer)
Anova(jmax_tleaf_lmer)
emmeans(jmax_tleaf_lmer, ~starting_trt)
emmeans(jmax_tleaf_lmer, ~ending_trt)
emtrends(jmax_tleaf_lmer, ~1, var = 'days_since_first')
emmeans(jmax_tleaf_lmer, ~1, at = list(days_since_first = 0))
emtrends(jmax_tleaf_lmer, ~starting_trt, var = 'days_since_first')
emtrends(jmax_tleaf_lmer, ~ending_trt, var = 'days_since_first') 
emtrends(jmax_tleaf_lmer, ~starting_trt * ending_trt, var = 'days_since_first') # M
data.frame( ts_data <- cld(emmeans(jmax_tleaf_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 0)))) ### B
cld(emmeans(jmax_tleaf_lmer, ~starting_trt*ending_trt, at =list(days_since_first =17)))
emmeans(jmax_tleaf_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 10))

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

data.frame( tsj_data0 <- cld(emmeans(jmax_tleaf_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 0)))) ### B
tsj_data0 <- tsj_data0 %>%
  mutate(days_since_first = 0)
data.frame( tsj_data1 <- cld(emmeans(jmax_tleaf_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 1)))) ### B
tsj_data1 <- tsj_data1 %>%
  mutate(days_since_first = 1)
data.frame( tsj_data2 <- cld(emmeans(jmax_tleaf_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 2)))) ### B
tsj_data2 <- tsj_data2 %>%
  mutate(days_since_first = 2)
data.frame( tsj_data3 <- cld(emmeans(jmax_tleaf_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 3)))) ### B
tsj_data3 <- tsj_data3 %>%
  mutate(days_since_first = 3)
data.frame( tsj_data4 <- cld(emmeans(jmax_tleaf_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 4)))) ### B
tsj_data4 <- tsj_data4 %>%
  mutate(days_since_first = 4)
data.frame( tsj_data5 <- cld(emmeans(jmax_tleaf_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 5)))) ### B
tsj_data5 <- tsj_data5 %>%
  mutate(days_since_first = 5)
data.frame( tsj_data6 <- cld(emmeans(jmax_tleaf_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 6)))) ### B
tsj_data6 <- tsj_data6 %>%
  mutate(days_since_first = 6)
data.frame( tsj_data7 <- cld(emmeans(jmax_tleaf_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 7)))) ### B
tsj_data7 <- tsj_data7 %>%
  mutate(days_since_first = 7)
data.frame( tsj_data8 <- cld(emmeans(jmax_tleaf_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 8)))) ### B
tsj_data8 <- tsj_data8 %>%
  mutate(days_since_first = 8)
data.frame( tsj_data9 <- cld(emmeans(jmax_tleaf_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 9)))) ### B
tsj_data9 <- tsj_data9 %>%
  mutate(days_since_first = 9)
data.frame( tsj_data10 <- cld(emmeans(jmax_tleaf_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 10)))) ### B
tsj_data10 <- tsj_data10 %>%
  mutate(days_since_first = 10)
data.frame( tsj_data11 <- cld(emmeans(jmax_tleaf_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 11)))) ### B
tsj_data11 <- tsj_data11 %>%
  mutate(days_since_first = 11)
data.frame( tsj_data12 <- cld(emmeans(jmax_tleaf_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 12)))) ### B
tsj_data12 <- tsj_data12 %>%
  mutate(days_since_first = 12)
data.frame( tsj_data13 <- cld(emmeans(jmax_tleaf_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 13)))) ### B
tsj_data13 <- tsj_data13 %>%
  mutate(days_since_first = 13)
data.frame( tsj_data14 <- cld(emmeans(jmax_tleaf_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 14)))) ### B
tsj_data14 <- tsj_data14 %>%
  mutate(days_since_first = 14)
data.frame( tsj_data15 <- cld(emmeans(jmax_tleaf_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 15)))) ### B
tsj_data15 <- tsj_data15 %>%
  mutate(days_since_first = 15)
data.frame( tsj_data16 <- cld(emmeans(jmax_tleaf_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 16)))) ### B
tsj_data16 <- tsj_data16 %>%
  mutate(days_since_first = 16)
data.frame( tsj_data17 <- cld(emmeans(jmax_tleaf_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 17)))) ### B
tsj_data17 <- tsj_data17 %>%
  mutate(days_since_first = 17)

tsj_data_all <- rbind(tsj_data0, tsj_data1, tsj_data2, tsj_data3, tsj_data4, 
                     tsj_data5, tsj_data6, tsj_data7, tsj_data8, tsj_data9, 
                     tsj_data10, tsj_data11, tsj_data12, tsj_data13, tsj_data14, 
                     tsj_data15, tsj_data16, tsj_data17)

tsj_data_all <- tsj_data_all %>%
  mutate(treatment = case_when(
    starting_trt == "low" & ending_trt == "low" ~ "lc",
    starting_trt == "high" & ending_trt == "high" ~ "hc",
    starting_trt == "low" & ending_trt == "high" ~ "lh",
    starting_trt == "high" & ending_trt == "low" ~ "hl"
  ))


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


### Test data frame on A values

hist(licor_resp_data$A) 
resp_lmer <- lmer(A ~ starting_trt * ending_trt + (1|chamber)
                  + (1|id), 
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

vcmax_tleaf_lmer <- lmer(log(vcmax_tleaf) ~ starting_trt * ending_trt + 
                           (1|chamber) + (1|id), 
                         data = subset(licor_photo_data, New == 'Y')) # this is the model setup to use for old leaves (ngs)
plot(resid(vcmax_tleaf_lmer) ~ fitted(vcmax_tleaf_lmer))
summary(vcmax_tleaf_lmer)
Anova(vcmax_tleaf_lmer)
emmeans(vcmax_tleaf_lmer, ~starting_trt*ending_trt)
emmeans(vcmax_tleaf_lmer, ~ending_trt)


### Test data frame on resp data

hist(licor_resp_data$A) 
resp_lmer <- lmer((A) ~ starting_trt * ending_trt * days_since_first + 
                          (1|chamber) + (1|id), 
                        data = subset(licor_resp_data, New == 'N')) # this is the model setup to use for old leaves (ngs)
plot(resid(resp_lmer) ~ fitted(resp_lmer))
summary(resp_lmer)
Anova(resp_lmer)
emmeans(resp_lmer, ~starting_trt)
emmeans(resp_lmer, ~ending_trt)
emtrends(resp_lmer, ~1, var = 'days_since_first')
emmeans(resp_lmer, ~1, at = list(days_since_first = 0))
emtrends(resp_lmer, ~starting_trt, var = 'days_since_first')
emtrends(resp_lmer, ~ending_trt, var = 'days_since_first') 
emtrends(resp_lmer, ~starting_trt * ending_trt, var = 'days_since_first') # M
data.frame( ts_data <- cld(emmeans(resp_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 0)))) ### B
cld(emmeans(resp_lmer, ~starting_trt*ending_trt, at =list(days_since_first =4)))
emmeans(resp_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 10))

xtrend = seq(0, max(subset(licor_photo_data, New == 'N')$days_since_first), 1) # X

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

data.frame( tsj_data0 <- cld(emmeans(resp_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 0)))) ### B
tsj_data0 <- tsj_data0 %>%
  mutate(days_since_first = 0)
data.frame( tsj_data1 <- cld(emmeans(resp_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 1)))) ### B
tsj_data1 <- tsj_data1 %>%
  mutate(days_since_first = 1)
data.frame( tsj_data2 <- cld(emmeans(resp_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 2)))) ### B
tsj_data2 <- tsj_data2 %>%
  mutate(days_since_first = 2)
data.frame( tsj_data3 <- cld(emmeans(resp_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 3)))) ### B
tsj_data3 <- tsj_data3 %>%
  mutate(days_since_first = 3)
data.frame( tsj_data4 <- cld(emmeans(resp_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 4)))) ### B
tsj_data4 <- tsj_data4 %>%
  mutate(days_since_first = 4)
data.frame( tsj_data5 <- cld(emmeans(resp_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 5)))) ### B
tsj_data5 <- tsj_data5 %>%
  mutate(days_since_first = 5)
data.frame( tsj_data6 <- cld(emmeans(resp_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 6)))) ### B
tsj_data6 <- tsj_data6 %>%
  mutate(days_since_first = 6)
data.frame( tsj_data7 <- cld(emmeans(resp_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 7)))) ### B
tsj_data7 <- tsj_data7 %>%
  mutate(days_since_first = 7)
data.frame( tsj_data8 <- cld(emmeans(resp_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 8)))) ### B
tsj_data8 <- tsj_data8 %>%
  mutate(days_since_first = 8)
data.frame( tsj_data9 <- cld(emmeans(resp_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 9)))) ### B
tsj_data9 <- tsj_data9 %>%
  mutate(days_since_first = 9)
data.frame( tsj_data10 <- cld(emmeans(resp_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 10)))) ### B
tsj_data10 <- tsj_data10 %>%
  mutate(days_since_first = 10)
data.frame( tsj_data11 <- cld(emmeans(resp_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 11)))) ### B
tsj_data11 <- tsj_data11 %>%
  mutate(days_since_first = 11)
data.frame( tsj_data12 <- cld(emmeans(resp_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 12)))) ### B
tsj_data12 <- tsj_data12 %>%
  mutate(days_since_first = 12)
data.frame( tsj_data13 <- cld(emmeans(resp_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 13)))) ### B
tsj_data13 <- tsj_data13 %>%
  mutate(days_since_first = 13)
data.frame( tsj_data14 <- cld(emmeans(resp_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 14)))) ### B
tsj_data14 <- tsj_data14 %>%
  mutate(days_since_first = 14)
data.frame( tsj_data15 <- cld(emmeans(resp_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 15)))) ### B
tsj_data15 <- tsj_data15 %>%
  mutate(days_since_first = 15)
data.frame( tsj_data16 <- cld(emmeans(resp_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 16)))) ### B
tsj_data16 <- tsj_data16 %>%
  mutate(days_since_first = 16)
data.frame( tsj_data17 <- cld(emmeans(resp_lmer, ~starting_trt*ending_trt, at =list(days_since_first = 17)))) ### B
tsj_data17 <- tsj_data17 %>%
  mutate(days_since_first = 17)

tsj_data_all <- rbind(tsj_data0, tsj_data1, tsj_data2, tsj_data3, tsj_data4, 
                      tsj_data5, tsj_data6, tsj_data7, tsj_data8, tsj_data9, 
                      tsj_data10, tsj_data11, tsj_data12, tsj_data13, tsj_data14, 
                      tsj_data15, tsj_data16, tsj_data17)

tsj_data_all <- tsj_data_all %>%
  mutate(treatment = case_when(
    starting_trt == "low" & ending_trt == "low" ~ "lc",
    starting_trt == "high" & ending_trt == "high" ~ "hc",
    starting_trt == "low" & ending_trt == "high" ~ "lh",
    starting_trt == "high" & ending_trt == "low" ~ "hl"
  ))

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

### Test boxplot

###SPAD

ggplot(subset(multipeq_data_light, New == "Y"), aes(x = Treatment, y = SPAD, fill = Treatment)) +
  geom_boxplot (size = 0.5) +
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

###Vcmax

ggplot(subset(licor_photo_data, New == "Y"), aes(x = treatment, y = vcmax_tleaf, fill = treatment)) +
  geom_boxplot (size = 0.5) +
  labs(title = "",
       x = "Treatment",
       y = "Vcmax (25°C)",
       fill = "Treatment") +
  theme_bw(base_size = 18) +
  theme(panel.border = element_rect(size = 1),  
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        panel.background = element_rect(fill = "white"),
        strip.background = element_blank(),  
        strip.text = element_text(size = 12)) +
  scale_fill_manual(values = c("hc" = "orangered4", "lh" = "lightcoral", "lc" = "royalblue4", "hl" = "skyblue2"))



vcmax0 <- ggplot(subset(licor_photo_data, days_since_first == 0 & New == "N"), 
                aes(x = treatment, y = vcmax_tleaf, fill = treatment)) +
  geom_boxplot(size = 0.5) +
  labs(title = "",
       x = "Day 0",
       y = "Vcmax (25°C)",
       fill = "") +
  theme_bw(base_size = 18) +
  theme(panel.border = element_rect(size = 1),  
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        panel.background = element_rect(fill = "white"),
        strip.background = element_blank(),  
        strip.text = element_text(size = 12),
        legend.position = "none") +
  scale_fill_manual(values = c("hc" = "orangered4", "lh" = "lightcoral", "lc" = "royalblue4", "hl" = "skyblue2"))

vcmax14 <- ggplot(subset(licor_photo_data, days_since_first == 14 & New == "N"), 
                 aes(x = treatment, y = vcmax_tleaf, fill = treatment)) +
  geom_boxplot(size = 0.5) +
  labs(title = "",
       x = "Day 14",
       y = "",
       fill = "") +
  theme_bw(base_size = 18) +
  theme(panel.border = element_rect(size = 1),  
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        panel.background = element_rect(fill = "white"),
        strip.background = element_blank(),  
        strip.text = element_text(size = 12),
        legend.position = "none") +
  scale_fill_manual(values = c("hc" = "orangered4", "lh" = "lightcoral", "lc" = "royalblue4", "hl" = "skyblue2"))

# Combine the plots using patchwork
combined_plot <- vcmax0 + vcmax14 + plot_layout(nrow = 1)
plot(combined_plot)

licor_photo_data$treatment <- factor(licor_photo_data$treatment, levels = c("hc", "lh", "lc", "hl"))

# Create the first plot for day 0
vcmax0 <- ggplot(subset(licor_photo_data, days_since_first == 0 & New == "N"), 
                 aes(x = treatment, y = vcmax_tleaf, fill = treatment)) +
  geom_boxplot(size = 0.5) +
  labs(title = "",
       x = "Day 0",
       y = "Vcmax (25°C)",
       fill = "") +
  theme_bw(base_size = 18) +
  theme(panel.border = element_rect(size = 1),  
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        panel.background = element_rect(fill = "white"),
        strip.background = element_blank(),  
        strip.text = element_text(size = 12),
        legend.position = "none") +
        ylim(0,89) +
  scale_fill_manual(values = c("hc" = "orangered4", "lh" = "lightcoral", "lc" = "royalblue4", "hl" = "skyblue2"))


# Create the second plot for day 14
vcmax14 <- ggplot(subset(licor_photo_data, days_since_first == 14 & New == "N"), 
                  aes(x = treatment, y = vcmax_tleaf, fill = treatment)) +
  geom_boxplot(size = 0.5) +
  labs(title = "",
       x = "Day 14",
       y = "",
       fill = "") +
  theme_bw(base_size = 18) +
  theme(panel.border = element_rect(size = 1),  
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        panel.background = element_rect(fill = "white"),
        strip.background = element_blank(),  
        strip.text = element_text(size = 12),
        legend.position = "none") +
        ylim(0,89) +
  scale_fill_manual(values = c("hc" = "orangered4", "lh" = "lightcoral", "lc" = "royalblue4", "hl" = "skyblue2"))



# Combine the plots using patchwork
combined_plot <- vcmax0 + vcmax14 + plot_layout(nrow = 1)

# Display the combined plot
print(combined_plot)
# Plot for New == "N"
vcmax_new_N <- ggplot(subset(licor_photo_data, New == "N"), aes(x = treatment, y = vcmax_tleaf, fill = treatment)) +
  geom_boxplot(size = 0.5) +
  labs(title = "",
       x = "Old leaf",
       y = "vcmax",
       fill = "") +
  theme_bw(base_size = 18) +
  theme(panel.border = element_rect(size = 1),  
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        panel.background = element_rect(fill = "white"),
        strip.background = element_blank(),  
        strip.text = element_text(size = 12),
        legend.position = "none") +
  scale_fill_manual(values = c("hc" = "orangered4", "lh" = "lightcoral", "lc" = "royalblue4", "hl" = "skyblue2"))

# Plot for New == "Y"
vcmax_new_Y <- ggplot(subset(licor_photo_data, New == "Y"), aes(x = treatment, y = vcmax_tleaf, fill = treatment)) +
  geom_boxplot(size = 0.5) +
  labs(title = "",
       x = "New leaf",
       y = "",
       fill = "") +
  theme_bw(base_size = 18) +
  theme(panel.border = element_rect(size = 1),  
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        strip.background = element_blank(),  
        strip.text = element_text(size = 12)) +
  scale_fill_manual(values = c("hc" = "orangered4", "lh" = "lightcoral", "lc" = "royalblue4", "hl" = "skyblue2"))

combined_plot <- vcmax_new_N + vcmax_new_Y + plot_layout(ncol = 2, widths = c(1, 1))

# Display the combined plot
print(combined_plot)

### Good plot multi-panel
ggplot(subset(licor_photo_data, New == "N"), aes(x = days_since_first, y = vcmax_tleaf)) +
  geom_point(aes(shape = treatment), size = 2, alpha = 0.8) +  # Larger points with some transparency
  geom_smooth(aes(color = treatment), method = "lm", se = TRUE, size = 1.5) +  # Trend lines with confidence intervals
  labs(title = "Vcmax by date across all treatments",
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
        axis.text.y = (element_text(size = 12))) +
  facet_wrap(~ treatment, scales = "free_y") 

### Trends

write_csv(hl_trend, file = "hl_trend.csv")
write_csv(licor_photo_data, file = "licor_photo_data.csv")

ggplot(subset(licor_photo_data, New == "N"), aes(x = days_since_first, y = log(vcmax_tleaf))) +
  geom_point(aes(shape = treatment), size = 2, alpha = 0.8) +  # Larger points with some transparency
  labs(title = "Old leaf Vcmax by date across all treatments",
       x = "Days since Baseline",
       y = "Vcmax at 25°C",
       shape = "Treatment",
       color = "Treatment") +
  scale_shape_manual(values = c("hc" = 15, "lh" = 16, "lc" = 17, "hl" = 18)) +  # Custom shapes for treatments
  scale_color_manual(values = c("hc" = "orangered4", "lh" = "lightcoral", "lc" = "royalblue4", "hl" = "skyblue2")) +  # Custom colors for lines
  geom_line(data = hc_trend, aes(x = xtrend, y = hc_y), color = "orangered4", size = 1.2) +  # hc trend line
  geom_line(data = hl_trend, aes(x = xtrend, y = hl_y), color = "skyblue2", size = 1.2) +  # hl trend line
  geom_line(data = lc_trend, aes(x = xtrend, y = lc_y), color = "royalblue4", size = 1.2) +  # lc trend line
  geom_line(data = lh_trend, aes(x = xtrend, y = lh_y), color = "lightcoral", size = 1.2) +  # lh trend line
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

write.csv(trends, file = "trends.csv")
 
# Plotting with ggplot

licor_photo_data <- licor_photo_data %>%
  mutate(broad_group = case_when(
    treatment %in% c("hc", "hl") ~ "hc and hl",
    treatment %in% c("lh", "lc") ~ "lc and lh"))


### Vcmax est means plot
ggplot() +
  geom_point(data = licor_photo_data %>% filter(New == "N"), 
             aes(x = days_since_first, y = (vcmax_tleaf), shape = treatment, color = treatment), 
             size = 1, alpha = 0.5) +
  geom_line(data = melted_data, aes( x = xtrend, y = exp(value), color = group), size = 1.2) +
  labs(title = "      Vcmax in old leaves over time",
       x = "Days since baseline",
       y = "Vcmax (25°C)",
       shape = "Vcmax data",
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
  facet_wrap(~ broad_group, scales = "free_y")

### jmax est means plot
ggplot() +
  geom_point(data = licor_photo_data %>% filter(New == "N"), 
             aes(x = days_since_first, y = (jmax_tleaf), shape = treatment, color = treatment), 
             size = 1, alpha = 0.5) +
  geom_line(data = melted_j_data, aes( x = xtrend, y = exp(value), color = group), size = 1.2) +
  labs(title = "      Jmax in old leaves over time",
       x = "Days since baseline",
       y = "jmax (25°C)",
       shape = "jmax data",
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
  facet_wrap(~ broad_group, scales = "free_y")

### Resp est mean plot

ggplot() +
  geom_point(data = licor_resp_data %>% filter(New == "N"), 
             aes(x = days_since_first, y = (A), shape = treatment, color = treatment), 
             size = 1, alpha = 0.5) +
  geom_line(data = melted_r_data, aes( x = xtrend, y = exp(value), color = group), size = 1.2) +
  labs(title = "      Resp in old leaves over time",
       x = "Days since baseline",
       y = "A",
       shape = "Resp data",
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
  facet_wrap(~ broad_group, scales = "free_y")

### SPAD est means plot
ggplot() +
  geom_point(data = multipeq_data_light %>% filter(New == "N"), 
             aes(x = days_since_first, y = (SPAD), shape = Treatment, color = Treatment), 
             size = 1, alpha = 0.5) +
  geom_line(data = melted_s_data, aes( x = xtrend, y = exp(value), color = group), size = 1.2) +
  labs(title = "      Vcmax in old leaves over time",
       x = "Days since baseline",
       y = "Vcmax (25°C)",
       shape = "Vcmax data",
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
  facet_wrap(~ broad_group, scales = "free_y")


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
ggplot(subset(multipeq_data_light, New == "N"), aes(x = days_since_first, y = SPAD)) +
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
  cols = c(Phi2, PhiNO, PhiNPQ),
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
  mutate(Variable = factor(Variable, levels = c("Phi2", "PhiNPQ", "PhiNO")))

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

ggplot(subset(multipeq_data_light, New == "N"), aes(x = days_since_first, y = qL)) +
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
  labs(title = "Old leaf resp",
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

# Extended data to show a gradual change
time <- c(0, 1,2, 3, 4, 5, 6, 7, 8, 9, 10, 11,12,13,14,15,16)  # More days from start
A <- c(14, 14.1, 14.1, 13, 11.4, 10.3, 10, 9.9, 10, 10.8, 12, 13, 13.6,14,14.1,14.1,14.13)  # Net photosynthesis values with gradual decrease and increase

# Creating a data frame
df <- data.frame(Time = time, A = A)

# Plotting
ggplot(df, aes(x = Time, y = A)) +
  geom_line() +
  geom_point() +  # Add points at data points
  labs(x = "Time", y = "Net Photosynthesis (A)") +
  ggtitle("Potential acclimation response to light") +
  theme_bw()

