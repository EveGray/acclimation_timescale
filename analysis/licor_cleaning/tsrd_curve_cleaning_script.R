# load needed libraries
library(tidyverse)
library(readLicorData)
# install.packages("remotes")
# remotes::install_github("poales/readLicorData")
library(plantecophys)
library(dplyr)
library(gtools)

## Clean Licor data 
licor1 <- licorData(location = "../../data/licor/licor_raw_text/352_ts_resp_alb")
#write.csv(licor1, "../../data/licor/licor_cleaned/resp/352_ts_resp_alb_clean.csv", 
          #row.names = FALSE)

licor2 <- licorData(location = "../../data/licor/licor_raw_text/352_ts_resp_oz")
#write.csv(licor2, "../../data/licor/licor_cleaned/resp/352_ts_resp_oz_clean.csv", 
          #row.names = FALSE)

licor3 <- licorData(location = "../../data/licor/licor_raw_text/352_ts_photo_gib")
#write.csv(licor3, "../../data/licor/licor_cleaned/resp/352_ts_resp_gib_clean.csv", 
          #row.names = FALSE)

### End of doy 352 ###

licor4 <- licorData(location = "../../data/licor/licor_raw_text/355_ts_resp_alb") 
#write.csv(licor4, "../../data/licor/licor_cleaned/resp/355_ts_resp_alb.csv", 
          #row.names = FALSE)

licor5 <- licorData(location = "../../data/licor/licor_raw_text/355-ts-resp-oz") 
#write.csv(licor5, "../../data/licor/licor_cleaned/resp/355_ts_resp_oz.csv", 
          #row.names = FALSE)

licor6 <- licorData(location = "../../data/licor/licor_raw_text/355_ts_resp_gib") 
#write.csv(licor4, "../../data/licor/licor_cleaned/resp/355_ts_resp_gib.csv", 
          #row.names = FALSE)

### End of doy 355 ###

licor7 <- licorData(location = "../../data/licor/licor_raw_text/358_ts_resp_alb") 
#write.csv(licor7, "../../data/licor/licor_cleaned/resp/358_ts_resp_alb.csv", 
          #row.names = FALSE)

licor8 <- licorData(location = "../../data/licor/licor_raw_text/358-ts-resp-oz") 
#write.csv(licor8, "../../data/licor/licor_cleaned/resp/358_ts_resp_oz.csv", 
          #row.names = FALSE)

licor9 <- licorData(location = "../../data/licor/licor_raw_text/358_ts_resp_gib") 
#write.csv(licor9, "../../data/licor/licor_cleaned/resp/358_ts_resp_gib.csv", 
          #row.names = FALSE)

### End of doy 358 ###

licor10 <- licorData(location = "../../data/licor/licor_raw_text/359_ts_resp_alb") 
#write.csv(licor10, "../../data/licor/licor_cleaned/resp/359_ts_resp_alb.csv", 
          #row.names = FALSE)

licor11 <- licorData(location = "../../data/licor/licor_raw_text/359_ts_resp_oz") 
#write.csv(licor11, "../../data/licor/licor_cleaned/resp/359_ts_resp_oz.csv", 
          #row.names = FALSE)

licor12 <- licorData(location = "../../data/licor/licor_raw_text/359_ts_resp_gib") 
#write.csv(licor12, "../../data/licor/licor_cleaned/resp/359_ts_resp_gib.csv", 
          #row.names = FALSE)

### End of doy 359 ###

licor13 <- licorData(location = "../../data/licor/licor_raw_text/360_ts_resp_alb") 
#write.csv(licor13, "../../data/licor/licor_cleaned/resp/360_ts_resp_alb.csv", 
          #row.names = FALSE)

licor14 <- licorData(location = "../../data/licor/licor_raw_text/360_ts_resp_oz") 
#write.csv(licor14, "../../data/licor/licor_cleaned/resp/360_ts_resp_oz.csv", 
          #row.names = FALSE)

licor15 <- licorData(location = "../../data/licor/licor_raw_text/360_ts_resp_gib") 
#write.csv(licor15, "../../data/licor/licor_cleaned/resp/360_ts_resp_gib.csv", 
          #row.names = FALSE)

### End of doy 360 ###

licor16 <- licorData(location = "../../data/licor/licor_raw_text/361_ts_resp_alb") 
#write.csv(licor16, "../../data/licor/licor_cleaned/resp/361_ts_resp_alb.csv", 
          #row.names = FALSE)

licor17 <- licorData(location = "../../data/licor/licor_raw_text/361_ts_resp_oz") 
#write.csv(licor17, "../../data/licor/licor_cleaned/resp/361_ts_resp_oz.csv", 
          #row.names = FALSE)

licor18 <- licorData(location = "../../data/licor/licor_raw_text/361_ts_resp_gib") 
#write.csv(licor18, "../../data/licor/licor_cleaned/resp/361_ts_resp_gib.csv", 
          #row.names = FALSE)

### End of doy 361 ###

licor19 <- licorData(location = "../../data/licor/licor_raw_text/362_ts_resp_alb") 
#write.csv(licor19, "../../data/licor/licor_cleaned/resp/362_ts_resp_alb.csv", 
          #row.names = FALSE)

licor20 <- licorData(location = "../../data/licor/licor_raw_text/362_ts_resp_oz") 
#write.csv(licor20, "../../data/licor/licor_cleaned/resp/362_ts_resp_oz.csv", 
          #row.names = FALSE)

#Missing this file in all reccords  
#licor21 <- licorData(location = "../../data/licor/licor_raw_text/") 
##write.csv(licor21, "../../data/licor/licor_cleaned/", 
#          #row.names = FALSE)

### End of doy 362 ###

licor22 <- licorData(location = "../../data/licor/licor_raw_text/364_ts_resp_alb") 
#write.csv(licor22, "../../data/licor/licor_cleaned/resp/364_ts_resp_alb.csv", 
          #row.names = FALSE)

licor23 <- licorData(location = "../../data/licor/licor_raw_text/364_ts_resp_oz") 
#write.csv(licor23, "../../data/licor/licor_cleaned/resp/364_ts_resp_oz.csv", 
          #row.names = FALSE)

licor24 <- licorData(location = "../../data/licor/licor_raw_text/364_ts_resp_gib") 
#write.csv(licor24, "../../data/licor/licor_cleaned/resp/364_ts_resp_gib.csv", 
          #row.names = FALSE)

### End of doy 364 ###

licor25 <- licorData(location = "../../data/licor/licor_raw_text/365_ts_resp_alb") 
#write.csv(licor25, "../../data/licor/licor_cleaned/resp/365_ts_resp_alb.csv", 
          #row.names = FALSE)

licor26 <- licorData(location = "../../data/licor/licor_raw_text/365_ts_resp_oz") 
#write.csv(licor26, "../../data/licor/licor_cleaned/resp/365_ts_resp_oz.csv", 
          #row.names = FALSE)

licor27 <- licorData(location = "../../data/licor/licor_raw_text/365_ts_resp_gib") 
#write.csv(licor27, "../../data/licor/licor_cleaned/resp/365_ts_resp_gib.csv", 
          #row.names = FALSE)

### End of doy 365 ###

licor28 <- licorData(location = "../../data/licor/licor_raw_text/1_ts_resp_alb") 
#write.csv(licor28, "../../data/licor/licor_cleaned/resp/1_ts_resp_alb.csv", 
          #row.names = FALSE)

licor29 <- licorData(location = "../../data/licor/licor_raw_text/1_ts_resp_oz") 
#write.csv(licor29, "../../data/licor/licor_cleaned/resp/1_ts_resp_oz.csv", 
          #row.names = FALSE)

licor30 <- licorData(location = "../../data/licor/licor_raw_text/1_ts_resp_gib") 
#write.csv(licor30, "../../data/licor/licor_cleaned/resp/1_ts_resp_gib.csv", 
          #row.names = FALSE)

### End of doy 1 ###

licor31 <- licorData(location = "../../data/licor/licor_raw_text/2_ts_resp_alb") 
#write.csv(licor31, "../../data/licor/licor_cleaned/resp/2_ts_resp_alb.csv", 
          #row.names = FALSE)

licor32 <- licorData(location = "../../data/licor/licor_raw_text/2_ts_resp_oz") 
#write.csv(licor32, "../../data/licor/licor_cleaned/resp/2_ts_resp_oz.csv", 
          #row.names = FALSE)

licor33 <- licorData(location = "../../data/licor/licor_raw_text/2_ts_resp_gib") 
#write.csv(licor33, "../../data/licor/licor_cleaned/resp/2_ts_resp_gib.csv", 
          #row.names = FALSE)

### End of doy 2 ###

licor34 <- licorData(location = "../../data/licor/licor_raw_text/3_ts_resp_alb") 
#write.csv(licor34, "../../data/licor/licor_cleaned/resp/3_ts_resp_alb.csv", 
          #row.names = FALSE)

licor35 <- licorData(location = "../../data/licor/licor_raw_text/3_ts_resp_oz") 
#write.csv(licor35, "../../data/licor/licor_cleaned/resp/3_ts_resp_oz.csv", 
          #row.names = FALSE)

licor36 <- licorData(location = "../../data/licor/licor_raw_text/3_ts_resp_gib") 
#write.csv(licor36, "../../data/licor/licor_cleaned/resp/3_ts_resp_gib.csv", 
          #row.names = FALSE)

### End of doy 3 ###

licor37 <- licorData(location = "../../data/licor/licor_raw_text/4_ts_resp_alb") 
#write.csv(licor37, "../../data/licor/licor_cleaned/resp/4_ts_resp_alb.csv", 
          #row.names = FALSE)

licor38 <- licorData(location = "../../data/licor/licor_raw_text/4_ts_resp_oz") 
#write.csv(licor38, "../../data/licor/licor_cleaned/resp/4_ts_resp_oz.csv", 
          #row.names = FALSE)

licor39 <- licorData(location = "../../data/licor/licor_raw_text/4_ts_resp_gib") 
#write.csv(licor39, "../../data/licor/licor_cleaned/resp/4_ts_resp_gib.csv", 
          #row.names = FALSE)

### End of doy 4 ###

licor40 <- licorData(location = "../../data/licor/licor_raw_text/5_ts_resp_alb") 
#write.csv(licor40, "../../data/licor/licor_cleaned/resp/5_ts_resp_alb.csv", 
          #row.names = FALSE)

licor41 <- licorData(location = "../../data/licor/licor_raw_text/5_ts_resp_oz") 
#write.csv(licor41, "../../data/licor/licor_cleaned/resp/5_ts_resp_oz.csv", 
          #row.names = FALSE)

licor42 <- licorData(location = "../../data/licor/licor_raw_text/5_ts_resp_gib") 
#write.csv(licor42, "../../data/licor/licor_cleaned/resp/5_ts_resp_gib.csv", 
          #row.names = FALSE)

### End of doy 5 ###

licor43 <- licorData(location = "../../data/licor/licor_raw_text/7_ts_resp_alb") 
#write.csv(licor43, "../../data/licor/licor_cleaned/resp/7_ts_resp_alb.csv", 
          #row.names = FALSE)

licor44 <- licorData(location = "../../data/licor/licor_raw_text/7_ts_resp_oz") 
#write.csv(licor44, "../../data/licor/licor_cleaned/resp/7_ts_resp_oz.csv", 
          #row.names = FALSE)

licor45 <- licorData(location = "../../data/licor/licor_raw_text/7_ts_resp_gib") 
#write.csv(licor45, "../../data/licor/licor_cleaned/resp/7_ts_resp_gib.csv", 
          #row.names = FALSE)

### End of doy 7 ###

#Comes up as zero observations of 4 variables
#licor46 <- licorData(location = "../../data/licor/licor_raw_text/8_ts_photo_alb_new") 
##write.csv(licor46, "../../data/licor/licor_cleaned/8_ts_photo_alb_new.csv", 
#          #row.names = FALSE)
#
#licor47 <- licorData(location = "../../data/licor/licor_raw_text/8_ts_photo_oz_new") 
##write.csv(licor47, "../../data/licor/licor_cleaned/8_ts_photo_oz_new.csv", 
#          #row.names = FALSE)
#
#licor48 <- licorData(location = "../../data/licor/licor_raw_text/8_ts_photo_gib_new") 
##write.csv(licor48, "../../data/licor/licor_cleaned/8_ts_photo_gib_new.csv", 
#          #row.names = FALSE)

### End of doy 8 ###

licor49 <- licorData(location = "../../data/licor/licor_raw_text/9_ts_resp_alb") 
#write.csv(licor49, "../../data/licor/licor_cleaned/resp/9_ts_resp_alb.csv", 
          #row.names = FALSE)

licor50 <- licorData(location = "../../data/licor/licor_raw_text/9_ts_resp_oz") 
#write.csv(licor50, "../../data/licor/licor_cleaned/resp/9_ts_resp_oz.csv", 
          #row.names = FALSE)

licor51 <- licorData(location = "../../data/licor/licor_raw_text/9_ts_resp_gib") 
#write.csv(licor51, "../../data/licor/licor_cleaned/resp/9_ts_resp_gib.csv", 
          #row.names = FALSE)

### End of doy 9 ###

licor52 <- licorData(location = "../../data/licor/licor_raw_text/10_ts_resp_alb") 
#write.csv(licor52, "../../data/licor/licor_cleaned/resp/10_ts_resp_alb.csv", 
          #row.names = FALSE)

licor53 <- licorData(location = "../../data/licor/licor_raw_text/10_ts_resp_oz") 
#write.csv(licor53, "../../data/licor/licor_cleaned/resp/10_ts_resp_oz.csv", 
          #row.names = FALSE)

licor54 <- licorData(location = "../../data/licor/licor_raw_text/10_ts_resp_gib") 
#write.csv(licor54, "../../data/licor/licor_cleaned/resp/10_ts_resp_gib.csv", 
          #row.names = FALSE)

### End of doy 10 ###

licor55 <- licorData(location = "../../data/licor/licor_raw_text/11_ts_resp_alb_new") 
#write.csv(licor55, "../../data/licor/licor_cleaned/resp/11_ts_resp_alb_new.csv", 
          #row.names = FALSE)

licor56 <- licorData(location = "../../data/licor/licor_raw_text/11_ts_resp_oz_new") 
#write.csv(licor56, "../../data/licor/licor_cleaned/resp/11_ts_resp_oz_new.csv", 
          #row.names = FALSE)

licor57 <- licorData(location = "../../data/licor/licor_raw_text/11_ts_resp_gib_new") 
#write.csv(licor57, "../../data/licor/licor_cleaned/resp/11_ts_resp_gib_new.csv", 
          #row.names = FALSE)

### End of doy 11 ###

licor58 <- licorData(location = "../../data/licor/licor_raw_text/12_ts_resp_alb") 
#write.csv(licor58, "../../data/licor/licor_cleaned/resp/12_ts_resp_alb.csv", 
          #row.names = FALSE)

licor59 <- licorData(location = "../../data/licor/licor_raw_text/12_ts_resp_oz") 
#write.csv(licor59, "../../data/licor/licor_cleaned/resp/12_ts_resp_oz.csv", 
          #row.names = FALSE)

licor60 <- licorData(location = "../../data/licor/licor_raw_text/12_ts_resp_gib") 
#write.csv(licor60, "../../data/licor/licor_cleaned/resp/12_ts_resp_gib.csv", 
          #row.names = FALSE)

### End of doy 12 ###

licor61 <- licorData(location = "../../data/licor/licor_raw_text/14_ts_resp_alb_new") 
#write.csv(licor61, "../../data/licor/licor_cleaned/resp/14_ts_resp_alb_new.csv", 
          #row.names = FALSE)

licor62 <- licorData(location = "../../data/licor/licor_raw_text/14_ts_resp_oz_new") 
#write.csv(licor62, "../../data/licor/licor_cleaned/resp/14_ts_resp_oz_new.csv", 
          #row.names = FALSE)

licor63 <- licorData(location = "../../data/licor/licor_raw_text/14_ts_photo_gib_new") 
#write.csv(licor63, "../../data/licor/licor_cleaned/resp/14_ts_resp_gib_new.csv", 
          #row.names = FALSE)

### End of doy 14 ###

licor64 <- licorData(location = "../../data/licor/licor_raw_text/15_ts_resp_alb_new") 
#write.csv(licor64, "../../data/licor/licor_cleaned/resp/15_ts_resp_alb_new.csv", 
          #row.names = FALSE)

licor65 <- licorData(location = "../../data/licor/licor_raw_text/15_ts_resp_oz_new") 
#write.csv(licor65, "../../data/licor/licor_cleaned/resp/15_ts_resp_oz_new.csv", 
          #row.names = FALSE)

licor66 <- licorData(location = "../../data/licor/licor_raw_text/15_ts_resp_gib_new") 
#write.csv(licor66, "../../data/licor/licor_cleaned/resp/15_ts_resp_gib_new.csv", 
          #row.names = FALSE)

### End of doy 15 ###

licor67 <- licorData(location = "../../data/licor/licor_raw_text/16_ts_resp_alb_new") 
#write.csv(licor67, "../../data/licor/licor_cleaned/resp/16_ts_resp_alb_new.csv", 
          #row.names = FALSE)

licor68 <- licorData(location = "../../data/licor/licor_raw_text/16_ts_resp_oz_new") 
#write.csv(licor68, "../../data/licor/licor_cleaned/resp/16_ts_resp_oz_new.csv", 
          #row.names = FALSE)

licor69 <- licorData(location = "../../data/licor/licor_raw_text/16_ts_resp_gib_new") 
#write.csv(licor69, "../../data/licor/licor_cleaned/resp/16_ts_resp_gib_new.csv", 
          #row.names = FALSE)

### End of doy 16 ###

licor70 <- licorData(location = "../../data/licor/licor_raw_text/17_ts_resp_alb_new") 
#write.csv(licor70, "../../data/licor/licor_cleaned/resp/17_ts_resp_alb_new.csv", 
          #row.names = FALSE)

licor71 <- licorData(location = "../../data/licor/licor_raw_text/17_ts_resp_oz_new") 
#write.csv(licor71, "../../data/licor/licor_cleaned/resp/17_ts_resp_oz_new.csv", 
          #row.names = FALSE)

licor72 <- licorData(location = "../../data/licor/licor_raw_text/17_ts_resp_gib_new") 
#write.csv(licor72, "../../data/licor/licor_cleaned/resp/17_ts_resp_gib_new.csv", 
          #row.names = FALSE)

### End of doy 17 ###

licor73 <- licorData(location = "../../data/licor/licor_raw_text/18_ts_resp_alb_new") 
#write.csv(licor73, "../../data/licor/licor_cleaned/resp/18_ts_resp_alb_new.csv", 
          #row.names = FALSE)

licor74 <- licorData(location = "../../data/licor/licor_raw_text/18_ts_resp_oz_new") 
#write.csv(licor74, "../../data/licor/licor_cleaned/resp/18_ts_resp_oz_new.csv", 
          #row.names = FALSE)

licor75 <- licorData(location = "../../data/licor/licor_raw_text/18_ts_resp_gib_new") 
#write.csv(licor75, "../../data/licor/licor_cleaned/resp/18_ts_resp_gib_new.csv", 
          #row.names = FALSE)

### End of doy 18 ###

licor76 <- licorData(location = "../../data/licor/licor_raw_text/19_ts_resp_alb_new") 
#write.csv(licor76, "../../data/licor/licor_cleaned/resp/19_ts_resp_alb_new.csv", 
          #row.names = FALSE)

licor77 <- licorData(location = "../../data/licor/licor_raw_text/19_ts_resp_oz_new") 
#write.csv(licor77, "../../data/licor/licor_cleaned/resp/19_ts_resp_oz_new.csv", 
          #row.names = FALSE)

licor78 <- licorData(location = "../../data/licor/licor_raw_text/19_ts_resp_gib_new") 
#write.csv(licor78, "../../data/licor/licor_cleaned/resp/19_ts_resp_gib_new.csv", 
          #row.names = FALSE)

### End of doy 19 ###

licor79 <- licorData(location = "../../data/licor/licor_raw_text/21_ts_resp_alb_new") 
#write.csv(licor79, "../../data/licor/licor_cleaned/resp/21_ts_resp_alb_new.csv", 
          #row.names = FALSE)

licor80 <- licorData(location = "../../data/licor/licor_raw_text/21_ts_resp_oz_new") 
#write.csv(licor80, "../../data/licor/licor_cleaned/resp/21_ts_resp_oz_new.csv", 
          #row.names = FALSE)

licor81 <- licorData(location = "../../data/licor/licor_raw_text/21_ts_resp_gib_new") 
#write.csv(licor81, "../../data/licor/licor_cleaned/resp/21_ts_resp_gib_new.csv", 
          #row.names = FALSE)

## merge all licor data together
licor_objects_merged <- smartbind(as.data.frame(licor1), as.data.frame(licor2),
                                  as.data.frame(licor3), as.data.frame(licor4),
                                  as.data.frame(licor5), as.data.frame(licor6),
                                  as.data.frame(licor7), as.data.frame(licor8),
                                  as.data.frame(licor9), as.data.frame(licor10),
                                  as.data.frame(licor11), as.data.frame(licor12),
                                  as.data.frame(licor13), as.data.frame(licor14),
                                  as.data.frame(licor15), as.data.frame(licor16),
                                  as.data.frame(licor17), as.data.frame(licor18),
                                  as.data.frame(licor19), as.data.frame(licor20),
                                  as.data.frame(licor22),
                                  as.data.frame(licor23), as.data.frame(licor24),
                                  as.data.frame(licor25), as.data.frame(licor26),
                                  as.data.frame(licor27), as.data.frame(licor28),
                                  as.data.frame(licor29), as.data.frame(licor30),
                                  as.data.frame(licor31), as.data.frame(licor32),
                                  as.data.frame(licor33), as.data.frame(licor34),
                                  as.data.frame(licor35), as.data.frame(licor36),
                                  as.data.frame(licor37), as.data.frame(licor38),
                                  as.data.frame(licor39), as.data.frame(licor40),
                                  as.data.frame(licor41), as.data.frame(licor42),
                                  as.data.frame(licor43), as.data.frame(licor44),
                                  as.data.frame(licor45),
                                  as.data.frame(licor49), as.data.frame(licor50),
                                  as.data.frame(licor51), as.data.frame(licor52),
                                  as.data.frame(licor53), as.data.frame(licor54),
                                  as.data.frame(licor55), as.data.frame(licor56),
                                  as.data.frame(licor57), as.data.frame(licor58),
                                  as.data.frame(licor59), as.data.frame(licor60),
                                  as.data.frame(licor61), as.data.frame(licor62),
                                  as.data.frame(licor63), as.data.frame(licor64),
                                  as.data.frame(licor65), as.data.frame(licor66),
                                  as.data.frame(licor67), as.data.frame(licor68),
                                  as.data.frame(licor69), as.data.frame(licor70),
                                  as.data.frame(licor71), as.data.frame(licor72),
                                  as.data.frame(licor73), as.data.frame(licor74),
                                  as.data.frame(licor75), as.data.frame(licor76),
                                  as.data.frame(licor77), as.data.frame(licor78),
                                  as.data.frame(licor79), as.data.frame(licor80),
                                  as.data.frame(licor81))

write.csv(licor_objects_merged, "../../data/licor/licor_cleaned/tsrd_merged_all.csv", row.names = FALSE) # write csv file


# file.list <- list.files(path = "../../data/licor/licor_cleaned/resp/", pattern = "*.csv",
#                         recursive = TRUE, full.names = TRUE)
# 
# file.list <- setNames(file.list, stringr::str_extract(basename(file.list),'.*(?=\\.csv)'))
# merged_files <- lapply(file.list, read.csv) %>%
#   reshape::merge_all()
# 
# resp_sum <- merged_files %>% tidyr::separate(date, into = c("date", "time"), sep = " ", remove = TRUE) %>%
#   group_by(id, date) %>% summarise(rd = mean(A, na.rm = TRUE)) %>%
#   mutate(rd = ifelse(rd < 0, 
#                      abs(rd), 
#                      ifelse(rd > 0, 0, NA)))
# 
# aci.df <- read.csv("../../data/licor/licor_cleaned/ts_merged_all.csv") # if doing this de novo, you will need to run the scipt
# # called "ts_curve_cleaning_script.R" to create this file
# 
# aci.df <- tidyr::separate(aci.df, date, into = c("date", "time"), sep = " ", remove = TRUE)
# 
# aci.df.rd <- aci.df %>% left_join(resp_sum, by = c("id", "date")) %>%
#   arrange(id, date, elapsed)
# 
# #write.csv(aci.df.rd, "../../data/licor/licor_cleaned/ts_merged_all_rd.csv", #row.names = FALSE)
