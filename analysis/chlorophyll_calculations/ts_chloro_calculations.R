###############################################################################
## Chlorophyll content
###############################################################################

#Load in files and make a dataframe

plate1.df <- data.frame(read.csv("eve_timescale_chl/eve_timescale_chl/Gray_chlorophyll_plate1_cleaned.csv"))
plate2.df <- data.frame(read.csv("eve_timescale_chl/eve_timescale_chl/Gray_chlorophyll_plate2_cleaned.csv"))

#merge
allplates <- rbind(plate1.df,plate2.df)

# Isolate to one row per ID
chlor.df <- (allplates) %>%
  filter(id != "blank") %>%
  dplyr::select(rep = id, everything()) %>%
  group_by(rep) %>%
  summarize(abs.649 = mean(abs_649_corrected, na.rm = TRUE),
            cv.649 = (sd(abs_649_corrected, na.rm = TRUE) / abs.649) * 100,
            abs.665 = mean(abs_665_corrected, na.rm = TRUE),
            cv.665 = (sd(abs_665_corrected, na.rm = TRUE) / abs.665) * 100) %>%
  mutate(abs.649 = ifelse(abs.649 < 0, 0, abs.649),
         abs.665 = ifelse(abs.665 < 0, 0, abs.665))

#Isolate disc area
structual.df <- data.frame(read.csv("structural_data.csv"))
structual.disc <- data.frame(structual.df$ID, structual.df$disc_area,
                             structual.df$chloro_weight, structual.df$total_area_chloro)

chlor.filter <- data.frame(chlor.df[-1,])
chlor.filter2 <- data.frame(chlor.filter[-1,])

#Bind the two
chlor.disc <- cbind(structual.disc, chlor.filter2)

names(chlor.disc)[names(chlor.disc) == "structual.df.ID"] <- "id"
names(chlor.disc)[names(chlor.disc) == "structual.df.disc_area"] <- "disc_area"
names(chlor.disc)[names(chlor.disc) == "structual.df.chloro_weight"] <- "chloro_weight"
names(chlor.disc)[names(chlor.disc) == "structual.df.total_area_chloro"] <- "chloro_area"

chlor.disc.all <- chlor.disc[, -which(names(chlor.disc) == "rep")]

chlor.disc7 <- chlor.disc.all %>% filter(id %in% c("paul.hc.3.19","brit.hc.2.2",
                                                   "brit.hl.1.5"))
chlor.disc7.5 <- chlor.disc.all %>% filter(id %in% c("ryan.hl.1.37","paul.hc.2.18"))
chlor.disc8 <- chlor.disc.all %>% filter(id %in% c("brit.hc.1.1","brit.hl.3.7",
                                                   "paul.hl.1.21","ryan.lc.1.41",
                                                   "brit.hl.2.6"))
chlor.disc8.2 <- chlor.disc.all %>% filter(id %in% c("ryan.lc.4.44"))
chlor.disc8.5 <- chlor.disc.all %>% filter(id %in% c("paul.hc.4.20","paul.hl.4.24"))
chlor.disc8.8 <- chlor.disc.all %>% filter(id %in% c("ryan.hc.3.35"))
chlor.disc9 <- chlor.disc.all %>% filter(id %in% c("ryan.hc.2.34","ryan.lc.3.43",
                                                   "ryan.lh.4.48"))
chlor.disc10 <- chlor.disc.all %>% filter(id %in% c("brit.lc.1.9","brit.lc.2.10",
                                                    "brit.lc.3.11","brit.lh.1.13",
                                                    "brit.lh.3.15","brit.lh.4.16",
                                                    "paul.lc.1.25","paul.lc.2.26",
                                                    "paul.lc.3.27","paul.lh.1.29",
                                                    "paul.lh.2.30","paul.lh.3.31",
                                                    "paul.hl.2.22","ryan.hc.1.33",
                                                    "ryan.hl.2.38","ryan.hl.4.40",
                                                    "ryan.lh.1.45","ryan.lh.3.47"))
                                                   
chlorophyll7 <- chlor.disc7 %>% 
  dplyr::select(id, abs.649, abs.665, disc_area) %>%
  mutate(chlA.ugml = (12.19 * abs.665) - (3.56 * abs.649),
         chlB.ugml = (21.99 * abs.649) - (5.32 * abs.665),
         chlA.ugml = ifelse(chlA.ugml < 0, 0, chlA.ugml),
         chlB.ugml = ifelse(chlB.ugml < 0, 0 , chlB.ugml),
         chlA.gml = chlA.ugml / 1000000,
         chlB.gml = chlB.ugml / 1000000,
         chlA.g = chlA.gml * 7, # extracted in 10mL DMSO
         chlB.g = chlB.gml * 7, # extracted in 10mL DMSO
         chlA.gm2 = chlA.g / (disc_area / 10000),
         chlB.gm2 = chlB.g / (disc_area / 10000),
         chlA.mmolm2 = chlA.gm2 / 893.51 * 1000,
         chlB.mmolm2 = chlB.gm2 / 907.47 * 1000) %>%
         dplyr::select(id, chlA.ugml:chlB.mmolm2)

chlorophyll7.5 <- chlor.disc7.7 %>% 
  dplyr::select(id, abs.649, abs.665, disc_area) %>%
  mutate(chlA.ugml = (12.19 * abs.665) - (3.56 * abs.649),
         chlB.ugml = (21.99 * abs.649) - (5.32 * abs.665),
         chlA.ugml = ifelse(chlA.ugml < 0, 0, chlA.ugml),
         chlB.ugml = ifelse(chlB.ugml < 0, 0 , chlB.ugml),
         chlA.gml = chlA.ugml / 1000000,
         chlB.gml = chlB.ugml / 1000000,
         chlA.g = chlA.gml * 7.5, # extracted in 10mL DMSO
         chlB.g = chlB.gml * 7.5, # extracted in 10mL DMSO
         chlA.gm2 = chlA.g / (disc_area / 10000),
         chlB.gm2 = chlB.g / (disc_area / 10000),
         chlA.mmolm2 = chlA.gm2 / 893.51 * 1000,
         chlB.mmolm2 = chlB.gm2 / 907.47 * 1000) %>%
         dplyr::select(id, chlA.ugml:chlB.mmolm2)

chlorophyll8 <- chlor.disc8 %>% 
  dplyr::select(id, abs.649, abs.665, disc_area) %>%
  mutate(chlA.ugml = (12.19 * abs.665) - (3.56 * abs.649),
         chlB.ugml = (21.99 * abs.649) - (5.32 * abs.665),
         chlA.ugml = ifelse(chlA.ugml < 0, 0, chlA.ugml),
         chlB.ugml = ifelse(chlB.ugml < 0, 0 , chlB.ugml),
         chlA.gml = chlA.ugml / 1000000,
         chlB.gml = chlB.ugml / 1000000,
         chlA.g = chlA.gml * 8, # extracted in 10mL DMSO
         chlB.g = chlB.gml * 8, # extracted in 10mL DMSO
         chlA.gm2 = chlA.g / (disc_area / 10000),
         chlB.gm2 = chlB.g / (disc_area / 10000),
         chlA.mmolm2 = chlA.gm2 / 893.51 * 1000,
         chlB.mmolm2 = chlB.gm2 / 907.47 * 1000) %>%
         dplyr::select(id, chlA.ugml:chlB.mmolm2)

chlorophyll8.2 <- chlor.disc8.2 %>% 
  dplyr::select(id, abs.649, abs.665, disc_area) %>%
  mutate(chlA.ugml = (12.19 * abs.665) - (3.56 * abs.649),
         chlB.ugml = (21.99 * abs.649) - (5.32 * abs.665),
         chlA.ugml = ifelse(chlA.ugml < 0, 0, chlA.ugml),
         chlB.ugml = ifelse(chlB.ugml < 0, 0 , chlB.ugml),
         chlA.gml = chlA.ugml / 1000000,
         chlB.gml = chlB.ugml / 1000000,
         chlA.g = chlA.gml * 8.2, # extracted in 10mL DMSO
         chlB.g = chlB.gml * 8.2, # extracted in 10mL DMSO
         chlA.gm2 = chlA.g / (disc_area / 10000),
         chlB.gm2 = chlB.g / (disc_area / 10000),
         chlA.mmolm2 = chlA.gm2 / 893.51 * 1000,
         chlB.mmolm2 = chlB.gm2 / 907.47 * 1000) %>%
         dplyr::select(id, chlA.ugml:chlB.mmolm2)

chlorophyll8.5 <- chlor.disc8.5 %>% 
  dplyr::select(id, abs.649, abs.665, disc_area) %>%
  mutate(chlA.ugml = (12.19 * abs.665) - (3.56 * abs.649),
         chlB.ugml = (21.99 * abs.649) - (5.32 * abs.665),
         chlA.ugml = ifelse(chlA.ugml < 0, 0, chlA.ugml),
         chlB.ugml = ifelse(chlB.ugml < 0, 0 , chlB.ugml),
         chlA.gml = chlA.ugml / 1000000,
         chlB.gml = chlB.ugml / 1000000,
         chlA.g = chlA.gml * 8.5, # extracted in 10mL DMSO
         chlB.g = chlB.gml * 8.5, # extracted in 10mL DMSO
         chlA.gm2 = chlA.g / (disc_area / 10000),
         chlB.gm2 = chlB.g / (disc_area / 10000),
         chlA.mmolm2 = chlA.gm2 / 893.51 * 1000,
         chlB.mmolm2 = chlB.gm2 / 907.47 * 1000) %>%
         dplyr::select(id, chlA.ugml:chlB.mmolm2)

chlorophyll8.8 <- chlor.disc8.8 %>% 
  dplyr::select(id, abs.649, abs.665, disc_area) %>%
  mutate(chlA.ugml = (12.19 * abs.665) - (3.56 * abs.649),
         chlB.ugml = (21.99 * abs.649) - (5.32 * abs.665),
         chlA.ugml = ifelse(chlA.ugml < 0, 0, chlA.ugml),
         chlB.ugml = ifelse(chlB.ugml < 0, 0 , chlB.ugml),
         chlA.gml = chlA.ugml / 1000000,
         chlB.gml = chlB.ugml / 1000000,
         chlA.g = chlA.gml * 8.8, # extracted in 10mL DMSO
         chlB.g = chlB.gml * 8.8, # extracted in 10mL DMSO
         chlA.gm2 = chlA.g / (disc_area / 10000),
         chlB.gm2 = chlB.g / (disc_area / 10000),
         chlA.mmolm2 = chlA.gm2 / 893.51 * 1000,
         chlB.mmolm2 = chlB.gm2 / 907.47 * 1000) %>%
         dplyr::select(id, chlA.ugml:chlB.mmolm2)

chlorophyll9 <- chlor.disc9 %>% 
  dplyr::select(id, abs.649, abs.665, disc_area) %>%
  mutate(chlA.ugml = (12.19 * abs.665) - (3.56 * abs.649),
         chlB.ugml = (21.99 * abs.649) - (5.32 * abs.665),
         chlA.ugml = ifelse(chlA.ugml < 0, 0, chlA.ugml),
         chlB.ugml = ifelse(chlB.ugml < 0, 0 , chlB.ugml),
         chlA.gml = chlA.ugml / 1000000,
         chlB.gml = chlB.ugml / 1000000,
         chlA.g = chlA.gml * 9, # extracted in 10mL DMSO
         chlB.g = chlB.gml * 9, # extracted in 10mL DMSO
         chlA.gm2 = chlA.g / (disc_area / 10000),
         chlB.gm2 = chlB.g / (disc_area / 10000),
         chlA.mmolm2 = chlA.gm2 / 893.51 * 1000,
         chlB.mmolm2 = chlB.gm2 / 907.47 * 1000) %>%
         dplyr::select(id, chlA.ugml:chlB.mmolm2)

chlorophyll10 <- chlor.disc10 %>% 
  dplyr::select(id, abs.649, abs.665, disc_area) %>%
  mutate(chlA.ugml = (12.19 * abs.665) - (3.56 * abs.649),
         chlB.ugml = (21.99 * abs.649) - (5.32 * abs.665),
         chlA.ugml = ifelse(chlA.ugml < 0, 0, chlA.ugml),
         chlB.ugml = ifelse(chlB.ugml < 0, 0 , chlB.ugml),
         chlA.gml = chlA.ugml / 1000000,
         chlB.gml = chlB.ugml / 1000000,
         chlA.g = chlA.gml * 10, # extracted in 10mL DMSO
         chlB.g = chlB.gml * 10, # extracted in 10mL DMSO
         chlA.gm2 = chlA.g / (disc_area / 10000),
         chlB.gm2 = chlB.g / (disc_area / 10000),
         chlA.mmolm2 = chlA.gm2 / 893.51 * 1000,
         chlB.mmolm2 = chlB.gm2 / 907.47 * 1000) %>%
         dplyr::select(id, chlA.ugml:chlB.mmolm2)

chlorophyll.data <- data.frame(rbind(chlorophyll7,chlorophyll7.5,chlorophyll8,
                                     chlorophyll8.2,chlorophyll8.5,chlorophyll8.8,
                                     chlorophyll9,chlorophyll10))

write.csv(chlorophyll.data, "ts_chlorophyll_data.csv", row.names = F)

