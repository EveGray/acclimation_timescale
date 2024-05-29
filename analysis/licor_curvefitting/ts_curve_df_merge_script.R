## Libraries
library(tidyverse)
library(plantecophys)
library(dplyr)
######################

## read in the data
aci.df <- read.csv("../../data/licor/licor_cleaned/ts_merged_all.csv")
head(aci.df)

## make new columns in aci.df for each of date and time
aci.df <- tidyr::separate(aci.df, date, into = c("day", "time"), sep = " ", remove = TRUE)

## make unique id for each curve based on id and day
aci.df$unique_id <- paste(aci.df$id, aci.df$day, sep = '_') # paste together plant id and date for unique id for each curve
aci.df.unique_id <- aci.df$unique_id # make a string for each unique curve id

## fit curves

### curve1_data
curve1_data <- subset(aci.df, unique_id == aci.df.unique_id[1]) # find correct curve from full dataframe and make new object
plot(curve1_data$A~curve1_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
curve1_fit <- fitaci(curve1_data, varnames = list(ALEAF = "A", # fit the curves
                                     Tleaf = "Tleaf",
                                     Ci = "Ci",
                                     PPFD = "Qin"),
                     fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(curve1_fit) # take a look at fitted values, adjust as needed
plot(curve1_fit) # plot the fitted curves over the raw data, adjust as needed
curve1_fits ### pick up here!!!!! (May 29, 2024)


##########################################################
#### decided not to start fitting anything until the raw data are fully cleaned
#### next step: clean raw licor files, rerun cleaning/merging script, rerun rd merging script, start modifying
#### this script to fit the aci curves
##########################################################


# 1/18



curve1 <- aci.df %>% 
  filter(id == "brit.lc.1.9") %>%
  filter(date == "2023-12-18") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"), 
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 300)

aci.coefs <- data.frame(id = "brit.lc.1.9", doy = "352", baseline = "y", new = "n", t(coef(curve1)))

curve2 <- aci.df %>% 
  filter(id == "brit.lc.2.10") %>%
  filter(date == "2023-12-18") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin",      
                         Rd = "rd"),
         fitTPU = TRUE, Tcorrect = FALSE, useRd = TRUE)

aci.coefs[2,] <- c(id = "brit.lc.2.10", doy = "352", baseline = "y", new = "n", t(coef(curve2)))

## Id:11
curve3 <- aci.df %>% 
  filter(id == "brit.lc.3.11") %>%
  filter(date == "2023-12-18") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 300)

aci.coefs[3,] <- c(id = "brit.lc.3.11", doy = "352", baseline = "y", new = "n", t(coef(curve3)))

## baseline Id:13
curve4 <- aci.df %>% 
  filter(id == "brit.lh.1.13") %>%
  filter(date == "2023-12-18") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 300)

aci.coefs[4,] <- c(id = "brit.lh.1.13", doy = "352", baseline = "y", new = "n", t(coef(curve4)))

## baseline Id:15
curve5 <- aci.df %>% 
  filter(id == "brit.lh.3.15") %>%
  filter(date == "2023-12-18") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 300)

aci.coefs[5,] <- c(id = "brit.lh.3.15", doy = "352", baseline = "y", new = "n", t(coef(curve5)))

## baseline Id:16
curve6 <- aci.df %>% 
  filter(id == "brit.lh.4.16") %>%
  filter(date == "2023-12-18") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 300 )

aci.coefs[6,] <- c(id = "brit.lh.4.16", doy = "352", baseline = "y", new = "n", t(coef(curve6)))

## baseline Id:25
curve7 <- aci.df %>% 
  filter(id == "paul.lc.1.25") %>%
  filter(date == "2023-12-18") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 300)

aci.coefs[7,] <- c(id = "paul.lc.1.25", doy = "352", baseline = "y", new = "n", t(coef(curve7)))

## baseline Id:26 
curve8 <- aci.df %>% 
  filter(id == "paul.lc.2.26") %>%
  filter(date == "2023-12-18") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin",
                         Rd = "rd"),
         fitTPU = TRUE, Tcorrect = FALSE, useRd = TRUE, Citransition = 400)

aci.coefs.352[8,] <- c(id = "paul.lc.2.26", doy = "352", baseline = "y", new = "n", t(coef(curve8)))

## baseline Id:27

curve9 <- aci.df %>% 
  filter(id == "paul.lc.3.27") %>%
  filter(date == "2023-12-18") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, useRd = TRUE, Citransition = 400)

aci.coefs[9,] <- c(id = "paul.lc.3.27", doy = "352", baseline = "y", new = "n", t(coef(curve9)))

## baseline Id:29

curve10 <- aci.df %>% 
  filter(id == "paul.lh.1.29") %>%
  filter(date == "2023-12-18") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 300)

aci.coefs[10,] <- c(id = "paul.lh.1.29", doy = "352", baseline = "y", new = "n", t(coef(curve10)))

## baseline Id:30

curve11 <- aci.df %>% 
  filter(id == "paul.lh.2.30") %>%
  filter(date == "2023-12-18") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 300)

aci.coefs[11,] <- c(id = "paul.lh.2.30", doy = "352", baseline = "y", new = "n", t(coef(curve11)))

## baseline Id:31

curve12 <- aci.df %>% 
  filter(id == "paul.lh.3.31") %>%
  filter(date == "2023-12-18") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[12,] <- c(id = "paul.lh.3.31", doy = "352", baseline = "y", new = "n", t(coef(curve12)))

###############################################################################

## Date 12/21
# Curve 2 G1, ID 9
curve13 <- aci.df %>% 
  filter(id == "brit.lc.1.9") %>%
  filter(date == "2023-12-21") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 300)

aci.coefs[13,] <- c(id = "brit.lc.1.9", doy = "355", baseline = "n", new = "n", t(coef(curve13)))

## Id:10
curve14 <- aci.df %>% 
  filter(id == "brit.lc.2.10") %>%
  filter(date == "2023-12-21") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 300)

aci.coefs[14,] <- c(id = "brit.lc.2.10", doy = "355", baseline = "n", new = "n", t(coef(curve14)))

## Id:11
curve15 <- aci.df %>% 
  filter(id == "brit.lc.3.11") %>%
  filter(date == "2023-12-21") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 300)

aci.coefs[15,] <- c(id = "brit.lc.3.11", doy = "355", baseline = "n", new = "n", t(coef(curve15)))

## baseline Id:13
curve16 <- aci.df %>% 
  filter(id == "brit.lh.1.13") %>%
  filter(date == "2023-12-21") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 600)

aci.coefs[16,] <- c(id = "brit.lh.1.13", doy = "355", baseline = "n", new = "n", t(coef(curve16)))

## baseline Id:15
curve17 <- aci.df %>% 
  filter(id == "brit.lh.3.15") %>%
  filter(date == "2023-12-21") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[17,] <- c(id = "brit.lh.3.15", doy = "355", baseline = "n", new = "n", t(coef(curve17)))

## baseline Id:16
curve18 <- aci.df %>% 
  filter(id == "brit.lh.4.16") %>%
  filter(date == "2023-12-21") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[18,] <- c(id = "brit.lh.4.16", doy = "355", baseline = "n", new = "n", t(coef(curve18)))

## baseline Id:25
curve19 <- aci.df %>% 
  filter(id == "paul.lc.1.25") %>%
  filter(date == "2023-12-21") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[19,] <- c(id = "paul.lc.1.25", doy = "355", baseline = "n", new = "n", t(coef(curve19)))

## baseline Id:26
curve20 <- aci.df %>% 
  filter(id == "paul.lc.2.26") %>%
  filter(date == "2023-12-21") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[20,] <- c(id = "paul.lc.2.26", doy = "355", baseline = "n", new = "n", t(coef(curve20)))

#baseline Id:27
curve21 <- aci.df %>% 
  filter(id == "paul.lc.3.27") %>%
  filter(date == "2023-12-21") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)


aci.coefs[21,] <- c(id = "paul.lc.3.27", doy = "355", baseline = "n", new = "n", t(coef(curve21)))

## baseline Id:29

curve22 <- aci.df %>% 
  filter(id == "paul.lh.1.29") %>%
  filter(date == "2023-12-21") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[22,] <- c(id = "paul.lh.1.29", doy = "355", baseline = "n", new = "n", t(coef(curve22)))

## baseline Id:30

curve23 <- aci.df %>% 
  filter(id == "paul.lh.2.30") %>%
  filter(date == "2023-12-21") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[23,] <- c(id = "paul.lh.2.30", doy = "355", baseline = "n", new = "n", t(coef(curve23)))

## baseline Id:31
curve24 <- aci.df %>% 
  filter(id == "paul.lh.3.31") %>%
  filter(date == "2023-12-21") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[24,] <- c(id = "paul.lh.3.31", doy = "355", baseline = "n", new = "n", t(coef(curve24)))
################################################################################

## Date 12/24
# baseline G2, ID 1
curve25 <- aci.df %>% 
  filter(id == "brit.hc.1.1") %>%
  filter(date == "2023-12-24") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[25,] <- c(id = "brit.hc.1.1", doy = "358", baseline = "y", new = "n", t(coef(curve25)))

## Id:2
curve26 <- aci.df %>% 
  filter(id == "brit.hc.2.2") %>%
  filter(date == "2023-12-24") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[26,] <- c(id = "brit.hc.2.2", doy = "358", baseline = "y", new = "n", t(coef(curve26)))

## Id:5
curve27 <- aci.df %>% 
  filter(id == "brit.hl.1.5") %>%
  filter(date == "2023-12-24") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[27,] <- c(id = "brit.hl.1.5", doy = "358", baseline = "y", new = "n", t(coef(curve27)))

## baseline Id:6
curve28 <- aci.df %>% 
  filter(id == "brit.hl.2.6") %>%
  filter(date == "2023-12-24") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[28,] <- c(id = "brit.hl.2.6", doy = "358", baseline = "y", new = "n", t(coef(curve28)))

## baseline Id:7
curve29 <- aci.df %>% 
  filter(id == "brit.hl.3.7") %>%
  filter(date == "2023-12-24") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[29,] <- c(id = "brit.hl.3.7", doy = "358", baseline = "y", new = "n", t(coef(curve29)))

## baseline Id:41
curve30 <- aci.df %>% 
  filter(id == "ryan.lc.1.41") %>%
  filter(date == "2023-12-24") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[30,] <- c(id = "ryan.lc.1.41", doy = "358", baseline = "y", new = "n", t(coef(curve30)))

## baseline Id:43
curve31 <- aci.df %>% 
  filter(id == "ryan.lc.3.43") %>%
  filter(date == "2023-12-24") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[31,] <- c(id = "ryan.lc.3.43", doy = "358", baseline = "y", new = "n", t(coef(curve31)))

## baseline Id:44
#curve32 <- aci.df %>% 
# filter(id == "ryan.lc.4.44") %>%
#  filter(date == "2023-12-24") %>%
#  fitaci(varnames = list(ALEAF = "A",
#                         Tleaf = "Tleaf",
#                         Ci = "Ci",
#                         PPFD = "Qin"),
#         fitTPU = TRUE, Tcorrect = FALSE)
#
#aci.coefs[32,] <- c(id = "ryan.lc.4.44", doy = "358", baseline = "y", new = "n", t(coef(curve32)))

## baseline Id:45

curve33 <- aci.df %>% 
  filter(id == "ryan.lh.1.45") %>%
  filter(date == "2023-12-24") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[33,] <- c(id = "ryan.lh.1.45", doy = "358", baseline = "y", new = "n", t(coef(curve33)))

## baseline Id:47

curve34 <- aci.df %>% 
  filter(id == "ryan.lh.3.47") %>%
  filter(date == "2023-12-24") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[34,] <- c(id = "ryan.lh.3.47", doy = "358", baseline = "y", new = "n", t(coef(curve34)))

## baseline Id:48

curve35 <- aci.df %>% 
  filter(id == "ryan.lh.4.48") %>%
  filter(date == "2023-12-24") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[35,] <- c(id = "ryan.lh.4.48", doy = "358", baseline = "y", new = "n", t(coef(curve35)))

################################################################################

## Date 12/25
# Curve 3 G1, ID 9
curve36 <- aci.df %>% 
  filter(id == "brit.lc.1.9") %>%
  filter(date == "2023-12-25") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[36,] <- c(id = "brit.lc.1.9", doy = "359", baseline = "n", new = "n", t(coef(curve36)))

## Id:10
curve37 <- aci.df %>% 
  filter(id == "brit.lc.2.10") %>%
  filter(date == "2023-12-25") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[37,] <- c(id = "brit.lc.2.10", doy = "359", baseline = "n", new = "n", t(coef(curve37)))

## Id:11
curve38 <- aci.df %>% 
  filter(id == "brit.lc.3.11") %>%
  filter(date == "2023-12-25") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[38,] <- c(id = "brit.lc.3.11", doy = "359", baseline = "n", new = "n", t(coef(curve38)))

## baseline Id:13
curve39 <- aci.df %>% 
  filter(id == "brit.lh.1.13") %>%
  filter(date == "2023-12-25") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[39,] <- c(id = "brit.lh.1.13", doy = "359", baseline = "n", new = "n", t(coef(curve39)))

## baseline Id:15
curve40 <- aci.df %>% 
  filter(id == "brit.lh.3.15") %>%
  filter(date == "2023-12-25") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[40,] <- c(id = "brit.lh.3.15", doy = "359", baseline = "n", new = "n", t(coef(curve40)))

## baseline Id:16
curve41 <- aci.df %>% 
  filter(id == "brit.lh.4.16") %>%
  filter(date == "2023-12-25") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[41,] <- c(id = "brit.lh.4.16", doy = "359", baseline = "n", new = "n", t(coef(curve41)))

## baseline Id:25
curve42 <- aci.df %>% 
  filter(id == "paul.lc.1.25") %>%
  filter(date == "2023-12-25") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 500)

aci.coefs[42,] <- c(id = "paul.lc.1.25", doy = "359", baseline = "n", new = "n", t(coef(curve42)))

## baseline Id:26
curve43 <- aci.df %>% 
  filter(id == "paul.lc.2.26") %>%
  filter(date == "2023-12-25") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[43,] <- c(id = "paul.lc.2.26", doy = "359", baseline = "n", new = "n", t(coef(curve43)))

## baseline Id:27

curve44 <- aci.df %>% 
  filter(id == "paul.lc.3.27") %>%
  filter(date == "2023-12-25") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[44,] <- c(id = "paul.lc.3.27", doy = "359", baseline = "n", new = "n", t(coef(curve44)))

## baseline Id:29

curve45 <- aci.df %>% 
  filter(id == "paul.lh.1.29") %>%
  filter(date == "2023-12-25") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[45,] <- c(id = "paul.lh.1.29", doy = "359", baseline = "n", new = "n", t(coef(curve45)))

## baseline Id:30

curve46 <- aci.df %>% 
  filter(id == "paul.lh.2.30") %>%
  filter(date == "2023-12-25") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[46,] <- c(id = "paul.lh.2.30", doy = "359", baseline = "n", new = "n", t(coef(curve46)))

## baseline Id:31

curve47 <- aci.df %>% 
  filter(id == "paul.lh.3.31") %>%
  filter(date == "2023-12-25") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[47,] <- c(id = "paul.lh.3.31", doy = "359", baseline = "n", new = "n", t(coef(curve47)))

################################################################################

## Date 12/26
# baseline G3, ID 18
curve48 <- aci.df %>% 
  filter(id == "paul.hc.2.18") %>%
  filter(date == "2023-12-26") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[48,] <- c(id = "paul.hc.2.18", doy = "360", baseline = "y", new = "n", t(coef(curve48)))


## Id:19
curve49 <- aci.df %>% 
  filter(id == "paul.hc.3.19") %>%
  filter(date == "2023-12-26") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[49,] <- c(id = "paul.hc.3.19", doy = "360", baseline = "y", new = "n", t(coef(curve49)))

## Id:20
curve50 <- aci.df %>% 
  filter(id == "paul.hc.4.20") %>%
  filter(date == "2023-12-26") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[50,] <- c(id = "paul.hc.4.20", doy = "360", baseline = "y", new = "n", t(coef(curve50)))

## baseline Id:21
curve51 <- aci.df %>% 
  filter(id == "paul.hl.1.21") %>%
  filter(date == "2023-12-26") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[51,] <- c(id = "paul.hl.1.21", doy = "360", baseline = "y", new = "n", t(coef(curve51)))

## baseline Id:22
curve52 <- aci.df %>% 
  filter(id == "paul.hl.2.22") %>%
  filter(date == "2023-12-26") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[52,] <- c(id = "paul.hl.2.22", doy = "360", baseline = "y", new = "n", t(coef(curve52)))

## baseline Id:24
curve53 <- aci.df %>% 
  filter(id == "paul.hl.4.24") %>%
  filter(date == "2023-12-26") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[53,] <- c(id = "paul.hl.4.24", doy = "360", baseline = "y", new = "n", t(coef(curve53)))

## baseline Id:33
curve54 <- aci.df %>% 
  filter(id == "ryan.hc.1.33") %>%
  filter(date == "2023-12-26") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[54,] <- c(id = "ryan.hc.1.33", doy = "360", baseline = "y", new = "n", t(coef(curve54)))

## baseline Id:34
curve55 <- aci.df %>% 
  filter(id == "ryan.hc.2.34") %>%
  filter(date == "2023-12-26") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[55,] <- c(id = "ryan.hc.2.34", doy = "360", baseline = "y", new = "n", t(coef(curve55)))

## baseline Id:35

curve56 <- aci.df %>% 
  filter(id == "ryan.hc.3.35") %>%
  filter(date == "2023-12-26") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[56,] <- c(id = "ryan.hc.3.35", doy = "360", baseline = "y", new = "n", t(coef(curve56)))

## baseline Id:37

curve57 <- aci.df %>% 
  filter(id == "ryan.hl.1.37") %>%
  filter(date == "2023-12-26") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[57,] <- c(id = "ryan.hl.1.37", doy = "360", baseline = "y", new = "n", t(coef(curve57)))

## baseline Id:38

curve58 <- aci.df %>% 
  filter(id == "ryan.hl.2.38") %>%
  filter(date == "2023-12-26") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[58,] <- c(id = "ryan.hl.2.38", doy = "360", baseline = "y", new = "n", t(coef(curve58)))

## baseline Id:40

curve59 <- aci.df %>% 
  filter(id == "ryan.hl.4.40") %>%
  filter(date == "2023-12-26") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[59,] <- c(id = "ryan.hl.4.40", doy = "360", baseline = "y", new = "n", t(coef(curve59)))

################################################################################

## Date 12/27
# Curve 1 G2, ID 1
curve60 <- aci.df %>% 
  filter(id == "brit.hc.1.1") %>%
  filter(date == "2023-12-27") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[60,] <- c(id = "brit.hc.1.1", doy = "361", baseline = "n", new = "n", t(coef(curve60)))

## Id:2
curve61 <- aci.df %>% 
  filter(id == "brit.hc.2.2") %>%
  filter(date == "2023-12-27") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[61,] <- c(id = "brit.hc.2.2", doy = "361", baseline = "n", new = "n", t(coef(curve61)))

## Id:5
curve62 <- aci.df %>% 
  filter(id == "brit.hl.1.5") %>%
  filter(date == "2023-12-27") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 500)

aci.coefs[62,] <- c(id = "brit.hl.1.5", doy = "361", baseline = "n", new = "n", t(coef(curve62)))

## baseline Id:6
curve63 <- aci.df %>% 
  filter(id == "brit.hl.2.6") %>%
  filter(date == "2023-12-27") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[63,] <- c(id = "brit.hl.2.6", doy = "361", baseline = "n", new = "n", t(coef(curve63)))

## baseline Id:7
curve64 <- aci.df %>% 
  filter(id == "brit.hl.3.7") %>%
  filter(date == "2023-12-27") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[64,] <- c(id = "brit.hl.3.7", doy = "361", baseline = "n", new = "n", t(coef(curve64)))

## baseline Id:41
curve65 <- aci.df %>% 
  filter(id == "ryan.lc.1.41") %>%
  filter(date == "2023-12-27") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[65,] <- c(id = "ryan.lc.1.41", doy = "361", baseline = "n", new = "n", t(coef(curve65)))

## baseline Id:43
curve66 <- aci.df %>% 
  filter(id == "ryan.lc.3.43") %>%
  filter(date == "2023-12-27") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[66,] <- c(id = "ryan.lc.3.43", doy = "361", baseline = "n", new = "n", t(coef(curve66)))

## baseline Id:44
curve67 <- aci.df %>% 
  filter(id == "ryan.lc.4.44") %>%
  filter(date == "2023-12-27") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[67,] <- c(id = "ryan.lc.4.44", doy = "361", baseline = "n", new = "n", t(coef(curve67)))

## baseline Id:45

curve68 <- aci.df %>% 
  filter(id == "ryan.lh.1.45") %>%
  filter(date == "2023-12-27") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[68,] <- c(id = "ryan.lh.1.45", doy = "361", baseline = "n", new = "n", t(coef(curve68)))

## baseline Id:47

curve69 <- aci.df %>% 
  filter(id == "ryan.lh.3.47") %>%
  filter(date == "2023-12-27") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[69,] <- c(id = "ryan.lh.3.47", doy = "361", baseline = "n", new = "n", t(coef(curve69)))

## baseline Id:48

curve70 <- aci.df %>% 
  filter(id == "ryan.lh.4.48") %>%
  filter(date == "2023-12-27") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[70,] <- c(id = "ryan.lh.4.48", doy = "361", baseline = "n", new = "n", t(coef(curve70)))

################################################################################

## Date 12/28
# Curve 4 G1, ID 9
curve71 <- aci.df %>% 
  filter(id == "brit.lc.1.9") %>%
  filter(date == "2023-12-28") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 500)

aci.coefs[71,] <- c(id = "brit.lc.1.9", doy = "362", baseline = "n", new = "n", t(coef(curve71)))

## Id:10
curve72 <- aci.df %>% 
  filter(id == "brit.lc.2.10") %>%
  filter(date == "2023-12-28") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[72,] <- c(id = "brit.lc.2.10", doy = "362", baseline = "n", new = "n", t(coef(curve72)))

## Id:11
curve73 <- aci.df %>% 
  filter(id == "brit.lc.3.11") %>%
  filter(date == "2023-12-28") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[73,] <- c(id = "brit.lc.3.11", doy = "362", baseline = "n", new = "n", t(coef(curve73)))

## baseline Id:13
curve74 <- aci.df %>% 
  filter(id == "brit.lh.1.13") %>%
  filter(date == "2023-12-28") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 450)

aci.coefs[74,] <- c(id = "brit.lh.1.13", doy = "362", baseline = "n", new = "n", t(coef(curve74)))

## baseline Id:15
curve75 <- aci.df %>% 
  filter(id == "brit.lh.3.15") %>%
  filter(date == "2023-12-28") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[75,] <- c(id = "brit.lh.3.15", doy = "362", baseline = "n", new = "n", t(coef(curve75)))

## baseline Id:16
curve76 <- aci.df %>% 
  filter(id == "brit.lh.4.16") %>%
  filter(date == "2023-12-28") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[76,] <- c(id = "brit.lh.4.16", doy = "362", baseline = "n", new = "n", t(coef(curve76)))

#baseline Id:25
curve77 <- aci.df %>% 
  filter(id == "paul.lc.1.25") %>%
  filter(date == "2023-12-28") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE)

aci.coefs[77,] <- c(id = "paul.lc.1.25", doy = "362", baseline = "n", new = "n", t(coef(curve77)))

#baseline Id:26
curve78 <- aci.df %>% 
  filter(id == "paul.lc.2.26") %>%
  filter(date == "2023-12-28") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE)

aci.coefs[78,] <- c(id = "paul.lc.2.26", doy = "362", baseline = "n", new = "n", t(coef(curve78)))

## baseline Id:27

#curve79 <- aci.df %>% 
#  filter(id == "paul.lc.3.27") %>%
#  filter(date == "2023-12-28") %>%
#  fitaci(varnames = list(ALEAF = "A",
#                         Tleaf = "Tleaf",
#                         Ci = "Ci",
#                         PPFD = "Qin"),
#         fitTPU = TRUE, Tcorrect = FALSE)
#
#aci.coefs[79,] <- c(id = "paul.lc.3.27", doy = "362", baseline = "n", new = "n", t(coef(curve79)))

## baseline Id:29

curve80 <- aci.df %>% 
  filter(id == "paul.lh.1.29") %>%
  filter(date == "2023-12-28") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[80,] <- c(id = "paul.lh.1.29", doy = "362", baseline = "n", new = "n", t(coef(curve80)))

## baseline Id:30

curve81 <- aci.df %>% 
  filter(id == "paul.lh.2.30") %>%
  filter(date == "2023-12-28") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE)

#aci.coefs[81,] <- c(id = "paul.lh.2.30", doy = "362", baseline = "n", new = "n", t(coef(curve81)))

## baseline Id:31

curve82 <- aci.df %>% 
  filter(id == "paul.lh.3.31") %>%
  filter(date == "2023-12-28") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[82,] <- c(id = "paul.lh.3.31", doy = "362", baseline = "n", new = "n", t(coef(curve82)))

################################################################################

## Date 12/30
# curve 1 G3, ID 18
curve83 <- aci.df %>% 
  filter(id == "paul.hc.2.18") %>%
  filter(date == "2023-12-30") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[83,] <- c(id = "paul.hc.2.18", doy = "364", baseline = "n", new = "n", t(coef(curve83)))

## Id:19
curve84 <- aci.df %>% 
  filter(id == "paul.hc.3.19") %>%
  filter(date == "2023-12-30") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[84,] <- c(id = "paul.hc.3.19", doy = "364", baseline = "n", new = "n", t(coef(curve84)))

## Id:20
curve85 <- aci.df %>% 
  filter(id == "paul.hc.4.20") %>%
  filter(date == "2023-12-30") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[85,] <- c(id = "paul.hc.4.20", doy = "364", baseline = "n", new = "n", t(coef(curve85)))

## baseline Id:21
curve86 <- aci.df %>% 
  filter(id == "paul.hl.1.21") %>%
  filter(date == "2023-12-30") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[86,] <- c(id = "paul.hl.1.21", doy = "364", baseline = "n", new = "n", t(coef(curve86)))

## baseline Id:22
curve87 <- aci.df %>% 
  filter(id == "paul.hl.2.22") %>%
  filter(date == "2023-12-30") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[87,] <- c(id = "paul.hl.2.22", doy = "364", baseline = "n", new = "n", t(coef(curve87)))

## baseline Id:24
curve88 <- aci.df %>% 
  filter(id == "paul.hl.4.24") %>%
  filter(date == "2023-12-30") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[88,] <- c(id = "paul.hl.4.24", doy = "364", baseline = "n", new = "n", t(coef(curve88)))

## baseline Id:33
#curve89 <- aci.df %>% 
#  filter(id == "ryan.hc.3.33") %>%
#  filter(date == "2023-12-30") %>%
#  fitaci(varnames = list(ALEAF = "A",
#                         Tleaf = "Tleaf",
#                         Ci = "Ci",
#                         PPFD = "Qin"),
#         fitTPU = TRUE, Tcorrect = FALSE)

#aci.coefs[89,] <- c(id = "ryan.hc.3.33", doy = "364", baseline = "n", new = "n", t(coef(curve89)))

## baseline Id:34
curve90 <- aci.df %>% 
  filter(id == "ryan.hc.2.34") %>%
  filter(date == "2023-12-30") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[90,] <- c(id = "ryan.hc.2.34", doy = "364", baseline = "n", new = "n", t(coef(curve90)))

## baseline Id:35

curve91 <- aci.df %>% 
  filter(id == "ryan.hc.3.35") %>%
  filter(date == "2023-12-30") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[91,] <- c(id = "ryan.hc.3.35", doy = "364", baseline = "n", new = "n", t(coef(curve91)))

## baseline Id:37

curve92 <- aci.df %>% 
  filter(id == "ryan.hl.1.37") %>%
  filter(date == "2023-12-30") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[92,] <- c(id = "ryan.hl.1.37", doy = "364", baseline = "n", new = "n", t(coef(curve92)))

## baseline Id:38

curve93 <- aci.df %>% 
  filter(id == "ryan.hl.2.38") %>%
  filter(date == "2023-12-30") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[93,] <- c(id = "ryan.hl.2.38", doy = "364", baseline = "n", new = "n", t(coef(curve93)))

## baseline Id:40

curve94 <- aci.df %>% 
  filter(id == "ryan.hl.4.40") %>%
  filter(date == "2023-12-30") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[94,] <- c(id = "ryan.hl.4.40", doy = "364", baseline = "n", new = "n", t(coef(curve94)))

################################################################################

## Date 12/31
# Curve 2 G2, ID 1
curve95 <- aci.df %>% 
  filter(id == "brit.hc.1.1") %>%
  filter(date == "2023-12-31") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[95,] <- c(id = "brit.hc.1.1", doy = "365", baseline = "n", new = "n", t(coef(curve95)))

## Id:2
curve96 <- aci.df %>% 
  filter(id == "brit.hc.2.2") %>%
  filter(date == "2023-12-31") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 500)

aci.coefs[96,] <- c(id = "brit.hc.2.2", doy = "365", baseline = "n", new = "n", t(coef(curve96)))

## Id:5
curve97 <- aci.df %>% 
  filter(id == "brit.hl.1.5") %>%
  filter(date == "2023-12-31") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)
plot(curve96)


aci.coefs[97,] <- c(id = "brit.hl.1.5", doy = "365", baseline = "n", new = "n", t(coef(curve97)))

## baseline Id:6
curve98 <- aci.df %>% 
  filter(id == "brit.hl.2.6") %>%
  filter(date == "2023-12-31") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[98,] <- c(id = "brit.hl.2.6", doy = "365", baseline = "n", new = "n", t(coef(curve98)))

## baseline Id:7
curve99 <- aci.df %>% 
  filter(id == "brit.hl.3.7") %>%
  filter(date == "2023-12-31") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[99,] <- c(id = "brit.hl.3.7", doy = "365", baseline = "n", new = "n", t(coef(curve99)))

## baseline Id:41
curve100 <- aci.df %>% 
  filter(id == "ryan.lc.1.41") %>%
  filter(date == "2023-12-31") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[100,] <- c(id = "ryan.lc.1.41", doy = "365", baseline = "n", new = "n", t(coef(curve100)))

## baseline Id:43
curve282 <- aci.df %>% 
  filter(id == "ryan.lc.3.43") %>%
  filter(date == "2023-12-31") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[282,] <- c(id = "ryan.lc.3.43", doy = "365", baseline = "n", new = "n", t(coef(curve282)))

## baseline Id:44
curve101 <- aci.df %>% 
  filter(id == "ryan.lc.4.44") %>%
  filter(date == "2023-12-31") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[101,] <- c(id = "ryan.lc.4.44", doy = "365", baseline = "n", new = "n", t(coef(curve101)))

## baseline Id:45

curve102 <- aci.df %>% 
  filter(id == "ryan.lh.1.45") %>%
  filter(date == "2023-12-31") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[102,] <- c(id = "ryan.lh.1.45", doy = "365", baseline = "n", new = "n", t(coef(curve102)))

## baseline Id:47

curve103 <- aci.df %>% 
  filter(id == "ryan.lh.3.47") %>%
  filter(date == "2023-12-31") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[103,] <- c(id = "ryan.lh.3.47", doy = "365", baseline = "n", new = "n", t(coef(curve103)))

## baseline Id:48

curve104 <- aci.df %>% 
  filter(id == "ryan.lh.4.48") %>%
  filter(date == "2023-12-31") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[104,] <- c(id = "ryan.lh.4.48", doy = "365", baseline = "n", new = "n", t(coef(curve104)))

#################################################################################

## Date 1/1
# Curve 5 G1

## Id:10
curve105 <- aci.df %>% 
  filter(id == "brit.lc.2.10") %>%
  filter(date == "2024-01-01") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"), 
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[105,] <- c(id = "brit.lc.2.10", doy = "1", baseline = "n", new = "n", t(coef(curve105)))

## Id:11
curve106 <- aci.df %>% 
  filter(id == "brit.lc.3.11") %>%
  filter(date == "2024-01-01") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[106,] <- c(id = "brit.lc.3.11", doy = "1", baseline = "n", new = "n", t(coef(curve106)))

## baseline Id:13
curve107 <- aci.df %>% 
  filter(id == "brit.lh.1.13") %>%
  filter(date == "2024-01-01") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[107,] <- c(id = "brit.lh.1.13", doy = "1", baseline = "n", new = "n", t(coef(curve107)))

## baseline Id:15
curve108 <- aci.df %>% 
  filter(id == "brit.lh.3.15") %>%
  filter(date == "2024-01-01") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[108,] <- c(id = "brit.lh.3.15", doy = "1", baseline = "n", new = "n", t(coef(curve108)))

## baseline Id:16
curve109 <- aci.df %>% 
  filter(id == "brit.lh.4.16") %>%
  filter(date == "2024-01-01") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[109,] <- c(id = "brit.lh.4.16", doy = "1", baseline = "n", new = "n", t(coef(curve109)))

## baseline Id:26
curve110 <- aci.df %>% 
  filter(id == "paul.lc.2.26") %>%
  filter(date == "2024-01-01") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[110,] <- c(id = "paul.lc.2.26", doy = "1", baseline = "n", new = "n", t(coef(curve110)))

## baseline Id:27

curve111 <- aci.df %>% 
  filter(id == "paul.lc.3.27") %>%
  filter(date == "2024-01-01") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 500)

aci.coefs[111,] <- c(id = "paul.lc.3.27", doy = "1", baseline = "n", new = "n", t(coef(curve111)))

## baseline Id:29
curve112 <- aci.df %>% 
  filter(id == "paul.lh.1.29") %>%
  filter(date == "2024-01-01") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[112,] <- c(id = "paul.lh.1.29", doy = "1", baseline = "n", new = "n", t(coef(curve112)))

## baseline Id:31

curve113 <- aci.df %>% 
  filter(id == "paul.lh.3.31") %>%
  filter(date == "2024-01-01") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[113,] <- c(id = "paul.lh.3.31", doy = "1", baseline = "n", new = "n", t(coef(curve113)))

################################################################################

## Date 1/2
# curve 2 G3, ID 18
curve114 <- aci.df %>% 
  filter(id == "paul.hc.2.18") %>%
  filter(date == "2024-01-02") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[114,] <- c(id = "paul.hc.2.18", doy = "2", baseline = "n", new = "n", t(coef(curve114)))

## Id:19

curve115 <- aci.df %>% 
  filter(id == "paul.hc.3.19") %>%
  filter(date == "2024-01-02") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, useRd = TRUE, citransition = 400)

aci.coefs[115,] <- c(id = "paul.hc.3.19", doy = "2", baseline = "n", new = "n", t(coef(curve115)))

## Id:20
curve116 <- aci.df %>% 
  filter(id == "paul.hc.4.20") %>%
  filter(date == "2024-01-02") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 600)

aci.coefs[116,] <- c(id = "paul.hc.4.20", doy = "2", baseline = "n", new = "n", t(coef(curve116)))

## baseline Id:21
curve117 <- aci.df %>% 
  filter(id == "paul.hl.1.21") %>%
  filter(date == "2024-01-02") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[117,] <- c(id = "paul.hl.1.21", doy = "2", baseline = "n", new = "n", t(coef(curve117)))

## baseline Id:22
curve118 <- aci.df %>% 
  filter(id == "paul.hl.2.22") %>%
  filter(date == "2024-01-02") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[118,] <- c(id = "paul.hl.2.22", doy = "2", baseline = "n", new = "n", t(coef(curve118)))

## baseline Id:24
curve119 <- aci.df %>% 
  filter(id == "paul.hl.4.24") %>%
  filter(date == "2024-01-02") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[119,] <- c(id = "paul.hl.4.24", doy = "2", baseline = "n", new = "n", t(coef(curve119)))

## baseline Id:34
curve120 <- aci.df %>% 
  filter(id == "ryan.hc.2.34") %>%
  filter(date == "2024-01-02") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[120,] <- c(id = "ryan.hc.2.34", doy = "2", baseline = "n", new = "n", t(coef(curve120)))

## baseline Id:35

curve121 <- aci.df %>% 
  filter(id == "ryan.hc.3.35") %>%
  filter(date == "2024-01-02") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[121,] <- c(id = "ryan.hc.3.35", doy = "2", baseline = "n", new = "n", t(coef(curve121)))

## baseline Id:37

curve122 <- aci.df %>% 
  filter(id == "ryan.hl.1.37") %>%
  filter(date == "2024-01-02") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[122,] <- c(id = "ryan.hl.1.37", doy = "2", baseline = "n", new = "n", t(coef(curve122)))

## baseline Id:38

curve123 <- aci.df %>% 
  filter(id == "ryan.hl.2.38") %>%
  filter(date == "2024-01-02") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[123,] <- c(id = "ryan.hl.2.38", doy = "2", baseline = "n", new = "n", t(coef(curve123)))

## baseline Id:40

curve124 <- aci.df %>% 
  filter(id == "ryan.hl.4.40") %>%
  filter(date == "2024-01-02") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[124,] <- c(id = "ryan.hl.4.40", doy = "2", baseline = "n", new = "n", t(coef(curve124)))

#################################################################################

## Date 1/3
# Curve 3 G2, ID 1
curve125 <- aci.df %>% 
  filter(id == "brit.hc.1.1") %>%
  filter(date == "2024-01-03") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 500)

aci.coefs[125,] <- c(id = "brit.hc.1.1", doy = "3", baseline = "n", new = "n", t(coef(curve125)))

## Id:2
curve126 <- aci.df %>% 
  filter(id == "brit.hc.2.2") %>%
  filter(date == "2024-01-03") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[126,] <- c(id = "brit.hc.2.2", doy = "3", baseline = "n", new = "n", t(coef(curve126)))

## Id:5
curve127 <- aci.df %>% 
  filter(id == "brit.hl.1.5") %>%
  filter(date == "2024-01-03") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 500)

aci.coefs[127,] <- c(id = "brit.hl.1.5", doy = "3", baseline = "n", new = "n", t(coef(curve127)))

## baseline Id:6
curve128 <- aci.df %>% 
  filter(id == "brit.hl.2.6") %>%
  filter(date == "2024-01-03") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[128,] <- c(id = "brit.hl.2.6", doy = "3", baseline = "n", new = "n", t(coef(curve128)))

## baseline Id:7
curve129 <- aci.df %>% 
  filter(id == "brit.hl.3.7") %>%
  filter(date == "2024-01-03") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[129,] <- c(id = "brit.hl.3.7", doy = "3", baseline = "n", new = "n", t(coef(curve129)))

## baseline Id:41
#curve130 <- aci.df %>% 
#  filter(id == "ryan.lc.1.41") %>%
#  filter(date == "2024-01-03") %>%
#  fitaci(varnames = list(ALEAF = "A",
#                         Tleaf = "Tleaf",
#                         Ci = "Ci",
#                         PPFD = "Qin"),
#         fitTPU = TRUE, Tcorrect = FALSE)
#
#aci.coefs[130,] <- c(id = "ryan.lc.1.41", doy = "3", baseline = "n", new = "n", t(coef(curve130)))

## baseline Id:43
curve131 <- aci.df %>% 
  filter(id == "ryan.lc.3.43") %>%
  filter(date == "2024-01-03") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[131,] <- c(id = "ryan.lc.3.43", doy = "3", baseline = "n", new = "n", t(coef(curve131)))

## baseline Id:44
curve132 <- aci.df %>% 
  filter(id == "ryan.lc.4.44") %>%
  filter(date == "2024-01-03") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[132,] <- c(id = "ryan.lc.4.44", doy = "3", baseline = "n", new = "n", t(coef(curve132)))

## baseline Id:45

curve133 <- aci.df %>% 
  filter(id == "ryan.lh.1.45") %>%
  filter(date == "2024-01-03") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[133,] <- c(id = "ryan.lh.1.45", doy = "3", baseline = "n", new = "n", t(coef(curve133)))

## baseline Id:47

curve134 <- aci.df %>% 
  filter(id == "ryan.lh.3.47") %>%
  filter(date == "2024-01-03") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[134,] <- c(id = "ryan.lh.3.47", doy = "3", baseline = "n", new = "n", t(coef(curve134)))

## baseline Id:48

curve135 <- aci.df %>% 
  filter(id == "ryan.lh.4.48") %>%
  filter(date == "2024-01-03") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[135,] <- c(id = "ryan.lh.4.48", doy = "3", baseline = "n", new = "n", t(coef(curve135)))

#################################################################################

## Date 1/4
# Curve 6 G1

## Id:10
curve136 <- aci.df %>% 
  filter(id == "brit.lc.2.10") %>%
  filter(date == "2024-01-04") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 425)

aci.coefs[136,] <- c(id = "brit.lc.2.10", doy = "4", baseline = "n", new = "n", t(coef(curve136)))

## Id:11
curve137 <- aci.df %>% 
  filter(id == "brit.lc.3.11") %>%
  filter(date == "2024-01-04") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[137,] <- c(id = "brit.lc.3.11", doy = "4", baseline = "n", new = "n", t(coef(curve137)))

## baseline Id:13
curve138 <- aci.df %>% 
  filter(id == "brit.lh.1.13") %>%
  filter(date == "2024-01-04") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[138,] <- c(id = "brit.lh.1.13", doy = "4", baseline = "n", new = "n", t(coef(curve138)))

## baseline Id:15
curve139 <- aci.df %>% 
  filter(id == "brit.lh.3.15") %>%
  filter(date == "2024-01-04") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[139,] <- c(id = "brit.lh.3.15", doy = "4", baseline = "n", new = "n", t(coef(curve139)))

## baseline Id:16
curve140 <- aci.df %>% 
  filter(id == "brit.lh.4.16") %>%
  filter(date == "2024-01-04") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[140,] <- c(id = "brit.lh.4.16", doy = "4", baseline = "n", new = "n", t(coef(curve140)))

## baseline Id:26
curve141 <- aci.df %>% 
  filter(id == "paul.lc.2.26") %>%
  filter(date == "2024-01-04") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[141,] <- c(id = "paul.lc.2.26", doy = "4", baseline = "n", new = "n", t(coef(curve141)))

## baseline Id:27

curve142 <- aci.df %>% 
  filter(id == "paul.lc.3.27") %>%
  filter(date == "2024-01-04") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[142,] <- c(id = "paul.lc.3.27", doy = "4", baseline = "n", new = "n", t(coef(curve142)))

## baseline Id:29

#curve143 <- aci.df %>% 
#  filter(id == "paul.lh.1.29") %>%
#  filter(date == "2024-01-04") %>%
#  fitaci(varnames = list(ALEAF = "A",
#                         Tleaf = "Tleaf",
#                         Ci = "Ci",
#                         PPFD = "Qin"),
#         fitTPU = TRUE, Tcorrect = FALSE)
#
#aci.coefs[143,] <- c(id = "paul.lh.1.29", doy = "4", baseline = "n", new = "n", t(coef(curve143)))

###################################################################################

## Date 1/5
# curve 3 G3, ID 18
curve144 <- aci.df %>% 
  filter(id == "paul.hc.2.18") %>%
  filter(date == "2024-01-05") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE)

aci.coefs[144,] <- c(id = "paul.hc.2.18", doy = "5", baseline = "n", new = "n", t(coef(curve144)))

## Id:19
curve145 <- aci.df %>% 
  filter(id == "paul.hc.3.19") %>%
  filter(date == "2024-01-05") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[145,] <- c(id = "paul.hc.3.19", doy = "5", baseline = "n", new = "n", t(coef(curve145)))

## Id:20
curve146 <- aci.df %>% 
  filter(id == "paul.hc.4.20") %>%
  filter(date == "2024-01-05") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 500)

aci.coefs[146,] <- c(id = "paul.hc.4.20", doy = "5", baseline = "n", new = "n", t(coef(curve146)))

## baseline Id:21
curve147 <- aci.df %>% 
  filter(id == "paul.hl.1.21") %>%
  filter(date == "2024-01-05") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[147,] <- c(id = "paul.hl.1.21", doy = "5", baseline = "n", new = "n", t(coef(curve147)))

## baseline Id:22
#curve148 <- aci.df %>% 
#  filter(id == "paul.hl.2.22") %>%
#  filter(date == "2024-01-05") %>%
#  fitaci(varnames = list(ALEAF = "A",
#                         Tleaf = "Tleaf",
#                         Ci = "Ci",
#                         PPFD = "Qin"),
#         fitTPU = TRUE, Tcorrect = FALSE)
#
#aci.coefs[148,] <- c(id = "paul.hl.2.22", doy = "5", baseline = "n", new = "n", t(coef(curve148)))

## baseline Id:24
curve149 <- aci.df %>% 
  filter(id == "paul.hl.4.24") %>%
  filter(date == "2024-01-05") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 500)

aci.coefs[149,] <- c(id = "paul.hl.4.24", doy = "5", baseline = "n", new = "n", t(coef(curve149)))

## baseline Id:34
curve150 <- aci.df %>% 
  filter(id == "ryan.hc.2.34") %>%
  filter(date == "2024-01-05") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[150,] <- c(id = "ryan.hc.2.34", doy = "5", baseline = "n", new = "n", t(coef(curve150)))

## baseline Id:35

curve151 <- aci.df %>% 
  filter(id == "ryan.hc.3.35") %>%
  filter(date == "2024-01-05") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[151,] <- c(id = "ryan.hc.3.35", doy = "5", baseline = "n", new = "n", t(coef(curve151)))

## baseline Id:37

curve152 <- aci.df %>% 
  filter(id == "ryan.hl.1.37") %>%
  filter(date == "2024-01-05") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[152,] <- c(id = "ryan.hl.1.37", doy = "5", baseline = "n", new = "n", t(coef(curve152)))

## baseline Id:38

curve153 <- aci.df %>% 
  filter(id == "ryan.hl.2.38") %>%
  filter(date == "2024-01-05") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[153,] <- c(id = "ryan.hl.2.38", doy = "5", baseline = "n", new = "n", t(coef(curve153)))

## baseline Id:40

curve154 <- aci.df %>% 
  filter(id == "ryan.hl.4.40") %>%
  filter(date == "2024-01-05") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"), 
         fitTPU = TRUE, Tcorrect = FALSE, useRd = TRUE, citransition = 400)

aci.coefs[154,] <- c(id = "ryan.hl.4.40", doy = "5", baseline = "n", new = "n", t(coef(curve154)))

################################################################################

## Date 1/7
# Curve 4 G2, ID 1
curve155 <- aci.df %>% 
  filter(id == "brit.hc.1.1") %>%
  filter(date == "2024-01-07") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[155,] <- c(id = "brit.hc.1.1", doy = "7", baseline = "n", new = "n", t(coef(curve155)))

## Id:2
curve156 <- aci.df %>% 
  filter(id == "brit.hc.2.2") %>%
  filter(date == "2024-01-07") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[156,] <- c(id = "brit.hc.2.2", doy = "7", baseline = "n", new = "n", t(coef(curve156)))

## Id:5
curve157 <- aci.df %>% 
  filter(id == "brit.hl.1.5") %>%
  filter(date == "2024-01-07") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[157,] <- c(id = "brit.hl.1.5", doy = "7", baseline = "n", new = "n", t(coef(curve157)))

## baseline Id:6
curve158 <- aci.df %>% 
  filter(id == "brit.hl.2.6") %>%
  filter(date == "2024-01-07") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[158,] <- c(id = "brit.hl.2.6", doy = "7", baseline = "n", new = "n", t(coef(curve158)))

## baseline Id:7
curve159 <- aci.df %>% 
  filter(id == "brit.hl.3.7") %>%
  filter(date == "2024-01-07") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[159,] <- c(id = "brit.hl.3.7", doy = "7", baseline = "n", new = "n", t(coef(curve159)))

## baseline Id:41
curve160 <- aci.df %>% 
  filter(id == "ryan.lc.1.41") %>%
  filter(date == "2024-01-07") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[160,] <- c(id = "ryan.lc.1.41", doy = "7", baseline = "n", new = "n", t(coef(curve160)))

## baseline Id:43
curve161 <- aci.df %>% 
  filter(id == "ryan.lc.3.43") %>%
  filter(date == "2024-01-07") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[161,] <- c(id = "ryan.lc.3.43", doy = "7", baseline = "n", new = "n", t(coef(curve161)))

## baseline Id:44
curve162 <- aci.df %>% 
  filter(id == "ryan.lc.4.44") %>%
  filter(date == "2024-01-07") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[162,] <- c(id = "ryan.lc.4.44", doy = "7", baseline = "n", new = "n", t(coef(curve162)))

## baseline Id:45

curve163 <- aci.df %>% 
  filter(id == "ryan.lh.1.45") %>%
  filter(date == "2024-01-07") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[163,] <- c(id = "ryan.lh.1.45", doy = "7", baseline = "n", new = "n", t(coef(curve163)))

## baseline Id:47

curve164 <- aci.df %>% 
  filter(id == "ryan.lh.3.47") %>%
  filter(date == "2024-01-07") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[164,] <- c(id = "ryan.lh.3.47", doy = "7", baseline = "n", new = "n", t(coef(curve164)))

## baseline Id:48

curve165 <- aci.df %>% 
  filter(id == "ryan.lh.4.48") %>%
  filter(date == "2024-01-07") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[165,] <- c(id = "ryan.lh.4.48", doy = "7", baseline = "n", new = "n", t(coef(curve165)))

#################################################################################

## Date 1/9
# curve 4 G3, ID 20 #Contains a shape that is not conducive to fitting 

#curve166 <- aci.df %>% 
#  filter(id == "paul.hc.4.20") %>%
#  filter(date == "2024-01-09") %>%
#  fitaci(varnames = list(ALEAF = "A",
#                         Tleaf = "Tleaf",
#                         Ci = "Ci",
#                         PPFD = "Qin"),
#         fitTPU = TRUE, Tcorrect = FALSE, citransition = 500)
#
#aci.coefs[166,] <- c(id = "paul.hc.4.20", doy = "9", baseline = "n", new = "n", t(coef(curve166)))

## baseline Id:21
curve167 <- aci.df %>% 
  filter(id == "paul.hl.1.21") %>%
  filter(date == "2024-01-09") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[167,] <- c(id = "paul.hl.1.21", doy = "9", baseline = "n", new = "n", t(coef(curve167)))

## baseline Id:24
curve168 <- aci.df %>% 
  filter(id == "paul.hl.4.24") %>%
  filter(date == "2024-01-09") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[168,] <- c(id = "paul.hl.4.24", doy = "9", baseline = "n", new = "n", t(coef(curve168)))

## baseline Id:35

curve169 <- aci.df %>% 
  filter(id == "ryan.hc.3.35") %>%
  filter(date == "2024-01-09") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)


aci.coefs[169,] <- c(id = "ryan.hc.3.35", doy = "9", baseline = "n", new = "n", t(coef(curve169)))

## baseline Id:37

curve170 <- aci.df %>% 
  filter(id == "ryan.hl.1.37") %>%
  filter(date == "2024-01-09") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[170,] <- c(id = "ryan.hl.1.37", doy = "9", baseline = "n", new = "n", t(coef(curve170)))

## baseline Id:38

curve171 <- aci.df %>% 
  filter(id == "ryan.hl.2.38") %>%
  filter(date == "2024-01-09") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[171,] <- c(id = "ryan.hl.2.38", doy = "9", baseline = "n", new = "n", t(coef(curve171)))

## baseline Id:40

curve172 <- aci.df %>% 
  filter(id == "ryan.hl.4.40") %>%
  filter(date == "2024-01-09") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[172,] <- c(id = "ryan.hl.4.40", doy = "9", baseline = "n", new = "n", t(coef(curve172)))

################################################################################

## Date 1/10
# Curve 5 G2, ID 1
#curve173 <- aci.df %>% 
#  filter(id == "brit.hc.1.1") %>%
#  filter(date == "2024-01-10") %>%
#  fitaci(varnames = list(ALEAF = "A",
#                         Tleaf = "Tleaf",
#                         Ci = "Ci",
#                         PPFD = "Qin"),
#         fitTPU = TRUE, Tcorrect = FALSE)
#
#aci.coefs[173,] <- c(id = "brit.hc.1.1", doy = "10", baseline = "n", new = "n", t(coef(curve173)))

## Id:2
curve174 <- aci.df %>% 
  filter(id == "brit.hc.2.2") %>%
  filter(date == "2024-01-10") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 500)

aci.coefs[174,] <- c(id = "brit.hc.2.2", doy = "10", baseline = "n", new = "n", t(coef(curve174)))

## Id:5
curve175 <- aci.df %>% 
  filter(id == "brit.hl.1.5") %>%
  filter(date == "2024-01-10") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 475)

aci.coefs[175,] <- c(id = "brit.hl.1.5", doy = "10", baseline = "n", new = "n", t(coef(curve175)))

## baseline Id:6
curve176 <- aci.df %>% 
  filter(id == "brit.hl.2.6") %>%
  filter(date == "2024-01-10") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[176,] <- c(id = "brit.hl.2.6", doy = "10", baseline = "n", new = "n", t(coef(curve176)))

## baseline Id:7
curve177 <- aci.df %>% 
  filter(id == "brit.hl.3.7") %>%
  filter(date == "2024-01-10") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[177,] <- c(id = "brit.hl.3.7", doy = "10", baseline = "n", new = "n", t(coef(curve177)))

## baseline Id:41
curve178 <- aci.df %>% 
  filter(id == "ryan.lc.1.41") %>%
  filter(date == "2024-01-10") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[178,] <- c(id = "ryan.lc.1.41", doy = "10", baseline = "n", new = "n", t(coef(curve178)))

## baseline Id:43
curve179 <- aci.df %>% 
  filter(id == "ryan.lc.3.43") %>%
  filter(date == "2024-01-10") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[179,] <- c(id = "ryan.lc.3.43", doy = "10", baseline = "n", new = "n", t(coef(curve179)))

## baseline Id:44
curve180 <- aci.df %>% 
  filter(id == "ryan.lc.4.44") %>%
  filter(date == "2024-01-10") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE)

aci.coefs[180,] <- c(id = "ryan.lc.4.44", doy = "10", baseline = "n", new = "n", t(coef(curve180)))

## baseline Id:45

curve181 <- aci.df %>% 
  filter(id == "ryan.lh.1.45") %>%
  filter(date == "2024-01-10") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[181,] <- c(id = "ryan.lh.1.45", doy = "10", baseline = "n", new = "n", t(coef(curve181)))

## baseline Id:47

curve182 <- aci.df %>% 
  filter(id == "ryan.lh.3.47") %>%
  filter(date == "2024-01-10") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[182,] <- c(id = "ryan.lh.3.47", doy = "10", baseline = "n", new = "n", t(coef(curve182)))

## baseline Id:48

curve183 <- aci.df %>% 
  filter(id == "ryan.lh.4.48") %>%
  filter(date == "2024-01-10") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[183,] <- c(id = "ryan.lh.4.48", doy = "10", baseline = "n", new = "n", t(coef(curve183)))

##################################################################################

## Date 1/11
# Curve 1 new G1, ID 9
curve184 <- aci.df %>% 
  filter(id == "brit.lc.1.9") %>%
  filter(date == "2024-01-11") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[184,] <- c(id = "brit.lc.1.9", doy = "11", baseline = "y", new = "y", t(coef(curve184)))

## Id:10
curve185 <- aci.df %>% 
  filter(id == "brit.lc.2.10") %>%
  filter(date == "2024-01-11") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[185,] <- c(id = "brit.lc.2.10", doy = "11", baseline = "y", new = "y", t(coef(curve185)))

## Id:11
#curve186 <- aci.df %>% 
#  filter(id == "brit.lc.3.11") %>%
#  filter(date == "2024-01-11") %>%
#  fitaci(varnames = list(ALEAF = "A",
#                         Tleaf = "Tleaf",
#                         Ci = "Ci",
#                         PPFD = "Qin"),
#         fitTPU = TRUE, Tcorrect = FALSE)
#
#aci.coefs[186,] <- c(id = "brit.lc.3.11", doy = "11", baseline = "y", new = "y", t(coef(curve186)))

## baseline Id:13
#curve187 <- aci.df %>% 
#  filter(id == "brit.lh.1.13") %>%
#  filter(date == "2024-01-11") %>%
#  fitaci(varnames = list(ALEAF = "A",
#                         Tleaf = "Tleaf",
#                         Ci = "Ci",
#                         PPFD = "Qin"),
#         fitTPU = TRUE, Tcorrect = FALSE)
#
#aci.coefs[187,] <- c(id = "brit.lh.1.13", doy = "11", baseline = "y", new = "y", t(coef(curve187)))

## baseline Id:15
curve188 <- aci.df %>% 
  filter(id == "brit.lh.3.15") %>%
  filter(date == "2024-01-11") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 350)

aci.coefs[188,] <- c(id = "brit.lh.3.15", doy = "11", baseline = "y", new = "y", t(coef(curve188)))

## baseline Id:16
curve189 <- aci.df %>% 
  filter(id == "brit.lh.4.16") %>%
  filter(date == "2024-01-11") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[189,] <- c(id = "brit.lh.4.16", doy = "11", baseline = "y", new = "y", t(coef(curve189)))

## baseline Id:25
curve190 <- aci.df %>% 
  filter(id == "paul.lc.1.25") %>%
  filter(date == "2024-01-11") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[190,] <- c(id = "paul.lc.1.25", doy = "11", baseline = "y", new = "y", t(coef(curve190)))

## baseline Id:26
#curve191 <- aci.df %>% 
#  filter(id == "paul.lc.2.26") %>%
#  filter(date == "2024-01-11") %>%
#  fitaci(varnames = list(ALEAF = "A",
#                         Tleaf = "Tleaf",
#                         Ci = "Ci",
#                         PPFD = "Qin"),
#         fitTPU = TRUE, Tcorrect = FALSE)
#
#aci.coefs[191,] <- c(id = "paul.lc.2.26", doy = "11", baseline = "y", new = "y", t(coef(curve191)))

## baseline Id:27

#curve192 <- aci.df %>% 
#  filter(id == "paul.lc.3.27") %>%
#  filter(date == "2024-01-11") %>%
#  fitaci(varnames = list(ALEAF = "A",
#                         Tleaf = "Tleaf",
#                         Ci = "Ci",
#                         PPFD = "Qin"),
#         fitTPU = TRUE, Tcorrect = FALSE, citransition = 500)
#
#aci.coefs[192,] <- c(id = "paul.lc.3.27", doy = "11", baseline = "y", new = "y", t(coef(curve192)))

## baseline Id:29

curve193 <- aci.df %>% 
  filter(id == "paul.lh.1.29") %>%
  filter(date == "2024-01-11") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[193,] <- c(id = "paul.lh.1.29", doy = "11", baseline = "y", new = "y", t(coef(curve193)))

## baseline Id:30

curve194 <- aci.df %>% 
  filter(id == "paul.lh.2.30") %>%
  filter(date == "2024-01-11") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[194,] <- c(id = "paul.lh.2.30", doy = "11", baseline = "y", new = "y", t(coef(curve194)))

## baseline Id:31

curve195 <- aci.df %>% 
  filter(id == "paul.lh.3.31") %>%
  filter(date == "2024-01-11") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[195,] <- c(id = "paul.lh.3.31", doy = "11", baseline = "y", new = "y", t(coef(curve195)))

#################################################################################

## Date 1/12
# curve 5 G3, ID 20

#curve196 <- aci.df %>% 
#  filter(id == "paul.hc.4.20") %>%
#  filter(date == "2024-01-12") %>%
#  fitaci(varnames = list(ALEAF = "A",
#                         Tleaf = "Tleaf",
#                         Ci = "Ci",
#                         PPFD = "Qin"),
#         fitTPU = TRUE, Tcorrect = FALSE)
#
#aci.coefs[196,] <- c(id = "paul.hc.4.20", doy = "12", baseline = "n", new = "n", t(coef(curve196)))

## baseline Id:21
curve197 <- aci.df %>% 
  filter(id == "paul.hl.1.21") %>%
  filter(date == "2024-01-12") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 450)

aci.coefs[197,] <- c(id = "paul.hl.1.21", doy = "12", baseline = "n", new = "n", t(coef(curve197)))

## baseline Id:24
curve198 <- aci.df %>% 
  filter(id == "paul.hl.4.24") %>%
  filter(date == "2024-01-12") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 390)

aci.coefs[198,] <- c(id = "paul.hl.4.24", doy = "12", baseline = "n", new = "n", t(coef(curve198)))

## baseline Id:35

curve199 <- aci.df %>% 
  filter(id == "ryan.hc.3.35") %>%
  filter(date == "2024-01-12") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[199,] <- c(id = "ryan.hc.3.35", doy = "12", baseline = "n", new = "n", t(coef(curve199)))

## baseline Id:37

curve200 <- aci.df %>% 
  filter(id == "ryan.hl.1.37") %>%
  filter(date == "2024-01-12") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[200,] <- c(id = "ryan.hl.1.37", doy = "12", baseline = "n", new = "n", t(coef(curve200)))

## baseline Id:38

curve201 <- aci.df %>% 
  filter(id == "ryan.hl.2.38") %>%
  filter(date == "2024-01-12") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[201,] <- c(id = "ryan.hl.2.38", doy = "12", baseline = "n", new = "n", t(coef(curve201)))

## baseline Id:40

curve202 <- aci.df %>% 
  filter(id == "ryan.hl.4.40") %>%
  filter(date == "2024-01-12") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[202,] <- c(id = "ryan.hl.4.40", doy = "12", baseline = "n", new = "n", t(coef(curve202)))

####################################################################################

## Date 1/14
# Baseline new G2, ID 1
curve203 <- aci.df %>% 
  filter(id == "brit.hc.1.1") %>%
  filter(date == "2024-01-14") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[203,] <- c(id = "brit.hc.1.1", doy = "14", baseline = "y", new = "y", t(coef(curve203)))

## Id:2
curve204 <- aci.df %>% 
  filter(id == "brit.hc.2.2") %>%
  filter(date == "2024-01-14") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[204,] <- c(id = "brit.hc.2.2", doy = "14", baseline = "y", new = "y", t(coef(curve204)))

## Id:5
curve205 <- aci.df %>% 
  filter(id == "brit.hl.1.5") %>%
  filter(date == "2024-01-14") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[205,] <- c(id = "brit.hl.1.5", doy = "14", baseline = "y", new = "y", t(coef(curve205)))

## baseline Id:6
curve206 <- aci.df %>% 
  filter(id == "brit.hl.2.6") %>%
  filter(date == "2024-01-14") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[206,] <- c(id = "brit.hl.2.6", doy = "14", baseline = "y", new = "y", t(coef(curve206)))

## baseline Id:7
curve207 <- aci.df %>% 
  filter(id == "brit.hl.3.7") %>%
  filter(date == "2024-01-14") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[207,] <- c(id = "brit.hl.3.7", doy = "14", baseline = "y", new = "y", t(coef(curve207)))

## baseline Id:41
curve208 <- aci.df %>% 
  filter(id == "ryan.lc.1.41") %>%
  filter(date == "2024-01-14") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[208,] <- c(id = "ryan.lc.1.41", doy = "14", baseline = "y", new = "y", t(coef(curve208)))

## baseline Id:43
curve209 <- aci.df %>% 
  filter(id == "ryan.lc.3.43") %>%
  filter(date == "2024-01-14") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[209,] <- c(id = "ryan.lc.3.43", doy = "14", baseline = "y", new = "y", t(coef(curve209)))

## baseline Id:44
curve210 <- aci.df %>% 
  filter(id == "ryan.lc.4.44") %>%
  filter(date == "2024-01-14") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[210,] <- c(id = "ryan.lc.4.44", doy = "14", baseline = "y", new = "y", t(coef(curve210)))

## baseline Id:45

curve211 <- aci.df %>% 
  filter(id == "ryan.lh.1.45") %>%
  filter(date == "2024-01-14") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[211,] <- c(id = "ryan.lh.1.45", doy = "14", baseline = "y", new = "y", t(coef(curve211)))

## baseline Id:47

curve212 <- aci.df %>% 
  filter(id == "ryan.lh.3.47") %>%
  filter(date == "2024-01-14") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[212,] <- c(id = "ryan.lh.3.47", doy = "14", baseline = "y", new = "y", t(coef(curve212)))

## baseline Id:48

curve213 <- aci.df %>% 
  filter(id == "ryan.lh.4.48") %>%
  filter(date == "2024-01-14") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[213,] <- c(id = "ryan.lh.4.48", doy = "14", baseline = "y", new = "y", t(coef(curve213)))

##################################################################################

## Date 1/15
# Curve 2 new G1, ID 9
curve213 <- aci.df %>% 
  filter(id == "brit.lc.1.9") %>%
  filter(date == "2024-01-15") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[213,] <- c(id = "brit.lc.1.9", doy = "15", baseline = "n", new = "y", t(coef(curve213)))

## Id:10
curve214 <- aci.df %>% 
  filter(id == "brit.lc.2.10") %>%
  filter(date == "2024-01-15") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[214,] <- c(id = "brit.lc.2.10", doy = "15", baseline = "n", new = "y", t(coef(curve214)))

## Id:11
curve215 <- aci.df %>% 
  filter(id == "brit.lc.3.11") %>%
  filter(date == "2024-01-15") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[283,] <- c(id = "brit.lc.3.11", doy = "15", baseline = "n", new = "y", t(coef(curve215)))

## baseline Id:13
curve215 <- aci.df %>% 
  filter(id == "brit.lh.1.13") %>%
  filter(date == "2024-01-15") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[215,] <- c(id = "brit.lh.1.13", doy = "15", baseline = "n", new = "y", t(coef(curve215)))

## baseline Id:15
#curve216 <- aci.df %>% 
#  filter(id == "brit.lh.3.15") %>%
#  filter(date == "2024-01-15") %>%
#  fitaci(varnames = list(ALEAF = "A",
#                         Tleaf = "Tleaf",
#                         Ci = "Ci",
#                         PPFD = "Qin"),
#         fitTPU = TRUE, Tcorrect = FALSE)
#
#aci.coefs[216,] <- c(id = "brit.lh.3.15", doy = "15", baseline = "n", new = "y", t(coef(curve216)))

## baseline Id:16
curve217 <- aci.df %>% 
  filter(id == "brit.lh.4.16") %>%
  filter(date == "2024-01-15") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[217,] <- c(id = "brit.lh.4.16", doy = "15", baseline = "n", new = "y", t(coef(curve217)))

## baseline Id:25
curve218 <- aci.df %>% 
  filter(id == "paul.lc.1.25") %>%
  filter(date == "2024-01-15") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[218,] <- c(id = "paul.lc.1.25", doy = "15", baseline = "n", new = "y", t(coef(curve218)))

## baseline Id:26
curve219 <- aci.df %>% 
  filter(id == "paul.lc.2.26") %>%
  filter(date == "2024-01-15") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[219,] <- c(id = "paul.lc.2.26", doy = "15", baseline = "n", new = "y", t(coef(curve219)))

## baseline Id:27

curve220 <- aci.df %>% 
  filter(id == "paul.lc.3.27") %>%
  filter(date == "2024-01-15") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[220,] <- c(id = "paul.lc.3.27", doy = "15", baseline = "n", new = "y", t(coef(curve220)))

## baseline Id:29

curve221 <- aci.df %>% 
  filter(id == "paul.lh.1.29") %>%
  filter(date == "2024-01-15") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[221,] <- c(id = "paul.lh.1.29", doy = "15", baseline = "n", new = "y", t(coef(curve221)))

## baseline Id:30

curve222 <- aci.df %>% 
  filter(id == "paul.lh.2.30") %>%
  filter(date == "2024-01-15") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[222,] <- c(id = "paul.lh.2.30", doy = "15", baseline = "n", new = "y", t(coef(curve222)))

## baseline Id:31

curve223 <- aci.df %>% 
  filter(id == "paul.lh.3.31") %>%
  filter(date == "2024-01-15") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[223,] <- c(id = "paul.lh.3.31", doy = "15", baseline = "n", new = "y", t(coef(curve223)))

################################################################################

## Date 1/16
# baseline new G3, ID 18
curve224 <- aci.df %>% 
  filter(id == "paul.hc.2.18") %>%
  filter(date == "2024-01-16") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[224,] <- c(id = "paul.hc.2.18", doy = "16", baseline = "y", new = "y", t(coef(curve224)))

## Id:19
curve225 <- aci.df %>% 
  filter(id == "paul.hc.3.19") %>%
  filter(date == "2024-01-16") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE)

aci.coefs[225,] <- c(id = "paul.hc.3.19", doy = "16", baseline = "y", new = "y", t(coef(curve225)))

## Id:20
curve226 <- aci.df %>% 
  filter(id == "paul.hc.4.20") %>%
  filter(date == "2024-01-16") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[226,] <- c(id = "paul.hc.4.20", doy = "16", baseline = "y", new = "y", t(coef(curve226)))

## baseline Id:21
curve227 <- aci.df %>% 
  filter(id == "paul.hl.1.21") %>%
  filter(date == "2024-01-16") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[227,] <- c(id = "paul.hl.1.21", doy = "16", baseline = "y", new = "y", t(coef(curve227)))

## baseline Id:22
curve228 <- aci.df %>% 
  filter(id == "paul.hl.2.22") %>%
  filter(date == "2024-01-16") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[228,] <- c(id = "paul.hl.2.22", doy = "16", baseline = "y", new = "y", t(coef(curve228)))

## baseline Id:24
curve229 <- aci.df %>% 
  filter(id == "paul.hl.4.24") %>%
  filter(date == "2024-01-16") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[229,] <- c(id = "paul.hl.4.24", doy = "16", baseline = "y", new = "y", t(coef(curve229)))

## baseline Id:33
curve230 <- aci.df %>% 
  filter(id == "ryan.hc.1.33") %>%
  filter(date == "2024-01-16") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[230,] <- c(id = "ryan.hc.1.33", doy = "16", baseline = "y", new = "y", t(coef(curve230)))

## baseline Id:34
curve231 <- aci.df %>% 
  filter(id == "ryan.hc.2.34") %>%
  filter(date == "2024-01-16") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[231,] <- c(id = "ryan.hc.2.34", doy = "16", baseline = "y", new = "y", t(coef(curve231)))

## baseline Id:35

curve232 <- aci.df %>% 
  filter(id == "ryan.hc.3.35") %>%
  filter(date == "2024-01-16") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[232,] <- c(id = "ryan.hc.3.35", doy = "16", baseline = "y", new = "y", t(coef(curve232)))


## baseline Id:37

curve233 <- aci.df %>% 
  filter(id == "ryan.hl.1.37") %>%
  filter(date == "2024-01-16") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[233,] <- c(id = "ryan.hl.1.37", doy = "16", baseline = "y", new = "y", t(coef(curve233)))

## baseline Id:38

curve234 <- aci.df %>% 
  filter(id == "ryan.hl.2.38") %>%
  filter(date == "2024-01-16") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 390)

aci.coefs[234,] <- c(id = "ryan.hl.2.38", doy = "16", baseline = "y", new = "y", t(coef(curve234)))

## baseline Id:40

curve235 <- aci.df %>% 
  filter(id == "ryan.hl.4.40") %>%
  filter(date == "2024-01-16") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[235,] <- c(id = "ryan.hl.4.40", doy = "16", baseline = "y", new = "y", t(coef(curve235)))

################################################################################

## Date 1/17
# curve 1 new G2, ID 1
curve236 <- aci.df %>% 
  filter(id == "brit.hc.1.1") %>%
  filter(date == "2024-01-17") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[236,] <- c(id = "brit.hc.1.1", doy = "17", baseline = "n", new = "y", t(coef(curve236)))

## Id:2
curve237 <- aci.df %>% 
  filter(id == "brit.hc.2.2") %>%
  filter(date == "2024-01-17") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[237,] <- c(id = "brit.hc.2.2", doy = "17", baseline = "n", new = "y", t(coef(curve237)))

## Id:5
curve238 <- aci.df %>% 
  filter(id == "brit.hl.1.5") %>%
  filter(date == "2024-01-17") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[238,] <- c(id = "brit.hl.1.5", doy = "17", baseline = "n", new = "y", t(coef(curve238)))

## baseline Id:6
curve239 <- aci.df %>% 
  filter(id == "brit.hl.2.6") %>%
  filter(date == "2024-01-17") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[239,] <- c(id = "brit.hl.2.6", doy = "17", baseline = "n", new = "y", t(coef(curve239)))

## baseline Id:7
curve240 <- aci.df %>% 
  filter(id == "brit.hl.3.7") %>%
  filter(date == "2024-01-17") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[240,] <- c(id = "brit.hl.3.7", doy = "17", baseline = "n", new = "y", t(coef(curve240)))

## baseline Id:41
curve241 <- aci.df %>% 
  filter(id == "ryan.lc.1.41") %>%
  filter(date == "2024-01-17") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[241,] <- c(id = "ryan.lc.1.41", doy = "17", baseline = "n", new = "y", t(coef(curve241)))

## baseline Id:43
curve242 <- aci.df %>% 
  filter(id == "ryan.lc.3.43") %>%
  filter(date == "2024-01-17") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[242,] <- c(id = "ryan.lc.3.43", doy = "17", baseline = "n", new = "y", t(coef(curve242)))

## baseline Id:44
curve243 <- aci.df %>% 
  filter(id == "ryan.lc.4.44") %>%
  filter(date == "2024-01-17") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[243,] <- c(id = "ryan.lc.4.44", doy = "17", baseline = "n", new = "y", t(coef(curve243)))

## baseline Id:45

curve244 <- aci.df %>% 
  filter(id == "ryan.lh.1.45") %>%
  filter(date == "2024-01-17") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[244,] <- c(id = "ryan.lh.1.45", doy = "17", baseline = "n", new = "y", t(coef(curve244)))

## baseline Id:47

curve245 <- aci.df %>% 
  filter(id == "ryan.lh.3.47") %>%
  filter(date == "2024-01-17") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[245,] <- c(id = "ryan.lh.3.47", doy = "17", baseline = "n", new = "y", t(coef(curve245)))

## baseline Id:48

curve246 <- aci.df %>% 
  filter(id == "ryan.lh.4.48") %>%
  filter(date == "2024-01-17") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[246,] <- c(id = "ryan.lh.4.48", doy = "17", baseline = "n", new = "y", t(coef(curve246)))

#################################################################################

## Date 1/18
# Curve 3 new G1, ID 9
curve247 <- aci.df %>% 
  filter(id == "brit.lc.1.9") %>%
  filter(date == "2024-01-18") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[247,] <- c(id = "brit.lc.1.9", doy = "18", baseline = "n", new = "y", t(coef(curve247)))

## Id:10
curve248 <- aci.df %>% 
  filter(id == "brit.lc.2.10") %>%
  filter(date == "2024-01-18") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[248,] <- c(id = "brit.lc.2.10", doy = "18", baseline = "n", new = "y", t(coef(curve248)))

## Id:11
curve249 <- aci.df %>% 
  filter(id == "brit.lc.3.11") %>%
  filter(date == "2024-01-18") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[249,] <- c(id = "brit.lc.3.11", doy = "18", baseline = "n", new = "y", t(coef(curve249)))

## baseline Id:13
curve250 <- aci.df %>% 
  filter(id == "brit.lh.1.13") %>%
  filter(date == "2024-01-18") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 440)

aci.coefs[250,] <- c(id = "brit.lh.1.13", doy = "18", baseline = "n", new = "y", t(coef(curve250)))

## baseline Id:15
curve251 <- aci.df %>% 
  filter(id == "brit.lh.3.15") %>%
  filter(date == "2024-01-18") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[251,] <- c(id = "brit.lh.3.15", doy = "18", baseline = "n", new = "y", t(coef(curve251)))

## baseline Id:16
curve252 <- aci.df %>% 
  filter(id == "brit.lh.4.16") %>%
  filter(date == "2024-01-18") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[252,] <- c(id = "brit.lh.4.16", doy = "18", baseline = "n", new = "y", t(coef(curve252)))

## baseline Id:25
#curve253 <- aci.df %>% 
#  filter(id == "paul.lc.1.25") %>%
#  filter(date == "2024-01-18") %>%
#  fitaci(varnames = list(ALEAF = "A",
#                         Tleaf = "Tleaf",
#                         Ci = "Ci",
#                         PPFD = "Qin"),
#         fitTPU = TRUE, Tcorrect = FALSE)
#
#aci.coefs[253,] <- c(id = "paul.lc.1.25", doy = "18", baseline = "n", new = "y", t(coef(curve253)))

## baseline Id:26
curve254 <- aci.df %>% 
  filter(id == "paul.lc.2.26") %>%
  filter(date == "2024-01-18") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[254,] <- c(id = "paul.lc.2.26", doy = "18", baseline = "n", new = "y", t(coef(curve254)))

## baseline Id:27

curve255 <- aci.df %>% 
  filter(id == "paul.lc.3.27") %>%
  filter(date == "2024-01-18") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[255,] <- c(id = "paul.lc.3.27", doy = "18", baseline = "n", new = "y", t(coef(curve255)))

## baseline Id:29

curve256 <- aci.df %>% 
  filter(id == "paul.lh.1.29") %>%
  filter(date == "2024-01-18") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[256,] <- c(id = "paul.lh.1.29", doy = "18", baseline = "n", new = "y", t(coef(curve256)))

## baseline Id:30

curve257 <- aci.df %>% 
  filter(id == "paul.lh.2.30") %>%
  filter(date == "2024-01-18") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 500)

aci.coefs[257,] <- c(id = "paul.lh.2.30", doy = "18", baseline = "n", new = "y", t(coef(curve257)))

## baseline Id:31

#curve258 <- aci.df %>% 
#  filter(id == "paul.lh.3.31") %>%
#  filter(date == "2024-01-18") %>%
#  fitaci(varnames = list(ALEAF = "A",
#                         Tleaf = "Tleaf",
#                         Ci = "Ci",
#                         PPFD = "Qin"),
#         fitTPU = TRUE, Tcorrect = FALSE, citransition = 500)
#
#aci.coefs[258,] <- c(id = "paul.lh.3.31", doy = "18", baseline = "n", new = "y", t(coef(curve258)))

################################################################################

## Date 1/19
# curve 1 new G3, ID 18
curve259 <- aci.df %>% 
  filter(id == "paul.hc.2.18") %>%
  filter(date == "2024-01-19") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[259,] <- c(id = "paul.hc.2.18", doy = "19", baseline = "n", new = "y", t(coef(curve259)))

## Id:19
curve260 <- aci.df %>% 
  filter(id == "paul.hc.3.19") %>%
  filter(date == "2024-01-19") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 395)

aci.coefs[260,] <- c(id = "paul.hc.3.19", doy = "19", baseline = "n", new = "y", t(coef(curve260)))

## Id:20
curve261 <- aci.df %>% 
  filter(id == "paul.hc.4.20") %>%
  filter(date == "2024-01-19") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 450)

aci.coefs[261,] <- c(id = "paul.hc.4.20", doy = "19", baseline = "n", new = "y", t(coef(curve261)))

## baseline Id:21
curve262 <- aci.df %>% 
  filter(id == "paul.hl.1.21") %>%
  filter(date == "2024-01-19") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[262,] <- c(id = "paul.hl.1.21", doy = "19", baseline = "n", new = "y", t(coef(curve262)))

## baseline Id:22
curve263 <- aci.df %>% 
  filter(id == "paul.hl.2.22") %>%
  filter(date == "2024-01-19") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[263,] <- c(id = "paul.hl.2.22", doy = "19", baseline = "n", new = "y", t(coef(curve263)))

## baseline Id:24
curve264 <- aci.df %>% 
  filter(id == "paul.hl.4.24") %>%
  filter(date == "2024-01-19") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE)

aci.coefs[264,] <- c(id = "paul.hl.4.24", doy = "19", baseline = "n", new = "y", t(coef(curve264)))

## baseline Id:33
curve265 <- aci.df %>% 
  filter(id == "ryan.hc.1.33") %>%
  filter(date == "2024-01-19") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE)

aci.coefs[265,] <- c(id = "ryan.hc.1.33", doy = "19", baseline = "n", new = "y", t(coef(curve265)))

## baseline Id:34
curve266 <- aci.df %>% 
  filter(id == "ryan.hc.2.34") %>%
  filter(date == "2024-01-19") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[266,] <- c(id = "ryan.hc.2.34", doy = "19", baseline = "n", new = "y", t(coef(curve266)))

## baseline Id:35

curve267 <- aci.df %>% 
  filter(id == "ryan.hc.3.35") %>%
  filter(date == "2024-01-19") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

plot(curve267)

aci.coefs[267,] <- c(id = "ryan.hc.3.35", doy = "19", baseline = "n", new = "y", t(coef(curve267)))

## baseline Id:37

curve268 <- aci.df %>% 
  filter(id == "ryan.hl.1.37") %>%
  filter(date == "2024-01-19") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 450)

aci.coefs[268,] <- c(id = "ryan.hl.1.37", doy = "19", baseline = "n", new = "y", t(coef(curve268)))

## baseline Id:38

curve269 <- aci.df %>% 
  filter(id == "ryan.hl.2.38") %>%
  filter(date == "2024-01-19") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[269,] <- c(id = "ryan.hl.2.38", doy = "19", baseline = "n", new = "y", t(coef(curve269)))

## baseline Id:40

#curve270 <- aci.df %>% 
#  filter(id == "ryan.hl.4.40") %>%
#  filter(date == "2024-01-19") %>%
#  fitaci(varnames = list(ALEAF = "A",
#                         Tleaf = "Tleaf",
#                         Ci = "Ci",
#                         PPFD = "Qin"),
#         fitTPU = TRUE, Tcorrect = FALSE)
#
#aci.coefs[270,] <- c(id = "ryan.hl.4.40", doy = "19", baseline = "n", new = "y", t(coef(curve270)))

################################################################################

## Date 1/21
# curve 2 new G2, ID 1
curve271 <- aci.df %>% 
  filter(id == "brit.hc.1.1") %>%
  filter(date == "2024-01-21") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 600)

aci.coefs[271,] <- c(id = "brit.hc.1.1", doy = "21", baseline = "n", new = "y", t(coef(curve271)))

## Id:2
curve272 <- aci.df %>% 
  filter(id == "brit.hc.2.2") %>%
  filter(date == "2024-01-21") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[272,] <- c(id = "brit.hc.2.2", doy = "21", baseline = "n", new = "y", t(coef(curve272)))

## Id:5
curve273 <- aci.df %>% 
  filter(id == "brit.hl.1.5") %>%
  filter(date == "2024-01-21") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[273,] <- c(id = "brit.hl.1.5", doy = "21", baseline = "n", new = "y", t(coef(curve273)))

## baseline Id:6
curve274 <- aci.df %>% 
  filter(id == "brit.hl.2.6") %>%
  filter(date == "2024-01-21") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[274,] <- c(id = "brit.hl.2.6", doy = "21", baseline = "n", new = "y", t(coef(curve274)))

## baseline Id:7
curve275 <- aci.df %>% 
  filter(id == "brit.hl.3.7") %>%
  filter(date == "2024-01-21") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 500)

plot(curve275)

aci.coefs[275,] <- c(id = "brit.hl.3.7", doy = "21", baseline = "n", new = "y", t(coef(curve275)))

## baseline Id:41
curve276 <- aci.df %>% 
  filter(id == "ryan.lc.1.41") %>%
  filter(date == "2024-01-21") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE)

aci.coefs[276,] <- c(id = "ryan.lc.1.41", doy = "21", baseline = "n", new = "y", t(coef(curve276)))

## baseline Id:43
curve277 <- aci.df %>% 
  filter(id == "ryan.lc.3.43") %>%
  filter(date == "2024-01-21") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[277,] <- c(id = "ryan.lc.3.43", doy = "21", baseline = "n", new = "y", t(coef(curve277)))

## baseline Id:44
curve278 <- aci.df %>% 
  filter(id == "ryan.lc.4.44") %>%
  filter(date == "2024-01-21") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

plot(curve278)

aci.coefs[278,] <- c(id = "ryan.lc.4.44", doy = "21", baseline = "n", new = "y", t(coef(curve278)))

## baseline Id:45

curve279 <- aci.df %>% 
  filter(id == "ryan.lh.1.45") %>%
  filter(date == "2024-01-21") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[279,] <- c(id = "ryan.lh.1.45", doy = "21", baseline = "n", new = "y", t(coef(curve279)))

## baseline Id:47

curve280 <- aci.df %>% 
  filter(id == "ryan.lh.3.47") %>%
  filter(date == "2024-01-21") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[280,] <- c(id = "ryan.lh.3.47", doy = "21", baseline = "n", new = "y", t(coef(curve280)))

## baseline Id:48

curve281 <- aci.df %>% 
  filter(id == "ryan.lh.4.48") %>%
  filter(date == "2024-01-21") %>%
  fitaci(varnames = list(ALEAF = "A",
                         Tleaf = "Tleaf",
                         Ci = "Ci",
                         PPFD = "Qin"),
         fitTPU = TRUE, Tcorrect = FALSE, citransition = 400)

aci.coefs[281,] <- c(id = "ryan.lh.4.48", doy = "21", baseline = "n", new = "y", t(coef(curve281)))

### TEST THIS
write.csv(aci.coefs, "/home/eevee/Desktop/ts_curve_data.csv", row.names = FALSE)
