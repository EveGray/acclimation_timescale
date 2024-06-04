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
aci.df.unique_id <- unique(aci.df$unique_id) # make a string for each unique curve id

## fit curves

all_curve_fits <- c() # make a empty dataset for adding the curve fit information

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
curve1_output <- cbind('curve1', curve1_data$id[1], curve1_data$unique_id[1], curve1_data$machine[1], curve1_data$baseline_yn[1],
                       curve1_data$A[1], curve1_data$Ci[1], curve1_data$gsw[1],
                       mean(curve1_data$VPDleaf, na.rm = T), mean(curve1_data$Tleaf, na.rm = T), mean(curve1_data$Qin, na.rm = T),
                       curve1_fit[[2]][1,1], curve1_fit[[2]][1,2],
                       curve1_fit[[2]][2,1], curve1_fit[[2]][2,2],
                       curve1_fit[[2]][3,1], curve1_fit[[2]][3,2],
                       curve1_fit$RMSE,
                       curve1_fit$Ci_transition,
                       curve1_fit$citransition,
                       curve1_fit$Km,
                       curve1_fit$GammaStar,
                       curve1_fit$fitmethod,
                       curve1_fit$Tcorrect,
                       curve1_fit$fitTPU) # put relevant data together in a vector of values
colnames(curve1_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                             'anet_420', 'ci_420', 'gs_420',
                             'vpd_leaf', 'temperature_leaf', 'par_leaf',
                             'vcmax_tleaf', 'vcmax_tleaf_se',
                             'jmax_tleaf', 'jmax_tleaf_se', 
                             'rdfit_tleaf', 'rdfit_tleaf_se',
                             'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                             'aci_km', 'aci_gammastar', 'aci_fitmethod',
                             'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, curve1_output) # add the curve fits to the larger data frame

### curve2_data
curve2_data <- subset(aci.df, unique_id == aci.df.unique_id[2]) # find correct curve from full dataframe and make new object
plot(curve2_data$A~curve2_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
curve2_fit <- fitaci(curve2_data, varnames = list(ALEAF = "A", # fit the curves
                                                  Tleaf = "Tleaf",
                                                  Ci = "Ci",
                                                  PPFD = "Qin"),
                     fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(curve2_fit) # take a look at fitted values, adjust as needed
plot(curve2_fit) # plot the fitted curves over the raw data, adjust as needed
curve2_output <- cbind('curve2', curve2_data$id[1], curve2_data$unique_id[1], curve2_data$machine[1], curve2_data$baseline_yn[1],
                       curve2_data$A[1], curve2_data$Ci[1], curve2_data$gsw[1],
                       mean(curve2_data$VPDleaf, na.rm = T), mean(curve2_data$Tleaf, na.rm = T), mean(curve2_data$Qin, na.rm = T),
                       curve2_fit[[2]][1,1], curve2_fit[[2]][1,2],
                       curve2_fit[[2]][2,1], curve2_fit[[2]][2,2],
                       curve2_fit[[2]][3,1], curve2_fit[[2]][3,2],
                       curve2_fit$RMSE,
                       curve2_fit$Ci_transition,
                       curve2_fit$citransition,
                       curve2_fit$Km,
                       curve2_fit$GammaStar,
                       curve2_fit$fitmethod,
                       curve2_fit$Tcorrect,
                       curve2_fit$fitTPU) # put relevant data together in a vector of values
colnames(curve2_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                             'anet_420', 'ci_420', 'gs_420',
                             'vpd_leaf', 'temperature_leaf', 'par_leaf',
                             'vcmax_tleaf', 'vcmax_tleaf_se',
                             'jmax_tleaf', 'jmax_tleaf_se', 
                             'rdfit_tleaf', 'rdfit_tleaf_se',
                             'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                             'aci_km', 'aci_gammastar', 'aci_fitmethod',
                             'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, curve2_output) # add the curve fits to the larger data frame

### curve3_data
curve3_data <- subset(aci.df, unique_id == aci.df.unique_id[3]) # find correct curve from full dataframe and make new object
plot(curve3_data$A~curve3_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
curve3_fit <- fitaci(curve3_data, varnames = list(ALEAF = "A", # fit the curves
                                                  Tleaf = "Tleaf",
                                                  Ci = "Ci",
                                                  PPFD = "Qin"),
                     fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(curve3_fit) # take a look at fitted values, adjust as needed
plot(curve3_fit) # plot the fitted curves over the raw data, adjust as needed
curve3_output <- cbind('curve3', curve3_data$id[1], curve3_data$unique_id[1], curve3_data$machine[1], curve3_data$baseline_yn[1],
                       curve3_data$A[1], curve3_data$Ci[1], curve3_data$gsw[1],
                       mean(curve3_data$VPDleaf, na.rm = T), mean(curve3_data$Tleaf, na.rm = T), mean(curve3_data$Qin, na.rm = T),
                       curve3_fit[[2]][1,1], curve3_fit[[2]][1,2],
                       curve3_fit[[2]][2,1], curve3_fit[[2]][2,2],
                       curve3_fit[[2]][3,1], curve3_fit[[2]][3,2],
                       curve3_fit$RMSE,
                       curve3_fit$Ci_transition,
                       curve3_fit$citransition,
                       curve3_fit$Km,
                       curve3_fit$GammaStar,
                       curve3_fit$fitmethod,
                       curve3_fit$Tcorrect,
                       curve3_fit$fitTPU) # put relevant data together in a vector of values
colnames(curve3_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                             'anet_420', 'ci_420', 'gs_420',
                             'vpd_leaf', 'temperature_leaf', 'par_leaf',
                             'vcmax_tleaf', 'vcmax_tleaf_se',
                             'jmax_tleaf', 'jmax_tleaf_se', 
                             'rdfit_tleaf', 'rdfit_tleaf_se',
                             'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                             'aci_km', 'aci_gammastar', 'aci_fitmethod',
                             'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, curve3_output) # add the curve fits to the larger data frame

### curve4_data
curve4_data <- subset(aci.df, unique_id == aci.df.unique_id[4]) # find correct curve from full dataframe and make new object
plot(curve4_data$A~curve4_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
curve4_fit <- fitaci(curve4_data, varnames = list(ALEAF = "A", # fit the curves
                                                  Tleaf = "Tleaf",
                                                  Ci = "Ci",
                                                  PPFD = "Qin"),
                     fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(curve4_fit) # take a look at fitted values, adjust as needed
plot(curve4_fit) # plot the fitted curves over the raw data, adjust as needed
curve4_output <- cbind('curve4', curve4_data$id[1], curve4_data$unique_id[1], curve4_data$machine[1], curve4_data$baseline_yn[1],
                       curve4_data$A[1], curve4_data$Ci[1], curve4_data$gsw[1],
                       mean(curve4_data$VPDleaf, na.rm = T), mean(curve4_data$Tleaf, na.rm = T), mean(curve4_data$Qin, na.rm = T),
                       curve4_fit[[2]][1,1], curve4_fit[[2]][1,2],
                       curve4_fit[[2]][2,1], curve4_fit[[2]][2,2],
                       curve4_fit[[2]][3,1], curve4_fit[[2]][3,2],
                       curve4_fit$RMSE,
                       curve4_fit$Ci_transition,
                       curve4_fit$citransition,
                       curve4_fit$Km,
                       curve4_fit$GammaStar,
                       curve4_fit$fitmethod,
                       curve4_fit$Tcorrect,
                       curve4_fit$fitTPU) # put relevant data together in a vector of values
colnames(curve4_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                             'anet_420', 'ci_420', 'gs_420',
                             'vpd_leaf', 'temperature_leaf', 'par_leaf',
                             'vcmax_tleaf', 'vcmax_tleaf_se',
                             'jmax_tleaf', 'jmax_tleaf_se', 
                             'rdfit_tleaf', 'rdfit_tleaf_se',
                             'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                             'aci_km', 'aci_gammastar', 'aci_fitmethod',
                             'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, curve4_output) # add the curve fits to the larger data frame

### curve5_data
curve5_data <- subset(aci.df, unique_id == aci.df.unique_id[5]) # find correct curve from full dataframe and make new object
plot(curve5_data$A~curve5_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
curve5_fit <- fitaci(curve5_data, varnames = list(ALEAF = "A", # fit the curves
                                                  Tleaf = "Tleaf",
                                                  Ci = "Ci",
                                                  PPFD = "Qin"),
                     fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(curve5_fit) # take a look at fitted values, adjust as needed
plot(curve5_fit) # plot the fitted curves over the raw data, adjust as needed
curve5_output <- cbind('curve5', curve5_data$id[1], curve5_data$unique_id[1], curve5_data$machine[1], curve5_data$baseline_yn[1],
                       curve5_data$A[1], curve5_data$Ci[1], curve5_data$gsw[1],
                       mean(curve5_data$VPDleaf, na.rm = T), mean(curve5_data$Tleaf, na.rm = T), mean(curve5_data$Qin, na.rm = T),
                       curve5_fit[[2]][1,1], curve5_fit[[2]][1,2],
                       curve5_fit[[2]][2,1], curve5_fit[[2]][2,2],
                       curve5_fit[[2]][3,1], curve5_fit[[2]][3,2],
                       curve5_fit$RMSE,
                       curve5_fit$Ci_transition,
                       curve5_fit$citransition,
                       curve5_fit$Km,
                       curve5_fit$GammaStar,
                       curve5_fit$fitmethod,
                       curve5_fit$Tcorrect,
                       curve5_fit$fitTPU) # put relevant data together in a vector of values
colnames(curve5_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                             'anet_420', 'ci_420', 'gs_420',
                             'vpd_leaf', 'temperature_leaf', 'par_leaf',
                             'vcmax_tleaf', 'vcmax_tleaf_se',
                             'jmax_tleaf', 'jmax_tleaf_se', 
                             'rdfit_tleaf', 'rdfit_tleaf_se',
                             'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                             'aci_km', 'aci_gammastar', 'aci_fitmethod',
                             'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, curve5_output) # add the curve fits to the larger data frame

### curve6_data
curve6_data <- subset(aci.df, unique_id == aci.df.unique_id[6]) # find correct curve from full dataframe and make new object
plot(curve6_data$A~curve6_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
curve6_fit <- fitaci(curve6_data, varnames = list(ALEAF = "A", # fit the curves
                                                  Tleaf = "Tleaf",
                                                  Ci = "Ci",
                                                  PPFD = "Qin"),
                     fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(curve6_fit) # take a look at fitted values, adjust as needed
plot(curve6_fit) # plot the fitted curves over the raw data, adjust as needed
curve6_output <- cbind('curve6', curve6_data$id[1], curve6_data$unique_id[1], curve6_data$machine[1], curve6_data$baseline_yn[1],
                       curve6_data$A[1], curve6_data$Ci[1], curve6_data$gsw[1],
                       mean(curve6_data$VPDleaf, na.rm = T), mean(curve6_data$Tleaf, na.rm = T), mean(curve6_data$Qin, na.rm = T),
                       curve6_fit[[2]][1,1], curve6_fit[[2]][1,2],
                       curve6_fit[[2]][2,1], curve6_fit[[2]][2,2],
                       curve6_fit[[2]][3,1], curve6_fit[[2]][3,2],
                       curve6_fit$RMSE,
                       curve6_fit$Ci_transition,
                       curve6_fit$citransition,
                       curve6_fit$Km,
                       curve6_fit$GammaStar,
                       curve6_fit$fitmethod,
                       curve6_fit$Tcorrect,
                       curve6_fit$fitTPU) # put relevant data together in a vector of values
colnames(curve6_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                             'anet_420', 'ci_420', 'gs_420',
                             'vpd_leaf', 'temperature_leaf', 'par_leaf',
                             'vcmax_tleaf', 'vcmax_tleaf_se',
                             'jmax_tleaf', 'jmax_tleaf_se', 
                             'rdfit_tleaf', 'rdfit_tleaf_se',
                             'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                             'aci_km', 'aci_gammastar', 'aci_fitmethod',
                             'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, curve6_output) # add the curve fits to the larger data frame

### curve7_data
curve7_data <- subset(aci.df, unique_id == aci.df.unique_id[7]) # find correct curve from full dataframe and make new object
plot(curve7_data$A~curve7_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
curve7_fit <- fitaci(curve7_data, varnames = list(ALEAF = "A", # fit the curves
                                                  Tleaf = "Tleaf",
                                                  Ci = "Ci",
                                                  PPFD = "Qin"),
                     fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(curve7_fit) # take a look at fitted values, adjust as needed
plot(curve7_fit) # plot the fitted curves over the raw data, adjust as needed
curve7_output <- cbind('curve7', curve7_data$id[1], curve7_data$unique_id[1], curve7_data$machine[1], curve7_data$baseline_yn[1],
                       curve7_data$A[1], curve7_data$Ci[1], curve7_data$gsw[1],
                       mean(curve7_data$VPDleaf, na.rm = T), mean(curve7_data$Tleaf, na.rm = T), mean(curve7_data$Qin, na.rm = T),
                       curve7_fit[[2]][1,1], curve7_fit[[2]][1,2],
                       curve7_fit[[2]][2,1], curve7_fit[[2]][2,2],
                       curve7_fit[[2]][3,1], curve7_fit[[2]][3,2],
                       curve7_fit$RMSE,
                       curve7_fit$Ci_transition,
                       curve7_fit$citransition,
                       curve7_fit$Km,
                       curve7_fit$GammaStar,
                       curve7_fit$fitmethod,
                       curve7_fit$Tcorrect,
                       curve7_fit$fitTPU) # put relevant data together in a vector of values
colnames(curve7_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                             'anet_420', 'ci_420', 'gs_420',
                             'vpd_leaf', 'temperature_leaf', 'par_leaf',
                             'vcmax_tleaf', 'vcmax_tleaf_se',
                             'jmax_tleaf', 'jmax_tleaf_se', 
                             'rdfit_tleaf', 'rdfit_tleaf_se',
                             'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                             'aci_km', 'aci_gammastar', 'aci_fitmethod',
                             'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, curve7_output) # add the curve fits to the larger data frame

### curve8_data
curve8_data <- subset(aci.df, unique_id == aci.df.unique_id[8]) # find correct curve from full dataframe and make new object
plot(curve8_data$A~curve8_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
curve8_fit <- fitaci(curve8_data, varnames = list(ALEAF = "A", # fit the curves
                                                  Tleaf = "Tleaf",
                                                  Ci = "Ci",
                                                  PPFD = "Qin"),
                     fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(curve8_fit) # take a look at fitted values, adjust as needed
plot(curve8_fit) # plot the fitted curves over the raw data, adjust as needed
curve8_output <- cbind('curve8', curve8_data$id[1], curve8_data$unique_id[1], curve8_data$machine[1], curve8_data$baseline_yn[1],
                       curve8_data$A[1], curve8_data$Ci[1], curve8_data$gsw[1],
                       mean(curve8_data$VPDleaf, na.rm = T), mean(curve8_data$Tleaf, na.rm = T), mean(curve8_data$Qin, na.rm = T),
                       curve8_fit[[2]][1,1], curve8_fit[[2]][1,2],
                       curve8_fit[[2]][2,1], curve8_fit[[2]][2,2],
                       curve8_fit[[2]][3,1], curve8_fit[[2]][3,2],
                       curve8_fit$RMSE,
                       curve8_fit$Ci_transition,
                       curve8_fit$citransition,
                       curve8_fit$Km,
                       curve8_fit$GammaStar,
                       curve8_fit$fitmethod,
                       curve8_fit$Tcorrect,
                       curve8_fit$fitTPU) # put relevant data together in a vector of values
colnames(curve8_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                             'anet_420', 'ci_420', 'gs_420',
                             'vpd_leaf', 'temperature_leaf', 'par_leaf',
                             'vcmax_tleaf', 'vcmax_tleaf_se',
                             'jmax_tleaf', 'jmax_tleaf_se', 
                             'rdfit_tleaf', 'rdfit_tleaf_se',
                             'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                             'aci_km', 'aci_gammastar', 'aci_fitmethod',
                             'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, curve8_output) # add the curve fits to the larger data frame

### curve9_data
curve9_data <- subset(aci.df, unique_id == aci.df.unique_id[9]) # find correct curve from full dataframe and make new object
plot(curve9_data$A~curve9_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
curve9_fit <- fitaci(curve9_data, varnames = list(ALEAF = "A", # fit the curves
                                                  Tleaf = "Tleaf",
                                                  Ci = "Ci",
                                                  PPFD = "Qin"),
                     fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(curve9_fit) # take a look at fitted values, adjust as needed
plot(curve9_fit) # plot the fitted curves over the raw data, adjust as needed
curve9_output <- cbind('curve9', curve9_data$id[1], curve9_data$unique_id[1], curve9_data$machine[1], curve9_data$baseline_yn[1],
                       curve9_data$A[1], curve9_data$Ci[1], curve9_data$gsw[1],
                       mean(curve9_data$VPDleaf, na.rm = T), mean(curve9_data$Tleaf, na.rm = T), mean(curve9_data$Qin, na.rm = T),
                       curve9_fit[[2]][1,1], curve9_fit[[2]][1,2],
                       curve9_fit[[2]][2,1], curve9_fit[[2]][2,2],
                       curve9_fit[[2]][3,1], curve9_fit[[2]][3,2],
                       curve9_fit$RMSE,
                       curve9_fit$Ci_transition,
                       curve9_fit$citransition,
                       curve9_fit$Km,
                       curve9_fit$GammaStar,
                       curve9_fit$fitmethod,
                       curve9_fit$Tcorrect,
                       curve9_fit$fitTPU) # put relevant data together in a vector of values
colnames(curve9_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                             'anet_420', 'ci_420', 'gs_420',
                             'vpd_leaf', 'temperature_leaf', 'par_leaf',
                             'vcmax_tleaf', 'vcmax_tleaf_se',
                             'jmax_tleaf', 'jmax_tleaf_se', 
                             'rdfit_tleaf', 'rdfit_tleaf_se',
                             'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                             'aci_km', 'aci_gammastar', 'aci_fitmethod',
                             'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, curve9_output) # add the curve fits to the larger data frame

### curve10_data
curve10_data <- subset(aci.df, unique_id == aci.df.unique_id[10]) # find correct curve from full dataframe and make new object
plot(curve10_data$A~curve10_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
curve10_fit <- fitaci(curve10_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(curve10_fit) # take a look at fitted values, adjust as needed
plot(curve10_fit) # plot the fitted curves over the raw data, adjust as needed
curve10_output <- cbind('curve10', curve10_data$id[1], curve10_data$unique_id[1], curve10_data$machine[1], curve10_data$baseline_yn[1],
                        curve10_data$A[1], curve10_data$Ci[1], curve10_data$gsw[1],
                        mean(curve10_data$VPDleaf, na.rm = T), mean(curve10_data$Tleaf, na.rm = T), mean(curve10_data$Qin, na.rm = T),
                        curve10_fit[[2]][1,1], curve10_fit[[2]][1,2],
                        curve10_fit[[2]][2,1], curve10_fit[[2]][2,2],
                        curve10_fit[[2]][3,1], curve10_fit[[2]][3,2],
                        curve10_fit$RMSE,
                        curve10_fit$Ci_transition,
                        curve10_fit$citransition,
                        curve10_fit$Km,
                        curve10_fit$GammaStar,
                        curve10_fit$fitmethod,
                        curve10_fit$Tcorrect,
                        curve10_fit$fitTPU) # put relevant data together in a vector of values
colnames(curve10_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, curve10_output) # add the curve fits to the larger data frame

### curve11_data
curve11_data <- subset(aci.df, unique_id == aci.df.unique_id[11]) # find correct curve from full dataframe and make new object
plot(curve11_data$A~curve11_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
curve11_fit <- fitaci(curve11_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(curve11_fit) # take a look at fitted values, adjust as needed
plot(curve11_fit) # plot the fitted curves over the raw data, adjust as needed
curve11_output <- cbind('curve11', curve11_data$id[1], curve11_data$unique_id[1], curve11_data$machine[1], curve11_data$baseline_yn[1],
                        curve11_data$A[1], curve11_data$Ci[1], curve11_data$gsw[1],
                        mean(curve11_data$VPDleaf, na.rm = T), mean(curve11_data$Tleaf, na.rm = T), mean(curve11_data$Qin, na.rm = T),
                        curve11_fit[[2]][1,1], curve11_fit[[2]][1,2],
                        curve11_fit[[2]][2,1], curve11_fit[[2]][2,2],
                        curve11_fit[[2]][3,1], curve11_fit[[2]][3,2],
                        curve11_fit$RMSE,
                        curve11_fit$Ci_transition,
                        curve11_fit$citransition,
                        curve11_fit$Km,
                        curve11_fit$GammaStar,
                        curve11_fit$fitmethod,
                        curve11_fit$Tcorrect,
                        curve11_fit$fitTPU) # put relevant data together in a vector of values
colnames(curve11_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, curve11_output) # add the curve fits to the larger data frame

### curve12_data
curve12_data <- subset(aci.df, unique_id == aci.df.unique_id[12]) # find correct curve from full dataframe and make new object
plot(curve12_data$A~curve12_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
curve12_fit <- fitaci(curve12_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(curve12_fit) # take a look at fitted values, adjust as needed
plot(curve12_fit) # plot the fitted curves over the raw data, adjust as needed
curve12_output <- cbind('curve12', curve12_data$id[1], curve12_data$unique_id[1], curve12_data$machine[1], curve12_data$baseline_yn[1],
                        curve12_data$A[1], curve12_data$Ci[1], curve12_data$gsw[1],
                        mean(curve12_data$VPDleaf, na.rm = T), mean(curve12_data$Tleaf, na.rm = T), mean(curve12_data$Qin, na.rm = T),
                        curve12_fit[[2]][1,1], curve12_fit[[2]][1,2],
                        curve12_fit[[2]][2,1], curve12_fit[[2]][2,2],
                        curve12_fit[[2]][3,1], curve12_fit[[2]][3,2],
                        curve12_fit$RMSE,
                        curve12_fit$Ci_transition,
                        curve12_fit$citransition,
                        curve12_fit$Km,
                        curve12_fit$GammaStar,
                        curve12_fit$fitmethod,
                        curve12_fit$Tcorrect,
                        curve12_fit$fitTPU) # put relevant data together in a vector of values
colnames(curve12_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, curve12_output) # add the curve fits to the larger data frame

### curve13_data
curve13_data <- subset(aci.df, unique_id == aci.df.unique_id[13]) # find correct curve from full dataframe and make new object
plot(curve13_data$A~curve13_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
curve13_fit <- fitaci(curve13_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(curve13_fit) # take a look at fitted values, adjust as needed
plot(curve13_fit) # plot the fitted curves over the raw data, adjust as needed
curve13_output <- cbind('curve13', curve13_data$id[1], curve13_data$unique_id[1], curve13_data$machine[1], curve13_data$baseline_yn[1],
                        curve13_data$A[1], curve13_data$Ci[1], curve13_data$gsw[1],
                        mean(curve13_data$VPDleaf, na.rm = T), mean(curve13_data$Tleaf, na.rm = T), mean(curve13_data$Qin, na.rm = T),
                        curve13_fit[[2]][1,1], curve13_fit[[2]][1,2],
                        curve13_fit[[2]][2,1], curve13_fit[[2]][2,2],
                        curve13_fit[[2]][3,1], curve13_fit[[2]][3,2],
                        curve13_fit$RMSE,
                        curve13_fit$Ci_transition,
                        curve13_fit$citransition,
                        curve13_fit$Km,
                        curve13_fit$GammaStar,
                        curve13_fit$fitmethod,
                        curve13_fit$Tcorrect,
                        curve13_fit$fitTPU) # put relevant data together in a vector of values
colnames(curve13_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, curve13_output) # add the curve fits to the larger data frame

### curve14_data
curve14_data <- subset(aci.df, unique_id == aci.df.unique_id[14]) # find correct curve from full dataframe and make new object
plot(curve14_data$A~curve14_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
curve14_fit <- fitaci(curve14_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(curve14_fit) # take a look at fitted values, adjust as needed
plot(curve14_fit) # plot the fitted curves over the raw data, adjust as needed
curve14_output <- cbind('curve14', curve14_data$id[1], curve14_data$unique_id[1], curve14_data$machine[1], curve14_data$baseline_yn[1],
                        curve14_data$A[1], curve14_data$Ci[1], curve14_data$gsw[1],
                        mean(curve14_data$VPDleaf, na.rm = T), mean(curve14_data$Tleaf, na.rm = T), mean(curve14_data$Qin, na.rm = T),
                        curve14_fit[[2]][1,1], curve14_fit[[2]][1,2],
                        curve14_fit[[2]][2,1], curve14_fit[[2]][2,2],
                        curve14_fit[[2]][3,1], curve14_fit[[2]][3,2],
                        curve14_fit$RMSE,
                        curve14_fit$Ci_transition,
                        curve14_fit$citransition,
                        curve14_fit$Km,
                        curve14_fit$GammaStar,
                        curve14_fit$fitmethod,
                        curve14_fit$Tcorrect,
                        curve14_fit$fitTPU) # put relevant data together in a vector of values
colnames(curve14_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, curve14_output) # add the curve fits to the larger data frame

### curve15_data
curve15_data <- subset(aci.df, unique_id == aci.df.unique_id[15]) # find correct curve from full dataframe and make new object
plot(curve15_data$A~curve15_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
curve15_fit <- fitaci(curve15_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(curve15_fit) # take a look at fitted values, adjust as needed
plot(curve15_fit) # plot the fitted curves over the raw data, adjust as needed
curve15_output <- cbind('curve15', curve15_data$id[1], curve15_data$unique_id[1], curve15_data$machine[1], curve15_data$baseline_yn[1],
                        curve15_data$A[1], curve15_data$Ci[1], curve15_data$gsw[1],
                        mean(curve15_data$VPDleaf, na.rm = T), mean(curve15_data$Tleaf, na.rm = T), mean(curve15_data$Qin, na.rm = T),
                        curve15_fit[[2]][1,1], curve15_fit[[2]][1,2],
                        curve15_fit[[2]][2,1], curve15_fit[[2]][2,2],
                        curve15_fit[[2]][3,1], curve15_fit[[2]][3,2],
                        curve15_fit$RMSE,
                        curve15_fit$Ci_transition,
                        curve15_fit$citransition,
                        curve15_fit$Km,
                        curve15_fit$GammaStar,
                        curve15_fit$fitmethod,
                        curve15_fit$Tcorrect,
                        curve15_fit$fitTPU) # put relevant data together in a vector of values
colnames(curve15_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, curve15_output) # add the curve fits to the larger data frame

### curve16_data
curve16_data <- subset(aci.df, unique_id == aci.df.unique_id[16]) # find correct curve from full dataframe and make new object
plot(curve16_data$A~curve16_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
curve16_fit <- fitaci(curve16_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(curve16_fit) # take a look at fitted values, adjust as needed
plot(curve16_fit) # plot the fitted curves over the raw data, adjust as needed
curve16_output <- cbind('curve16', curve16_data$id[1], curve16_data$unique_id[1], curve16_data$machine[1], curve16_data$baseline_yn[1],
                        curve16_data$A[1], curve16_data$Ci[1], curve16_data$gsw[1],
                        mean(curve16_data$VPDleaf, na.rm = T), mean(curve16_data$Tleaf, na.rm = T), mean(curve16_data$Qin, na.rm = T),
                        curve16_fit[[2]][1,1], curve16_fit[[2]][1,2],
                        curve16_fit[[2]][2,1], curve16_fit[[2]][2,2],
                        curve16_fit[[2]][3,1], curve16_fit[[2]][3,2],
                        curve16_fit$RMSE,
                        curve16_fit$Ci_transition,
                        curve16_fit$citransition,
                        curve16_fit$Km,
                        curve16_fit$GammaStar,
                        curve16_fit$fitmethod,
                        curve16_fit$Tcorrect,
                        curve16_fit$fitTPU) # put relevant data together in a vector of values
colnames(curve16_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, curve16_output) # add the curve fits to the larger data frame

### curve17_data
curve17_data <- subset(aci.df, unique_id == aci.df.unique_id[17] & Ci < 1000) # find correct curve from full dataframe and make new object
plot(curve17_data$A~curve17_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
curve17_fit <- fitaci(curve17_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(curve17_fit) # take a look at fitted values, adjust as needed
plot(curve17_fit) # plot the fitted curves over the raw data, adjust as needed
curve17_output <- cbind('curve17', curve17_data$id[1], curve17_data$unique_id[1], curve17_data$machine[1], curve17_data$baseline_yn[1],
                        curve17_data$A[1], curve17_data$Ci[1], curve17_data$gsw[1],
                        mean(curve17_data$VPDleaf, na.rm = T), mean(curve17_data$Tleaf, na.rm = T), mean(curve17_data$Qin, na.rm = T),
                        curve17_fit[[2]][1,1], curve17_fit[[2]][1,2],
                        curve17_fit[[2]][2,1], curve17_fit[[2]][2,2],
                        curve17_fit[[2]][3,1], curve17_fit[[2]][3,2],
                        curve17_fit$RMSE,
                        curve17_fit$Ci_transition,
                        curve17_fit$citransition,
                        curve17_fit$Km,
                        curve17_fit$GammaStar,
                        curve17_fit$fitmethod,
                        curve17_fit$Tcorrect,
                        curve17_fit$fitTPU) # put relevant data together in a vector of values
colnames(curve17_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, curve17_output) # add the curve fits to the larger data frame

### curve18_data
curve18_data <- subset(aci.df, unique_id == aci.df.unique_id[18]) # find correct curve from full dataframe and make new object
plot(curve18_data$A~curve18_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
curve18_fit <- fitaci(curve18_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(curve18_fit) # take a look at fitted values, adjust as needed
plot(curve18_fit) # plot the fitted curves over the raw data, adjust as needed
curve18_output <- cbind('curve18', curve18_data$id[1], curve18_data$unique_id[1], curve18_data$machine[1], curve18_data$baseline_yn[1],
                        curve18_data$A[1], curve18_data$Ci[1], curve18_data$gsw[1],
                        mean(curve18_data$VPDleaf, na.rm = T), mean(curve18_data$Tleaf, na.rm = T), mean(curve18_data$Qin, na.rm = T),
                        curve18_fit[[2]][1,1], curve18_fit[[2]][1,2],
                        curve18_fit[[2]][2,1], curve18_fit[[2]][2,2],
                        curve18_fit[[2]][3,1], curve18_fit[[2]][3,2],
                        curve18_fit$RMSE,
                        curve18_fit$Ci_transition,
                        curve18_fit$citransition,
                        curve18_fit$Km,
                        curve18_fit$GammaStar,
                        curve18_fit$fitmethod,
                        curve18_fit$Tcorrect,
                        curve18_fit$fitTPU) # put relevant data together in a vector of values
colnames(curve18_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, curve18_output) # add the curve fits to the larger data frame

### curve19_data
curve19_data <- subset(aci.df, unique_id == aci.df.unique_id[19]) # find correct curve from full dataframe and make new object
plot(curve19_data$A~curve19_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
curve19_fit <- fitaci(curve19_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(curve19_fit) # take a look at fitted values, adjust as needed
plot(curve19_fit) # plot the fitted curves over the raw data, adjust as needed
curve19_output <- cbind('curve19', curve19_data$id[1], curve19_data$unique_id[1], curve19_data$machine[1], curve19_data$baseline_yn[1],
                        curve19_data$A[1], curve19_data$Ci[1], curve19_data$gsw[1],
                        mean(curve19_data$VPDleaf, na.rm = T), mean(curve19_data$Tleaf, na.rm = T), mean(curve19_data$Qin, na.rm = T),
                        curve19_fit[[2]][1,1], curve19_fit[[2]][1,2],
                        curve19_fit[[2]][2,1], curve19_fit[[2]][2,2],
                        curve19_fit[[2]][3,1], curve19_fit[[2]][3,2],
                        curve19_fit$RMSE,
                        curve19_fit$Ci_transition,
                        curve19_fit$citransition,
                        curve19_fit$Km,
                        curve19_fit$GammaStar,
                        curve19_fit$fitmethod,
                        curve19_fit$Tcorrect,
                        curve19_fit$fitTPU) # put relevant data together in a vector of values
colnames(curve19_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, curve19_output) # add the curve fits to the larger data frame

### curve20_data
curve20_data <- subset(aci.df, unique_id == aci.df.unique_id[20]) # find correct curve from full dataframe and make new object
plot(curve20_data$A~curve20_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
curve20_fit <- fitaci(curve20_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(curve20_fit) # take a look at fitted values, adjust as needed
plot(curve20_fit) # plot the fitted curves over the raw data, adjust as needed
curve20_output <- cbind('curve20', curve20_data$id[1], curve20_data$unique_id[1], curve20_data$machine[1], curve20_data$baseline_yn[1],
                        curve20_data$A[1], curve20_data$Ci[1], curve20_data$gsw[1],
                        mean(curve20_data$VPDleaf, na.rm = T), mean(curve20_data$Tleaf, na.rm = T), mean(curve20_data$Qin, na.rm = T),
                        curve20_fit[[2]][1,1], curve20_fit[[2]][1,2],
                        curve20_fit[[2]][2,1], curve20_fit[[2]][2,2],
                        curve20_fit[[2]][3,1], curve20_fit[[2]][3,2],
                        curve20_fit$RMSE,
                        curve20_fit$Ci_transition,
                        curve20_fit$citransition,
                        curve20_fit$Km,
                        curve20_fit$GammaStar,
                        curve20_fit$fitmethod,
                        curve20_fit$Tcorrect,
                        curve20_fit$fitTPU) # put relevant data together in a vector of values
colnames(curve20_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, curve20_output) # add the curve fits to the larger data frame

### curve21_data
curve21_data <- subset(aci.df, unique_id == aci.df.unique_id[21]) # find correct curve from full dataframe and make new object
plot(curve21_data$A~curve21_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
curve21_fit <- fitaci(curve21_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(curve21_fit) # take a look at fitted values, adjust as needed
plot(curve21_fit) # plot the fitted curves over the raw data, adjust as needed
curve21_output <- cbind('curve21', curve21_data$id[1], curve21_data$unique_id[1], curve21_data$machine[1], curve21_data$baseline_yn[1],
                        curve21_data$A[1], curve21_data$Ci[1], curve21_data$gsw[1],
                        mean(curve21_data$VPDleaf, na.rm = T), mean(curve21_data$Tleaf, na.rm = T), mean(curve21_data$Qin, na.rm = T),
                        curve21_fit[[2]][1,1], curve21_fit[[2]][1,2],
                        curve21_fit[[2]][2,1], curve21_fit[[2]][2,2],
                        curve21_fit[[2]][3,1], curve21_fit[[2]][3,2],
                        curve21_fit$RMSE,
                        curve21_fit$Ci_transition,
                        curve21_fit$citransition,
                        curve21_fit$Km,
                        curve21_fit$GammaStar,
                        curve21_fit$fitmethod,
                        curve21_fit$Tcorrect,
                        curve21_fit$fitTPU) # put relevant data together in a vector of values
colnames(curve21_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, curve21_output) # add the curve fits to the larger data frame

### Curve22_data
Curve22_data <- subset(aci.df, unique_id == aci.df.unique_id[22]) # find correct curve from full dataframe and make new object
plot(Curve22_data$A~Curve22_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve22_fit <- fitaci(Curve22_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve22_fit) # take a look at fitted values, adjust as needed
plot(Curve22_fit) # plot the fitted curves over the raw data, adjust as needed
Curve22_output <- cbind('Curve22', Curve22_data$id[1], Curve22_data$unique_id[1], Curve22_data$machine[1], Curve22_data$baseline_yn[1],
                        Curve22_data$A[1], Curve22_data$Ci[1], Curve22_data$gsw[1],
                        mean(Curve22_data$VPDleaf, na.rm = T), mean(Curve22_data$Tleaf, na.rm = T), mean(Curve22_data$Qin, na.rm = T),
                        Curve22_fit[[2]][1,1], Curve22_fit[[2]][1,2],
                        Curve22_fit[[2]][2,1], Curve22_fit[[2]][2,2],
                        Curve22_fit[[2]][3,1], Curve22_fit[[2]][3,2],
                        Curve22_fit$RMSE,
                        Curve22_fit$Ci_transition,
                        Curve22_fit$citransition,
                        Curve22_fit$Km,
                        Curve22_fit$GammaStar,
                        Curve22_fit$fitmethod,
                        Curve22_fit$Tcorrect,
                        Curve22_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve22_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve22_output) # add the curve fits to the larger data frame

### Curve23_data
Curve23_data <- subset(aci.df, unique_id == aci.df.unique_id[23] & Ci < 1000) # find correct curve from full dataframe and make new object
plot(Curve23_data$A~Curve23_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve23_fit <- fitaci(Curve23_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve23_fit) # take a look at fitted values, adjust as needed
plot(Curve23_fit) # plot the fitted curves over the raw data, adjust as needed
Curve23_output <- cbind('Curve23', Curve23_data$id[1], Curve23_data$unique_id[1], Curve23_data$machine[1], Curve23_data$baseline_yn[1],
                        Curve23_data$A[1], Curve23_data$Ci[1], Curve23_data$gsw[1],
                        mean(Curve23_data$VPDleaf, na.rm = T), mean(Curve23_data$Tleaf, na.rm = T), mean(Curve23_data$Qin, na.rm = T),
                        Curve23_fit[[2]][1,1], Curve23_fit[[2]][1,2],
                        Curve23_fit[[2]][2,1], Curve23_fit[[2]][2,2],
                        Curve23_fit[[2]][3,1], Curve23_fit[[2]][3,2],
                        Curve23_fit$RMSE,
                        Curve23_fit$Ci_transition,
                        Curve23_fit$citransition,
                        Curve23_fit$Km,
                        Curve23_fit$GammaStar,
                        Curve23_fit$fitmethod,
                        Curve23_fit$Tcorrect,
                        Curve23_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve23_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve23_output) # add the curve fits to the larger data frame

### Curve24_data
Curve24_data <- subset(aci.df, unique_id == aci.df.unique_id[24]) # find correct curve from full dataframe and make new object
plot(Curve24_data$A~Curve24_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve24_fit <- fitaci(Curve24_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve24_fit) # take a look at fitted values, adjust as needed
plot(Curve24_fit) # plot the fitted curves over the raw data, adjust as needed
Curve24_output <- cbind('Curve24', Curve24_data$id[1], Curve24_data$unique_id[1], Curve24_data$machine[1], Curve24_data$baseline_yn[1],
                        Curve24_data$A[1], Curve24_data$Ci[1], Curve24_data$gsw[1],
                        mean(Curve24_data$VPDleaf, na.rm = T), mean(Curve24_data$Tleaf, na.rm = T), mean(Curve24_data$Qin, na.rm = T),
                        Curve24_fit[[2]][1,1], Curve24_fit[[2]][1,2],
                        Curve24_fit[[2]][2,1], Curve24_fit[[2]][2,2],
                        Curve24_fit[[2]][3,1], Curve24_fit[[2]][3,2],
                        Curve24_fit$RMSE,
                        Curve24_fit$Ci_transition,
                        Curve24_fit$citransition,
                        Curve24_fit$Km,
                        Curve24_fit$GammaStar,
                        Curve24_fit$fitmethod,
                        Curve24_fit$Tcorrect,
                        Curve24_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve24_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve24_output) # add the curve fits to the larger data frame

### Curve25_data
Curve25_data <- subset(aci.df, unique_id == aci.df.unique_id[25]) # find correct curve from full dataframe and make new object
plot(Curve25_data$A~Curve25_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve25_fit <- fitaci(Curve25_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve25_fit) # take a look at fitted values, adjust as needed
plot(Curve25_fit) # plot the fitted curves over the raw data, adjust as needed
Curve25_output <- cbind('Curve25', Curve25_data$id[1], Curve25_data$unique_id[1], Curve25_data$machine[1], Curve25_data$baseline_yn[1],
                        Curve25_data$A[1], Curve25_data$Ci[1], Curve25_data$gsw[1],
                        mean(Curve25_data$VPDleaf, na.rm = T), mean(Curve25_data$Tleaf, na.rm = T), mean(Curve25_data$Qin, na.rm = T),
                        Curve25_fit[[2]][1,1], Curve25_fit[[2]][1,2],
                        Curve25_fit[[2]][2,1], Curve25_fit[[2]][2,2],
                        Curve25_fit[[2]][3,1], Curve25_fit[[2]][3,2],
                        Curve25_fit$RMSE,
                        Curve25_fit$Ci_transition,
                        Curve25_fit$citransition,
                        Curve25_fit$Km,
                        Curve25_fit$GammaStar,
                        Curve25_fit$fitmethod,
                        Curve25_fit$Tcorrect,
                        Curve25_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve25_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve25_output) # add the curve fits to the larger data frame

### Curve26_data
Curve26_data <- subset(aci.df, unique_id == aci.df.unique_id[26]) # find correct curve from full dataframe and make new object
plot(Curve26_data$A~Curve26_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve26_fit <- fitaci(Curve26_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve26_fit) # take a look at fitted values, adjust as needed
plot(Curve26_fit) # plot the fitted curves over the raw data, adjust as needed
Curve26_output <- cbind('Curve26', Curve26_data$id[1], Curve26_data$unique_id[1], Curve26_data$machine[1], Curve26_data$baseline_yn[1],
                        Curve26_data$A[1], Curve26_data$Ci[1], Curve26_data$gsw[1],
                        mean(Curve26_data$VPDleaf, na.rm = T), mean(Curve26_data$Tleaf, na.rm = T), mean(Curve26_data$Qin, na.rm = T),
                        Curve26_fit[[2]][1,1], Curve26_fit[[2]][1,2],
                        Curve26_fit[[2]][2,1], Curve26_fit[[2]][2,2],
                        Curve26_fit[[2]][3,1], Curve26_fit[[2]][3,2],
                        Curve26_fit$RMSE,
                        Curve26_fit$Ci_transition,
                        Curve26_fit$citransition,
                        Curve26_fit$Km,
                        Curve26_fit$GammaStar,
                        Curve26_fit$fitmethod,
                        Curve26_fit$Tcorrect,
                        Curve26_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve26_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve26_output) # add the curve fits to the larger data frame

### Curve27_data
Curve27_data <- subset(aci.df, unique_id == aci.df.unique_id[27]) # find correct curve from full dataframe and make new object
plot(Curve27_data$A~Curve27_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve27_fit <- fitaci(Curve27_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve27_fit) # take a look at fitted values, adjust as needed
plot(Curve27_fit) # plot the fitted curves over the raw data, adjust as needed
Curve27_output <- cbind('Curve27', Curve27_data$id[1], Curve27_data$unique_id[1], Curve27_data$machine[1], Curve27_data$baseline_yn[1],
                        Curve27_data$A[1], Curve27_data$Ci[1], Curve27_data$gsw[1],
                        mean(Curve27_data$VPDleaf, na.rm = T), mean(Curve27_data$Tleaf, na.rm = T), mean(Curve27_data$Qin, na.rm = T),
                        Curve27_fit[[2]][1,1], Curve27_fit[[2]][1,2],
                        Curve27_fit[[2]][2,1], Curve27_fit[[2]][2,2],
                        Curve27_fit[[2]][3,1], Curve27_fit[[2]][3,2],
                        Curve27_fit$RMSE,
                        Curve27_fit$Ci_transition,
                        Curve27_fit$citransition,
                        Curve27_fit$Km,
                        Curve27_fit$GammaStar,
                        Curve27_fit$fitmethod,
                        Curve27_fit$Tcorrect,
                        Curve27_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve27_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve27_output) # add the curve fits to the larger data frame

### Curve28_data
Curve28_data <- subset(aci.df, unique_id == aci.df.unique_id[28]) # find correct curve from full dataframe and make new object
plot(Curve28_data$A~Curve28_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve28_fit <- fitaci(Curve28_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve28_fit) # take a look at fitted values, adjust as needed
plot(Curve28_fit) # plot the fitted curves over the raw data, adjust as needed
Curve28_output <- cbind('Curve28', Curve28_data$id[1], Curve28_data$unique_id[1], Curve28_data$machine[1], Curve28_data$baseline_yn[1],
                        Curve28_data$A[1], Curve28_data$Ci[1], Curve28_data$gsw[1],
                        mean(Curve28_data$VPDleaf, na.rm = T), mean(Curve28_data$Tleaf, na.rm = T), mean(Curve28_data$Qin, na.rm = T),
                        Curve28_fit[[2]][1,1], Curve28_fit[[2]][1,2],
                        Curve28_fit[[2]][2,1], Curve28_fit[[2]][2,2],
                        Curve28_fit[[2]][3,1], Curve28_fit[[2]][3,2],
                        Curve28_fit$RMSE,
                        Curve28_fit$Ci_transition,
                        Curve28_fit$citransition,
                        Curve28_fit$Km,
                        Curve28_fit$GammaStar,
                        Curve28_fit$fitmethod,
                        Curve28_fit$Tcorrect,
                        Curve28_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve28_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve28_output) # add the curve fits to the larger data frame

### Curve29_data
Curve29_data <- subset(aci.df, unique_id == aci.df.unique_id[29]) # find correct curve from full dataframe and make new object
plot(Curve29_data$A~Curve29_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve29_fit <- fitaci(Curve29_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve29_fit) # take a look at fitted values, adjust as needed
plot(Curve29_fit) # plot the fitted curves over the raw data, adjust as needed
Curve29_output <- cbind('Curve29', Curve29_data$id[1], Curve29_data$unique_id[1], Curve29_data$machine[1], Curve29_data$baseline_yn[1],
                        Curve29_data$A[1], Curve29_data$Ci[1], Curve29_data$gsw[1],
                        mean(Curve29_data$VPDleaf, na.rm = T), mean(Curve29_data$Tleaf, na.rm = T), mean(Curve29_data$Qin, na.rm = T),
                        Curve29_fit[[2]][1,1], Curve29_fit[[2]][1,2],
                        Curve29_fit[[2]][2,1], Curve29_fit[[2]][2,2],
                        Curve29_fit[[2]][3,1], Curve29_fit[[2]][3,2],
                        Curve29_fit$RMSE,
                        Curve29_fit$Ci_transition,
                        Curve29_fit$citransition,
                        Curve29_fit$Km,
                        Curve29_fit$GammaStar,
                        Curve29_fit$fitmethod,
                        Curve29_fit$Tcorrect,
                        Curve29_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve29_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve29_output) # add the curve fits to the larger data frame

### Curve30_data
Curve30_data <- subset(aci.df, unique_id == aci.df.unique_id[30]) # find correct curve from full dataframe and make new object
plot(Curve30_data$A~Curve30_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve30_fit <- fitaci(Curve30_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve30_fit) # take a look at fitted values, adjust as needed
plot(Curve30_fit) # plot the fitted curves over the raw data, adjust as needed
Curve30_output <- cbind('Curve30', Curve30_data$id[1], Curve30_data$unique_id[1], Curve30_data$machine[1], Curve30_data$baseline_yn[1],
                        Curve30_data$A[1], Curve30_data$Ci[1], Curve30_data$gsw[1],
                        mean(Curve30_data$VPDleaf, na.rm = T), mean(Curve30_data$Tleaf, na.rm = T), mean(Curve30_data$Qin, na.rm = T),
                        Curve30_fit[[2]][1,1], Curve30_fit[[2]][1,2],
                        Curve30_fit[[2]][2,1], Curve30_fit[[2]][2,2],
                        Curve30_fit[[2]][3,1], Curve30_fit[[2]][3,2],
                        Curve30_fit$RMSE,
                        Curve30_fit$Ci_transition,
                        Curve30_fit$citransition,
                        Curve30_fit$Km,
                        Curve30_fit$GammaStar,
                        Curve30_fit$fitmethod,
                        Curve30_fit$Tcorrect,
                        Curve30_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve30_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve30_output) # add the curve fits to the larger data frame

### Curve31_data
Curve31_data <- subset(aci.df, unique_id == aci.df.unique_id[31]) # find correct curve from full dataframe and make new object
plot(Curve31_data$A~Curve31_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve31_fit <- fitaci(Curve31_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve31_fit) # take a look at fitted values, adjust as needed
plot(Curve31_fit) # plot the fitted curves over the raw data, adjust as needed
Curve31_output <- cbind('Curve31', Curve31_data$id[1], Curve31_data$unique_id[1], Curve31_data$machine[1], Curve31_data$baseline_yn[1],
                        Curve31_data$A[1], Curve31_data$Ci[1], Curve31_data$gsw[1],
                        mean(Curve31_data$VPDleaf, na.rm = T), mean(Curve31_data$Tleaf, na.rm = T), mean(Curve31_data$Qin, na.rm = T),
                        Curve31_fit[[2]][1,1], Curve31_fit[[2]][1,2],
                        Curve31_fit[[2]][2,1], Curve31_fit[[2]][2,2],
                        Curve31_fit[[2]][3,1], Curve31_fit[[2]][3,2],
                        Curve31_fit$RMSE,
                        Curve31_fit$Ci_transition,
                        Curve31_fit$citransition,
                        Curve31_fit$Km,
                        Curve31_fit$GammaStar,
                        Curve31_fit$fitmethod,
                        Curve31_fit$Tcorrect,
                        Curve31_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve31_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve31_output) # add the curve fits to the larger data frame

### Curve32_data
Curve32_data <- subset(aci.df, unique_id == aci.df.unique_id[32]) # find correct curve from full dataframe and make new object
plot(Curve32_data$A~Curve32_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve32_fit <- fitaci(Curve32_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve32_fit) # take a look at fitted values, adjust as needed
plot(Curve32_fit) # plot the fitted curves over the raw data, adjust as needed
Curve32_output <- cbind('Curve32', Curve32_data$id[1], Curve32_data$unique_id[1], Curve32_data$machine[1], Curve32_data$baseline_yn[1],
                        Curve32_data$A[1], Curve32_data$Ci[1], Curve32_data$gsw[1],
                        mean(Curve32_data$VPDleaf, na.rm = T), mean(Curve32_data$Tleaf, na.rm = T), mean(Curve32_data$Qin, na.rm = T),
                        Curve32_fit[[2]][1,1], Curve32_fit[[2]][1,2],
                        Curve32_fit[[2]][2,1], Curve32_fit[[2]][2,2],
                        Curve32_fit[[2]][3,1], Curve32_fit[[2]][3,2],
                        Curve32_fit$RMSE,
                        Curve32_fit$Ci_transition,
                        Curve32_fit$citransition,
                        Curve32_fit$Km,
                        Curve32_fit$GammaStar,
                        Curve32_fit$fitmethod,
                        Curve32_fit$Tcorrect,
                        Curve32_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve32_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve32_output) # add the curve fits to the larger data frame

### Curve33_data
Curve33_data <- subset(aci.df, unique_id == aci.df.unique_id[33]) # find correct curve from full dataframe and make new object
plot(Curve33_data$A~Curve33_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve33_fit <- fitaci(Curve33_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve33_fit) # take a look at fitted values, adjust as needed
plot(Curve33_fit) # plot the fitted curves over the raw data, adjust as needed
Curve33_output <- cbind('Curve33', Curve33_data$id[1], Curve33_data$unique_id[1], Curve33_data$machine[1], Curve33_data$baseline_yn[1],
                        Curve33_data$A[1], Curve33_data$Ci[1], Curve33_data$gsw[1],
                        mean(Curve33_data$VPDleaf, na.rm = T), mean(Curve33_data$Tleaf, na.rm = T), mean(Curve33_data$Qin, na.rm = T),
                        Curve33_fit[[2]][1,1], Curve33_fit[[2]][1,2],
                        Curve33_fit[[2]][2,1], Curve33_fit[[2]][2,2],
                        Curve33_fit[[2]][3,1], Curve33_fit[[2]][3,2],
                        Curve33_fit$RMSE,
                        Curve33_fit$Ci_transition,
                        Curve33_fit$citransition,
                        Curve33_fit$Km,
                        Curve33_fit$GammaStar,
                        Curve33_fit$fitmethod,
                        Curve33_fit$Tcorrect,
                        Curve33_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve33_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve33_output) # add the curve fits to the larger data frame

### Curve34_data
Curve34_data <- subset(aci.df, unique_id == aci.df.unique_id[34] & Ci < 1000) # find correct curve from full dataframe and make new object
plot(Curve34_data$A~Curve34_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve34_fit <- fitaci(Curve34_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve34_fit) # take a look at fitted values, adjust as needed
plot(Curve34_fit) # plot the fitted curves over the raw data, adjust as needed
Curve34_output <- cbind('Curve34', Curve34_data$id[1], Curve34_data$unique_id[1], Curve34_data$machine[1], Curve34_data$baseline_yn[1],
                        Curve34_data$A[1], Curve34_data$Ci[1], Curve34_data$gsw[1],
                        mean(Curve34_data$VPDleaf, na.rm = T), mean(Curve34_data$Tleaf, na.rm = T), mean(Curve34_data$Qin, na.rm = T),
                        Curve34_fit[[2]][1,1], Curve34_fit[[2]][1,2],
                        Curve34_fit[[2]][2,1], Curve34_fit[[2]][2,2],
                        Curve34_fit[[2]][3,1], Curve34_fit[[2]][3,2],
                        Curve34_fit$RMSE,
                        Curve34_fit$Ci_transition,
                        Curve34_fit$citransition,
                        Curve34_fit$Km,
                        Curve34_fit$GammaStar,
                        Curve34_fit$fitmethod,
                        Curve34_fit$Tcorrect,
                        Curve34_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve34_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve34_output) # add the curve fits to the larger data frame

### Curve35_data
Curve35_data <- subset(aci.df, unique_id == aci.df.unique_id[35]) # find correct curve from full dataframe and make new object
plot(Curve35_data$A~Curve35_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve35_fit <- fitaci(Curve35_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve35_fit) # take a look at fitted values, adjust as needed
plot(Curve35_fit) # plot the fitted curves over the raw data, adjust as needed
Curve35_output <- cbind('Curve35', Curve35_data$id[1], Curve35_data$unique_id[1], Curve35_data$machine[1], Curve35_data$baseline_yn[1],
                        Curve35_data$A[1], Curve35_data$Ci[1], Curve35_data$gsw[1],
                        mean(Curve35_data$VPDleaf, na.rm = T), mean(Curve35_data$Tleaf, na.rm = T), mean(Curve35_data$Qin, na.rm = T),
                        Curve35_fit[[2]][1,1], Curve35_fit[[2]][1,2],
                        Curve35_fit[[2]][2,1], Curve35_fit[[2]][2,2],
                        Curve35_fit[[2]][3,1], Curve35_fit[[2]][3,2],
                        Curve35_fit$RMSE,
                        Curve35_fit$Ci_transition,
                        Curve35_fit$citransition,
                        Curve35_fit$Km,
                        Curve35_fit$GammaStar,
                        Curve35_fit$fitmethod,
                        Curve35_fit$Tcorrect,
                        Curve35_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve35_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve35_output) # add the curve fits to the larger data frame

### Curve36_data
Curve36_data <- subset(aci.df, unique_id == aci.df.unique_id[36]) # find correct curve from full dataframe and make new object
plot(Curve36_data$A~Curve36_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve36_fit <- fitaci(Curve36_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve36_fit) # take a look at fitted values, adjust as needed
plot(Curve36_fit) # plot the fitted curves over the raw data, adjust as needed
Curve36_output <- cbind('Curve36', Curve36_data$id[1], Curve36_data$unique_id[1], Curve36_data$machine[1], Curve36_data$baseline_yn[1],
                        Curve36_data$A[1], Curve36_data$Ci[1], Curve36_data$gsw[1],
                        mean(Curve36_data$VPDleaf, na.rm = T), mean(Curve36_data$Tleaf, na.rm = T), mean(Curve36_data$Qin, na.rm = T),
                        Curve36_fit[[2]][1,1], Curve36_fit[[2]][1,2],
                        Curve36_fit[[2]][2,1], Curve36_fit[[2]][2,2],
                        Curve36_fit[[2]][3,1], Curve36_fit[[2]][3,2],
                        Curve36_fit$RMSE,
                        Curve36_fit$Ci_transition,
                        Curve36_fit$citransition,
                        Curve36_fit$Km,
                        Curve36_fit$GammaStar,
                        Curve36_fit$fitmethod,
                        Curve36_fit$Tcorrect,
                        Curve36_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve36_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve36_output) # add the curve fits to the larger data frame

### Curve37_data
Curve37_data <- subset(aci.df, unique_id == aci.df.unique_id[37]) # find correct curve from full dataframe and make new object
plot(Curve37_data$A~Curve37_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve37_fit <- fitaci(Curve37_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve37_fit) # take a look at fitted values, adjust as needed
plot(Curve37_fit) # plot the fitted curves over the raw data, adjust as needed
Curve37_output <- cbind('Curve37', Curve37_data$id[1], Curve37_data$unique_id[1], Curve37_data$machine[1], Curve37_data$baseline_yn[1],
                        Curve37_data$A[1], Curve37_data$Ci[1], Curve37_data$gsw[1],
                        mean(Curve37_data$VPDleaf, na.rm = T), mean(Curve37_data$Tleaf, na.rm = T), mean(Curve37_data$Qin, na.rm = T),
                        Curve37_fit[[2]][1,1], Curve37_fit[[2]][1,2],
                        Curve37_fit[[2]][2,1], Curve37_fit[[2]][2,2],
                        Curve37_fit[[2]][3,1], Curve37_fit[[2]][3,2],
                        Curve37_fit$RMSE,
                        Curve37_fit$Ci_transition,
                        Curve37_fit$citransition,
                        Curve37_fit$Km,
                        Curve37_fit$GammaStar,
                        Curve37_fit$fitmethod,
                        Curve37_fit$Tcorrect,
                        Curve37_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve37_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve37_output) # add the curve fits to the larger data frame

### Curve38_data
Curve38_data <- subset(aci.df, unique_id == aci.df.unique_id[38]) # find correct curve from full dataframe and make new object
plot(Curve38_data$A~Curve38_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve38_fit <- fitaci(Curve38_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve38_fit) # take a look at fitted values, adjust as needed
plot(Curve38_fit) # plot the fitted curves over the raw data, adjust as needed
Curve38_output <- cbind('Curve38', Curve38_data$id[1], Curve38_data$unique_id[1], Curve38_data$machine[1], Curve38_data$baseline_yn[1],
                        Curve38_data$A[1], Curve38_data$Ci[1], Curve38_data$gsw[1],
                        mean(Curve38_data$VPDleaf, na.rm = T), mean(Curve38_data$Tleaf, na.rm = T), mean(Curve38_data$Qin, na.rm = T),
                        Curve38_fit[[2]][1,1], Curve38_fit[[2]][1,2],
                        Curve38_fit[[2]][2,1], Curve38_fit[[2]][2,2],
                        Curve38_fit[[2]][3,1], Curve38_fit[[2]][3,2],
                        Curve38_fit$RMSE,
                        Curve38_fit$Ci_transition,
                        Curve38_fit$citransition,
                        Curve38_fit$Km,
                        Curve38_fit$GammaStar,
                        Curve38_fit$fitmethod,
                        Curve38_fit$Tcorrect,
                        Curve38_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve38_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve38_output) # add the curve fits to the larger data frame

### Curve39_data
Curve39_data <- subset(aci.df, unique_id == aci.df.unique_id[39]) # find correct curve from full dataframe and make new object
plot(Curve39_data$A~Curve39_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve39_fit <- fitaci(Curve39_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve39_fit) # take a look at fitted values, adjust as needed
plot(Curve39_fit) # plot the fitted curves over the raw data, adjust as needed
Curve39_output <- cbind('Curve39', Curve39_data$id[1], Curve39_data$unique_id[1], Curve39_data$machine[1], Curve39_data$baseline_yn[1],
                        Curve39_data$A[1], Curve39_data$Ci[1], Curve39_data$gsw[1],
                        mean(Curve39_data$VPDleaf, na.rm = T), mean(Curve39_data$Tleaf, na.rm = T), mean(Curve39_data$Qin, na.rm = T),
                        Curve39_fit[[2]][1,1], Curve39_fit[[2]][1,2],
                        Curve39_fit[[2]][2,1], Curve39_fit[[2]][2,2],
                        Curve39_fit[[2]][3,1], Curve39_fit[[2]][3,2],
                        Curve39_fit$RMSE,
                        Curve39_fit$Ci_transition,
                        Curve39_fit$citransition,
                        Curve39_fit$Km,
                        Curve39_fit$GammaStar,
                        Curve39_fit$fitmethod,
                        Curve39_fit$Tcorrect,
                        Curve39_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve39_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve39_output) # add the curve fits to the larger data frame

### Curve40_data
Curve40_data <- subset(aci.df, unique_id == aci.df.unique_id[40] & Ci < 1000) # find correct curve from full dataframe and make new object
plot(Curve40_data$A~Curve40_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve40_fit <- fitaci(Curve40_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve40_fit) # take a look at fitted values, adjust as needed
plot(Curve40_fit) # plot the fitted curves over the raw data, adjust as needed
Curve40_output <- cbind('Curve40', Curve40_data$id[1], Curve40_data$unique_id[1], Curve40_data$machine[1], Curve40_data$baseline_yn[1],
                        Curve40_data$A[1], Curve40_data$Ci[1], Curve40_data$gsw[1],
                        mean(Curve40_data$VPDleaf, na.rm = T), mean(Curve40_data$Tleaf, na.rm = T), mean(Curve40_data$Qin, na.rm = T),
                        Curve40_fit[[2]][1,1], Curve40_fit[[2]][1,2],
                        Curve40_fit[[2]][2,1], Curve40_fit[[2]][2,2],
                        Curve40_fit[[2]][3,1], Curve40_fit[[2]][3,2],
                        Curve40_fit$RMSE,
                        Curve40_fit$Ci_transition,
                        Curve40_fit$citransition,
                        Curve40_fit$Km,
                        Curve40_fit$GammaStar,
                        Curve40_fit$fitmethod,
                        Curve40_fit$Tcorrect,
                        Curve40_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve40_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve40_output) # add the curve fits to the larger data frame

### Curve41_data
Curve41_data <- subset(aci.df, unique_id == aci.df.unique_id[41]) # find correct curve from full dataframe and make new object
plot(Curve41_data$A~Curve41_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve41_fit <- fitaci(Curve41_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve41_fit) # take a look at fitted values, adjust as needed
plot(Curve41_fit) # plot the fitted curves over the raw data, adjust as needed
Curve41_output <- cbind('Curve41', Curve41_data$id[1], Curve41_data$unique_id[1], Curve41_data$machine[1], Curve41_data$baseline_yn[1],
                        Curve41_data$A[1], Curve41_data$Ci[1], Curve41_data$gsw[1],
                        mean(Curve41_data$VPDleaf, na.rm = T), mean(Curve41_data$Tleaf, na.rm = T), mean(Curve41_data$Qin, na.rm = T),
                        Curve41_fit[[2]][1,1], Curve41_fit[[2]][1,2],
                        Curve41_fit[[2]][2,1], Curve41_fit[[2]][2,2],
                        Curve41_fit[[2]][3,1], Curve41_fit[[2]][3,2],
                        Curve41_fit$RMSE,
                        Curve41_fit$Ci_transition,
                        Curve41_fit$citransition,
                        Curve41_fit$Km,
                        Curve41_fit$GammaStar,
                        Curve41_fit$fitmethod,
                        Curve41_fit$Tcorrect,
                        Curve41_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve41_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve41_output) # add the curve fits to the larger data frame

### Curve42_data
Curve42_data <- subset(aci.df, unique_id == aci.df.unique_id[42]) # find correct curve from full dataframe and make new object
plot(Curve42_data$A~Curve42_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve42_fit <- fitaci(Curve42_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve42_fit) # take a look at fitted values, adjust as needed
plot(Curve42_fit) # plot the fitted curves over the raw data, adjust as needed
Curve42_output <- cbind('Curve42', Curve42_data$id[1], Curve42_data$unique_id[1], Curve42_data$machine[1], Curve42_data$baseline_yn[1],
                        Curve42_data$A[1], Curve42_data$Ci[1], Curve42_data$gsw[1],
                        mean(Curve42_data$VPDleaf, na.rm = T), mean(Curve42_data$Tleaf, na.rm = T), mean(Curve42_data$Qin, na.rm = T),
                        Curve42_fit[[2]][1,1], Curve42_fit[[2]][1,2],
                        Curve42_fit[[2]][2,1], Curve42_fit[[2]][2,2],
                        Curve42_fit[[2]][3,1], Curve42_fit[[2]][3,2],
                        Curve42_fit$RMSE,
                        Curve42_fit$Ci_transition,
                        Curve42_fit$citransition,
                        Curve42_fit$Km,
                        Curve42_fit$GammaStar,
                        Curve42_fit$fitmethod,
                        Curve42_fit$Tcorrect,
                        Curve42_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve42_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve42_output) # add the curve fits to the larger data frame

### Curve43_data
Curve43_data <- subset(aci.df, unique_id == aci.df.unique_id[43]) # find correct curve from full dataframe and make new object
plot(Curve43_data$A~Curve43_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve43_fit <- fitaci(Curve43_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve43_fit) # take a look at fitted values, adjust as needed
plot(Curve43_fit) # plot the fitted curves over the raw data, adjust as needed
Curve43_output <- cbind('Curve43', Curve43_data$id[1], Curve43_data$unique_id[1], Curve43_data$machine[1], Curve43_data$baseline_yn[1],
                        Curve43_data$A[1], Curve43_data$Ci[1], Curve43_data$gsw[1],
                        mean(Curve43_data$VPDleaf, na.rm = T), mean(Curve43_data$Tleaf, na.rm = T), mean(Curve43_data$Qin, na.rm = T),
                        Curve43_fit[[2]][1,1], Curve43_fit[[2]][1,2],
                        Curve43_fit[[2]][2,1], Curve43_fit[[2]][2,2],
                        Curve43_fit[[2]][3,1], Curve43_fit[[2]][3,2],
                        Curve43_fit$RMSE,
                        Curve43_fit$Ci_transition,
                        Curve43_fit$citransition,
                        Curve43_fit$Km,
                        Curve43_fit$GammaStar,
                        Curve43_fit$fitmethod,
                        Curve43_fit$Tcorrect,
                        Curve43_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve43_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve43_output) # add the curve fits to the larger data frame

### Curve44_data
Curve44_data <- subset(aci.df, unique_id == aci.df.unique_id[44]) # find correct curve from full dataframe and make new object
plot(Curve44_data$A~Curve44_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve44_fit <- fitaci(Curve44_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve44_fit) # take a look at fitted values, adjust as needed
plot(Curve44_fit) # plot the fitted curves over the raw data, adjust as needed
Curve44_output <- cbind('Curve44', Curve44_data$id[1], Curve44_data$unique_id[1], Curve44_data$machine[1], Curve44_data$baseline_yn[1],
                        Curve44_data$A[1], Curve44_data$Ci[1], Curve44_data$gsw[1],
                        mean(Curve44_data$VPDleaf, na.rm = T), mean(Curve44_data$Tleaf, na.rm = T), mean(Curve44_data$Qin, na.rm = T),
                        Curve44_fit[[2]][1,1], Curve44_fit[[2]][1,2],
                        Curve44_fit[[2]][2,1], Curve44_fit[[2]][2,2],
                        Curve44_fit[[2]][3,1], Curve44_fit[[2]][3,2],
                        Curve44_fit$RMSE,
                        Curve44_fit$Ci_transition,
                        Curve44_fit$citransition,
                        Curve44_fit$Km,
                        Curve44_fit$GammaStar,
                        Curve44_fit$fitmethod,
                        Curve44_fit$Tcorrect,
                        Curve44_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve44_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve44_output) # add the curve fits to the larger data frame

### Curve45_data
Curve45_data <- subset(aci.df, unique_id == aci.df.unique_id[45] & Ci < 1000) # find correct curve from full dataframe and make new object
plot(Curve45_data$A~Curve45_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve45_fit <- fitaci(Curve45_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve45_fit) # take a look at fitted values, adjust as needed
plot(Curve45_fit) # plot the fitted curves over the raw data, adjust as needed
Curve45_output <- cbind('Curve45', Curve45_data$id[1], Curve45_data$unique_id[1], Curve45_data$machine[1], Curve45_data$baseline_yn[1],
                        Curve45_data$A[1], Curve45_data$Ci[1], Curve45_data$gsw[1],
                        mean(Curve45_data$VPDleaf, na.rm = T), mean(Curve45_data$Tleaf, na.rm = T), mean(Curve45_data$Qin, na.rm = T),
                        Curve45_fit[[2]][1,1], Curve45_fit[[2]][1,2],
                        Curve45_fit[[2]][2,1], Curve45_fit[[2]][2,2],
                        Curve45_fit[[2]][3,1], Curve45_fit[[2]][3,2],
                        Curve45_fit$RMSE,
                        Curve45_fit$Ci_transition,
                        Curve45_fit$citransition,
                        Curve45_fit$Km,
                        Curve45_fit$GammaStar,
                        Curve45_fit$fitmethod,
                        Curve45_fit$Tcorrect,
                        Curve45_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve45_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve45_output) # add the curve fits to the larger data frame

### Curve46_data
Curve46_data <- subset(aci.df, unique_id == aci.df.unique_id[46]) # find correct curve from full dataframe and make new object
plot(Curve46_data$A~Curve46_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve46_fit <- fitaci(Curve46_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve46_fit) # take a look at fitted values, adjust as needed
plot(Curve46_fit) # plot the fitted curves over the raw data, adjust as needed
Curve46_output <- cbind('Curve46', Curve46_data$id[1], Curve46_data$unique_id[1], Curve46_data$machine[1], Curve46_data$baseline_yn[1],
                        Curve46_data$A[1], Curve46_data$Ci[1], Curve46_data$gsw[1],
                        mean(Curve46_data$VPDleaf, na.rm = T), mean(Curve46_data$Tleaf, na.rm = T), mean(Curve46_data$Qin, na.rm = T),
                        Curve46_fit[[2]][1,1], Curve46_fit[[2]][1,2],
                        Curve46_fit[[2]][2,1], Curve46_fit[[2]][2,2],
                        Curve46_fit[[2]][3,1], Curve46_fit[[2]][3,2],
                        Curve46_fit$RMSE,
                        Curve46_fit$Ci_transition,
                        Curve46_fit$citransition,
                        Curve46_fit$Km,
                        Curve46_fit$GammaStar,
                        Curve46_fit$fitmethod,
                        Curve46_fit$Tcorrect,
                        Curve46_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve46_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve46_output) # add the curve fits to the larger data frame

### Curve47_data
Curve47_data <- subset(aci.df, unique_id == aci.df.unique_id[47]) # find correct curve from full dataframe and make new object
plot(Curve47_data$A~Curve47_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve47_fit <- fitaci(Curve47_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve47_fit) # take a look at fitted values, adjust as needed
plot(Curve47_fit) # plot the fitted curves over the raw data, adjust as needed
Curve47_output <- cbind('Curve47', Curve47_data$id[1], Curve47_data$unique_id[1], Curve47_data$machine[1], Curve47_data$baseline_yn[1],
                        Curve47_data$A[1], Curve47_data$Ci[1], Curve47_data$gsw[1],
                        mean(Curve47_data$VPDleaf, na.rm = T), mean(Curve47_data$Tleaf, na.rm = T), mean(Curve47_data$Qin, na.rm = T),
                        Curve47_fit[[2]][1,1], Curve47_fit[[2]][1,2],
                        Curve47_fit[[2]][2,1], Curve47_fit[[2]][2,2],
                        Curve47_fit[[2]][3,1], Curve47_fit[[2]][3,2],
                        Curve47_fit$RMSE,
                        Curve47_fit$Ci_transition,
                        Curve47_fit$citransition,
                        Curve47_fit$Km,
                        Curve47_fit$GammaStar,
                        Curve47_fit$fitmethod,
                        Curve47_fit$Tcorrect,
                        Curve47_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve47_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve47_output) # add the curve fits to the larger data frame

### Curve48_data
Curve48_data <- subset(aci.df, unique_id == aci.df.unique_id[48]) # find correct curve from full dataframe and make new object
plot(Curve48_data$A~Curve48_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve48_fit <- fitaci(Curve48_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve48_fit) # take a look at fitted values, adjust as needed
plot(Curve48_fit) # plot the fitted curves over the raw data, adjust as needed
Curve48_output <- cbind('Curve48', Curve48_data$id[1], Curve48_data$unique_id[1], Curve48_data$machine[1], Curve48_data$baseline_yn[1],
                        Curve48_data$A[1], Curve48_data$Ci[1], Curve48_data$gsw[1],
                        mean(Curve48_data$VPDleaf, na.rm = T), mean(Curve48_data$Tleaf, na.rm = T), mean(Curve48_data$Qin, na.rm = T),
                        Curve48_fit[[2]][1,1], Curve48_fit[[2]][1,2],
                        Curve48_fit[[2]][2,1], Curve48_fit[[2]][2,2],
                        Curve48_fit[[2]][3,1], Curve48_fit[[2]][3,2],
                        Curve48_fit$RMSE,
                        Curve48_fit$Ci_transition,
                        Curve48_fit$citransition,
                        Curve48_fit$Km,
                        Curve48_fit$GammaStar,
                        Curve48_fit$fitmethod,
                        Curve48_fit$Tcorrect,
                        Curve48_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve48_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve48_output) # add the curve fits to the larger data frame

### Curve49_data
Curve49_data <- subset(aci.df, unique_id == aci.df.unique_id[49]) # find correct curve from full dataframe and make new object
plot(Curve49_data$A~Curve49_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve49_fit <- fitaci(Curve49_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve49_fit) # take a look at fitted values, adjust as needed
plot(Curve49_fit) # plot the fitted curves over the raw data, adjust as needed
Curve49_output <- cbind('Curve49', Curve49_data$id[1], Curve49_data$unique_id[1], Curve49_data$machine[1], Curve49_data$baseline_yn[1],
                        Curve49_data$A[1], Curve49_data$Ci[1], Curve49_data$gsw[1],
                        mean(Curve49_data$VPDleaf, na.rm = T), mean(Curve49_data$Tleaf, na.rm = T), mean(Curve49_data$Qin, na.rm = T),
                        Curve49_fit[[2]][1,1], Curve49_fit[[2]][1,2],
                        Curve49_fit[[2]][2,1], Curve49_fit[[2]][2,2],
                        Curve49_fit[[2]][3,1], Curve49_fit[[2]][3,2],
                        Curve49_fit$RMSE,
                        Curve49_fit$Ci_transition,
                        Curve49_fit$citransition,
                        Curve49_fit$Km,
                        Curve49_fit$GammaStar,
                        Curve49_fit$fitmethod,
                        Curve49_fit$Tcorrect,
                        Curve49_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve49_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve49_output) # add the curve fits to the larger data frame

### Curve50_data
Curve50_data <- subset(aci.df, unique_id == aci.df.unique_id[50]) # find correct curve from full dataframe and make new object
plot(Curve50_data$A~Curve50_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve50_fit <- fitaci(Curve50_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve50_fit) # take a look at fitted values, adjust as needed
plot(Curve50_fit) # plot the fitted curves over the raw data, adjust as needed
Curve50_output <- cbind('Curve50', Curve50_data$id[1], Curve50_data$unique_id[1], Curve50_data$machine[1], Curve50_data$baseline_yn[1],
                        Curve50_data$A[1], Curve50_data$Ci[1], Curve50_data$gsw[1],
                        mean(Curve50_data$VPDleaf, na.rm = T), mean(Curve50_data$Tleaf, na.rm = T), mean(Curve50_data$Qin, na.rm = T),
                        Curve50_fit[[2]][1,1], Curve50_fit[[2]][1,2],
                        Curve50_fit[[2]][2,1], Curve50_fit[[2]][2,2],
                        Curve50_fit[[2]][3,1], Curve50_fit[[2]][3,2],
                        Curve50_fit$RMSE,
                        Curve50_fit$Ci_transition,
                        Curve50_fit$citransition,
                        Curve50_fit$Km,
                        Curve50_fit$GammaStar,
                        Curve50_fit$fitmethod,
                        Curve50_fit$Tcorrect,
                        Curve50_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve50_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve50_output) # add the curve fits to the larger data frame

### Curve51_data
Curve51_data <- subset(aci.df, unique_id == aci.df.unique_id[51] & Ci < 1000) # find correct curve from full dataframe and make new object
plot(Curve51_data$A~Curve51_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve51_fit <- fitaci(Curve51_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve51_fit) # take a look at fitted values, adjust as needed
plot(Curve51_fit) # plot the fitted curves over the raw data, adjust as needed
Curve51_output <- cbind('Curve51', Curve51_data$id[1], Curve51_data$unique_id[1], Curve51_data$machine[1], Curve51_data$baseline_yn[1],
                        Curve51_data$A[1], Curve51_data$Ci[1], Curve51_data$gsw[1],
                        mean(Curve51_data$VPDleaf, na.rm = T), mean(Curve51_data$Tleaf, na.rm = T), mean(Curve51_data$Qin, na.rm = T),
                        Curve51_fit[[2]][1,1], Curve51_fit[[2]][1,2],
                        Curve51_fit[[2]][2,1], Curve51_fit[[2]][2,2],
                        Curve51_fit[[2]][3,1], Curve51_fit[[2]][3,2],
                        Curve51_fit$RMSE,
                        Curve51_fit$Ci_transition,
                        Curve51_fit$citransition,
                        Curve51_fit$Km,
                        Curve51_fit$GammaStar,
                        Curve51_fit$fitmethod,
                        Curve51_fit$Tcorrect,
                        Curve51_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve51_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve51_output) # add the curve fits to the larger data frame

### Curve52_data
Curve52_data <- subset(aci.df, unique_id == aci.df.unique_id[52] & Ci < 1000) # find correct curve from full dataframe and make new object
plot(Curve52_data$A~Curve52_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve52_fit <- fitaci(Curve52_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve52_fit) # take a look at fitted values, adjust as needed
plot(Curve52_fit) # plot the fitted curves over the raw data, adjust as needed
Curve52_output <- cbind('Curve52', Curve52_data$id[1], Curve52_data$unique_id[1], Curve52_data$machine[1], Curve52_data$baseline_yn[1],
                        Curve52_data$A[1], Curve52_data$Ci[1], Curve52_data$gsw[1],
                        mean(Curve52_data$VPDleaf, na.rm = T), mean(Curve52_data$Tleaf, na.rm = T), mean(Curve52_data$Qin, na.rm = T),
                        Curve52_fit[[2]][1,1], Curve52_fit[[2]][1,2],
                        Curve52_fit[[2]][2,1], Curve52_fit[[2]][2,2],
                        Curve52_fit[[2]][3,1], Curve52_fit[[2]][3,2],
                        Curve52_fit$RMSE,
                        Curve52_fit$Ci_transition,
                        Curve52_fit$citransition,
                        Curve52_fit$Km,
                        Curve52_fit$GammaStar,
                        Curve52_fit$fitmethod,
                        Curve52_fit$Tcorrect,
                        Curve52_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve52_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve52_output) # add the curve fits to the larger data frame

### Curve53_data
Curve53_data <- subset(aci.df, unique_id == aci.df.unique_id[53]) # find correct curve from full dataframe and make new object
plot(Curve53_data$A~Curve53_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve53_fit <- fitaci(Curve53_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve53_fit) # take a look at fitted values, adjust as needed
plot(Curve53_fit) # plot the fitted curves over the raw data, adjust as needed
Curve53_output <- cbind('Curve53', Curve53_data$id[1], Curve53_data$unique_id[1], Curve53_data$machine[1], Curve53_data$baseline_yn[1],
                        Curve53_data$A[1], Curve53_data$Ci[1], Curve53_data$gsw[1],
                        mean(Curve53_data$VPDleaf, na.rm = T), mean(Curve53_data$Tleaf, na.rm = T), mean(Curve53_data$Qin, na.rm = T),
                        Curve53_fit[[2]][1,1], Curve53_fit[[2]][1,2],
                        Curve53_fit[[2]][2,1], Curve53_fit[[2]][2,2],
                        Curve53_fit[[2]][3,1], Curve53_fit[[2]][3,2],
                        Curve53_fit$RMSE,
                        Curve53_fit$Ci_transition,
                        Curve53_fit$citransition,
                        Curve53_fit$Km,
                        Curve53_fit$GammaStar,
                        Curve53_fit$fitmethod,
                        Curve53_fit$Tcorrect,
                        Curve53_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve53_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve53_output) # add the curve fits to the larger data frame

### Curve54_data
Curve54_data <- subset(aci.df, unique_id == aci.df.unique_id[54]) # find correct curve from full dataframe and make new object
plot(Curve54_data$A~Curve54_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve54_fit <- fitaci(Curve54_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve54_fit) # take a look at fitted values, adjust as needed
plot(Curve54_fit) # plot the fitted curves over the raw data, adjust as needed
Curve54_output <- cbind('Curve54', Curve54_data$id[1], Curve54_data$unique_id[1], Curve54_data$machine[1], Curve54_data$baseline_yn[1],
                        Curve54_data$A[1], Curve54_data$Ci[1], Curve54_data$gsw[1],
                        mean(Curve54_data$VPDleaf, na.rm = T), mean(Curve54_data$Tleaf, na.rm = T), mean(Curve54_data$Qin, na.rm = T),
                        Curve54_fit[[2]][1,1], Curve54_fit[[2]][1,2],
                        Curve54_fit[[2]][2,1], Curve54_fit[[2]][2,2],
                        Curve54_fit[[2]][3,1], Curve54_fit[[2]][3,2],
                        Curve54_fit$RMSE,
                        Curve54_fit$Ci_transition,
                        Curve54_fit$citransition,
                        Curve54_fit$Km,
                        Curve54_fit$GammaStar,
                        Curve54_fit$fitmethod,
                        Curve54_fit$Tcorrect,
                        Curve54_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve54_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve54_output) # add the curve fits to the larger data frame

### Curve55_data
Curve55_data <- subset(aci.df, unique_id == aci.df.unique_id[55]) # find correct curve from full dataframe and make new object
plot(Curve55_data$A~Curve55_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve55_fit <- fitaci(Curve55_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve55_fit) # take a look at fitted values, adjust as needed
plot(Curve55_fit) # plot the fitted curves over the raw data, adjust as needed
Curve55_output <- cbind('Curve55', Curve55_data$id[1], Curve55_data$unique_id[1], Curve55_data$machine[1], Curve55_data$baseline_yn[1],
                        Curve55_data$A[1], Curve55_data$Ci[1], Curve55_data$gsw[1],
                        mean(Curve55_data$VPDleaf, na.rm = T), mean(Curve55_data$Tleaf, na.rm = T), mean(Curve55_data$Qin, na.rm = T),
                        Curve55_fit[[2]][1,1], Curve55_fit[[2]][1,2],
                        Curve55_fit[[2]][2,1], Curve55_fit[[2]][2,2],
                        Curve55_fit[[2]][3,1], Curve55_fit[[2]][3,2],
                        Curve55_fit$RMSE,
                        Curve55_fit$Ci_transition,
                        Curve55_fit$citransition,
                        Curve55_fit$Km,
                        Curve55_fit$GammaStar,
                        Curve55_fit$fitmethod,
                        Curve55_fit$Tcorrect,
                        Curve55_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve55_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve55_output) # add the curve fits to the larger data frame

### Curve56_data
Curve56_data <- subset(aci.df, unique_id == aci.df.unique_id[56]) # find correct curve from full dataframe and make new object
plot(Curve56_data$A~Curve56_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve56_fit <- fitaci(Curve56_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve56_fit) # take a look at fitted values, adjust as needed
plot(Curve56_fit) # plot the fitted curves over the raw data, adjust as needed
Curve56_output <- cbind('Curve56', Curve56_data$id[1], Curve56_data$unique_id[1], Curve56_data$machine[1], Curve56_data$baseline_yn[1],
                        Curve56_data$A[1], Curve56_data$Ci[1], Curve56_data$gsw[1],
                        mean(Curve56_data$VPDleaf, na.rm = T), mean(Curve56_data$Tleaf, na.rm = T), mean(Curve56_data$Qin, na.rm = T),
                        Curve56_fit[[2]][1,1], Curve56_fit[[2]][1,2],
                        Curve56_fit[[2]][2,1], Curve56_fit[[2]][2,2],
                        Curve56_fit[[2]][3,1], Curve56_fit[[2]][3,2],
                        Curve56_fit$RMSE,
                        Curve56_fit$Ci_transition,
                        Curve56_fit$citransition,
                        Curve56_fit$Km,
                        Curve56_fit$GammaStar,
                        Curve56_fit$fitmethod,
                        Curve56_fit$Tcorrect,
                        Curve56_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve56_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve56_output) # add the curve fits to the larger data frame

### Curve57_data
Curve57_data <- subset(aci.df, unique_id == aci.df.unique_id[57]) # find correct curve from full dataframe and make new object
plot(Curve57_data$A~Curve57_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve57_fit <- fitaci(Curve57_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve57_fit) # take a look at fitted values, adjust as needed
plot(Curve57_fit) # plot the fitted curves over the raw data, adjust as needed
Curve57_output <- cbind('Curve57', Curve57_data$id[1], Curve57_data$unique_id[1], Curve57_data$machine[1], Curve57_data$baseline_yn[1],
                        Curve57_data$A[1], Curve57_data$Ci[1], Curve57_data$gsw[1],
                        mean(Curve57_data$VPDleaf, na.rm = T), mean(Curve57_data$Tleaf, na.rm = T), mean(Curve57_data$Qin, na.rm = T),
                        Curve57_fit[[2]][1,1], Curve57_fit[[2]][1,2],
                        Curve57_fit[[2]][2,1], Curve57_fit[[2]][2,2],
                        Curve57_fit[[2]][3,1], Curve57_fit[[2]][3,2],
                        Curve57_fit$RMSE,
                        Curve57_fit$Ci_transition,
                        Curve57_fit$citransition,
                        Curve57_fit$Km,
                        Curve57_fit$GammaStar,
                        Curve57_fit$fitmethod,
                        Curve57_fit$Tcorrect,
                        Curve57_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve57_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve57_output) # add the curve fits to the larger data frame

### Curve58_data
Curve58_data <- subset(aci.df, unique_id == aci.df.unique_id[58]) # find correct curve from full dataframe and make new object
plot(Curve58_data$A~Curve58_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve58_fit <- fitaci(Curve58_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve58_fit) # take a look at fitted values, adjust as needed
plot(Curve58_fit) # plot the fitted curves over the raw data, adjust as needed
Curve58_output <- cbind('Curve58', Curve58_data$id[1], Curve58_data$unique_id[1], Curve58_data$machine[1], Curve58_data$baseline_yn[1],
                        Curve58_data$A[1], Curve58_data$Ci[1], Curve58_data$gsw[1],
                        mean(Curve58_data$VPDleaf, na.rm = T), mean(Curve58_data$Tleaf, na.rm = T), mean(Curve58_data$Qin, na.rm = T),
                        Curve58_fit[[2]][1,1], Curve58_fit[[2]][1,2],
                        Curve58_fit[[2]][2,1], Curve58_fit[[2]][2,2],
                        Curve58_fit[[2]][3,1], Curve58_fit[[2]][3,2],
                        Curve58_fit$RMSE,
                        Curve58_fit$Ci_transition,
                        Curve58_fit$citransition,
                        Curve58_fit$Km,
                        Curve58_fit$GammaStar,
                        Curve58_fit$fitmethod,
                        Curve58_fit$Tcorrect,
                        Curve58_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve58_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve58_output) # add the curve fits to the larger data frame

### Curve59_data
Curve59_data <- subset(aci.df, unique_id == aci.df.unique_id[59]) # find correct curve from full dataframe and make new object
plot(Curve59_data$A~Curve59_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve59_fit <- fitaci(Curve59_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve59_fit) # take a look at fitted values, adjust as needed
plot(Curve59_fit) # plot the fitted curves over the raw data, adjust as needed
Curve59_output <- cbind('Curve59', Curve59_data$id[1], Curve59_data$unique_id[1], Curve59_data$machine[1], Curve59_data$baseline_yn[1],
                        Curve59_data$A[1], Curve59_data$Ci[1], Curve59_data$gsw[1],
                        mean(Curve59_data$VPDleaf, na.rm = T), mean(Curve59_data$Tleaf, na.rm = T), mean(Curve59_data$Qin, na.rm = T),
                        Curve59_fit[[2]][1,1], Curve59_fit[[2]][1,2],
                        Curve59_fit[[2]][2,1], Curve59_fit[[2]][2,2],
                        Curve59_fit[[2]][3,1], Curve59_fit[[2]][3,2],
                        Curve59_fit$RMSE,
                        Curve59_fit$Ci_transition,
                        Curve59_fit$citransition,
                        Curve59_fit$Km,
                        Curve59_fit$GammaStar,
                        Curve59_fit$fitmethod,
                        Curve59_fit$Tcorrect,
                        Curve59_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve59_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve59_output) # add the curve fits to the larger data frame

### Curve60_data
Curve60_data <- subset(aci.df, unique_id == aci.df.unique_id[60]) # find correct curve from full dataframe and make new object
plot(Curve60_data$A~Curve60_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve60_fit <- fitaci(Curve60_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve60_fit) # take a look at fitted values, adjust as needed
plot(Curve60_fit) # plot the fitted curves over the raw data, adjust as needed
Curve60_output <- cbind('Curve60', Curve60_data$id[1], Curve60_data$unique_id[1], Curve60_data$machine[1], Curve60_data$baseline_yn[1],
                        Curve60_data$A[1], Curve60_data$Ci[1], Curve60_data$gsw[1],
                        mean(Curve60_data$VPDleaf, na.rm = T), mean(Curve60_data$Tleaf, na.rm = T), mean(Curve60_data$Qin, na.rm = T),
                        Curve60_fit[[2]][1,1], Curve60_fit[[2]][1,2],
                        Curve60_fit[[2]][2,1], Curve60_fit[[2]][2,2],
                        Curve60_fit[[2]][3,1], Curve60_fit[[2]][3,2],
                        Curve60_fit$RMSE,
                        Curve60_fit$Ci_transition,
                        Curve60_fit$citransition,
                        Curve60_fit$Km,
                        Curve60_fit$GammaStar,
                        Curve60_fit$fitmethod,
                        Curve60_fit$Tcorrect,
                        Curve60_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve60_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve60_output) # add the curve fits to the larger data frame

### Curve61_data
Curve61_data <- subset(aci.df, unique_id == aci.df.unique_id[61]) # find correct curve from full dataframe and make new object
plot(Curve61_data$A~Curve61_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve61_fit <- fitaci(Curve61_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve61_fit) # take a look at fitted values, adjust as needed
plot(Curve61_fit) # plot the fitted curves over the raw data, adjust as needed
Curve61_output <- cbind('Curve61', Curve61_data$id[1], Curve61_data$unique_id[1], Curve61_data$machine[1], Curve61_data$baseline_yn[1],
                        Curve61_data$A[1], Curve61_data$Ci[1], Curve61_data$gsw[1],
                        mean(Curve61_data$VPDleaf, na.rm = T), mean(Curve61_data$Tleaf, na.rm = T), mean(Curve61_data$Qin, na.rm = T),
                        Curve61_fit[[2]][1,1], Curve61_fit[[2]][1,2],
                        Curve61_fit[[2]][2,1], Curve61_fit[[2]][2,2],
                        Curve61_fit[[2]][3,1], Curve61_fit[[2]][3,2],
                        Curve61_fit$RMSE,
                        Curve61_fit$Ci_transition,
                        Curve61_fit$citransition,
                        Curve61_fit$Km,
                        Curve61_fit$GammaStar,
                        Curve61_fit$fitmethod,
                        Curve61_fit$Tcorrect,
                        Curve61_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve61_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve61_output) # add the curve fits to the larger data frame

### Curve62_data
Curve62_data <- subset(aci.df, unique_id == aci.df.unique_id[62]) # find correct curve from full dataframe and make new object
print(Curve62_data)
plot(Curve62_data$A~Curve62_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve62_fit <- fitaci(Curve62_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve62_fit) # take a look at fitted values, adjust as needed
plot(Curve62_fit) # plot the fitted curves over the raw data, adjust as needed
Curve62_output <- cbind('Curve62', Curve62_data$id[1], Curve62_data$unique_id[1], Curve62_data$machine[1], Curve62_data$baseline_yn[1],
                        Curve62_data$A[1], Curve62_data$Ci[1], Curve62_data$gsw[1],
                        mean(Curve62_data$VPDleaf, na.rm = T), mean(Curve62_data$Tleaf, na.rm = T), mean(Curve62_data$Qin, na.rm = T),
                        Curve62_fit[[2]][1,1], Curve62_fit[[2]][1,2],
                        Curve62_fit[[2]][2,1], Curve62_fit[[2]][2,2],
                        Curve62_fit[[2]][3,1], Curve62_fit[[2]][3,2],
                        Curve62_fit$RMSE,
                        Curve62_fit$Ci_transition,
                        Curve62_fit$citransition,
                        Curve62_fit$Km,
                        Curve62_fit$GammaStar,
                        Curve62_fit$fitmethod,
                        Curve62_fit$Tcorrect,
                        Curve62_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve62_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve62_output) # add the curve fits to the larger data frame

### Curve63_data
Curve63_data <- subset(aci.df, unique_id == aci.df.unique_id[63]) # find correct curve from full dataframe and make new object
plot(Curve63_data$A~Curve63_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve63_fit <- fitaci(Curve63_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve63_fit) # take a look at fitted values, adjust as needed
plot(Curve63_fit) # plot the fitted curves over the raw data, adjust as needed
Curve63_output <- cbind('Curve63', Curve63_data$id[1], Curve63_data$unique_id[1], Curve63_data$machine[1], Curve63_data$baseline_yn[1],
                        Curve63_data$A[1], Curve63_data$Ci[1], Curve63_data$gsw[1],
                        mean(Curve63_data$VPDleaf, na.rm = T), mean(Curve63_data$Tleaf, na.rm = T), mean(Curve63_data$Qin, na.rm = T),
                        Curve63_fit[[2]][1,1], Curve63_fit[[2]][1,2],
                        Curve63_fit[[2]][2,1], Curve63_fit[[2]][2,2],
                        Curve63_fit[[2]][3,1], Curve63_fit[[2]][3,2],
                        Curve63_fit$RMSE,
                        Curve63_fit$Ci_transition,
                        Curve63_fit$citransition,
                        Curve63_fit$Km,
                        Curve63_fit$GammaStar,
                        Curve63_fit$fitmethod,
                        Curve63_fit$Tcorrect,
                        Curve63_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve63_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve63_output) # add the curve fits to the larger data frame

### Curve64_data
Curve64_data <- subset(aci.df, unique_id == aci.df.unique_id[64]) # find correct curve from full dataframe and make new object
plot(Curve64_data$A~Curve64_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve64_fit <- fitaci(Curve64_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve64_fit) # take a look at fitted values, adjust as needed
plot(Curve64_fit) # plot the fitted curves over the raw data, adjust as needed
Curve64_output <- cbind('Curve64', Curve64_data$id[1], Curve64_data$unique_id[1], Curve64_data$machine[1], Curve64_data$baseline_yn[1],
                        Curve64_data$A[1], Curve64_data$Ci[1], Curve64_data$gsw[1],
                        mean(Curve64_data$VPDleaf, na.rm = T), mean(Curve64_data$Tleaf, na.rm = T), mean(Curve64_data$Qin, na.rm = T),
                        Curve64_fit[[2]][1,1], Curve64_fit[[2]][1,2],
                        Curve64_fit[[2]][2,1], Curve64_fit[[2]][2,2],
                        Curve64_fit[[2]][3,1], Curve64_fit[[2]][3,2],
                        Curve64_fit$RMSE,
                        Curve64_fit$Ci_transition,
                        Curve64_fit$citransition,
                        Curve64_fit$Km,
                        Curve64_fit$GammaStar,
                        Curve64_fit$fitmethod,
                        Curve64_fit$Tcorrect,
                        Curve64_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve64_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve64_output) # add the curve fits to the larger data frame

### Curve65_data
Curve65_data <- subset(aci.df, unique_id == aci.df.unique_id[65]) # find correct curve from full dataframe and make new object
plot(Curve65_data$A~Curve65_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve65_fit <- fitaci(Curve65_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve65_fit) # take a look at fitted values, adjust as needed
plot(Curve65_fit) # plot the fitted curves over the raw data, adjust as needed
Curve65_output <- cbind('Curve65', Curve65_data$id[1], Curve65_data$unique_id[1], Curve65_data$machine[1], Curve65_data$baseline_yn[1],
                        Curve65_data$A[1], Curve65_data$Ci[1], Curve65_data$gsw[1],
                        mean(Curve65_data$VPDleaf, na.rm = T), mean(Curve65_data$Tleaf, na.rm = T), mean(Curve65_data$Qin, na.rm = T),
                        Curve65_fit[[2]][1,1], Curve65_fit[[2]][1,2],
                        Curve65_fit[[2]][2,1], Curve65_fit[[2]][2,2],
                        Curve65_fit[[2]][3,1], Curve65_fit[[2]][3,2],
                        Curve65_fit$RMSE,
                        Curve65_fit$Ci_transition,
                        Curve65_fit$citransition,
                        Curve65_fit$Km,
                        Curve65_fit$GammaStar,
                        Curve65_fit$fitmethod,
                        Curve65_fit$Tcorrect,
                        Curve65_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve65_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve65_output) # add the curve fits to the larger data frame

### Curve66_data
Curve66_data <- subset(aci.df, unique_id == aci.df.unique_id[66]) # find correct curve from full dataframe and make new object
plot(Curve66_data$A~Curve66_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve66_fit <- fitaci(Curve66_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve66_fit) # take a look at fitted values, adjust as needed
plot(Curve66_fit) # plot the fitted curves over the raw data, adjust as needed
Curve66_output <- cbind('Curve66', Curve66_data$id[1], Curve66_data$unique_id[1], Curve66_data$machine[1], Curve66_data$baseline_yn[1],
                        Curve66_data$A[1], Curve66_data$Ci[1], Curve66_data$gsw[1],
                        mean(Curve66_data$VPDleaf, na.rm = T), mean(Curve66_data$Tleaf, na.rm = T), mean(Curve66_data$Qin, na.rm = T),
                        Curve66_fit[[2]][1,1], Curve66_fit[[2]][1,2],
                        Curve66_fit[[2]][2,1], Curve66_fit[[2]][2,2],
                        Curve66_fit[[2]][3,1], Curve66_fit[[2]][3,2],
                        Curve66_fit$RMSE,
                        Curve66_fit$Ci_transition,
                        Curve66_fit$citransition,
                        Curve66_fit$Km,
                        Curve66_fit$GammaStar,
                        Curve66_fit$fitmethod,
                        Curve66_fit$Tcorrect,
                        Curve66_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve66_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve66_output) # add the curve fits to the larger data frame

### Curve67_data
Curve67_data <- subset(aci.df, unique_id == aci.df.unique_id[67]) # find correct curve from full dataframe and make new object
plot(Curve67_data$A~Curve67_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve67_fit <- fitaci(Curve67_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve67_fit) # take a look at fitted values, adjust as needed
plot(Curve67_fit) # plot the fitted curves over the raw data, adjust as needed
Curve67_output <- cbind('Curve67', Curve67_data$id[1], Curve67_data$unique_id[1], Curve67_data$machine[1], Curve67_data$baseline_yn[1],
                        Curve67_data$A[1], Curve67_data$Ci[1], Curve67_data$gsw[1],
                        mean(Curve67_data$VPDleaf, na.rm = T), mean(Curve67_data$Tleaf, na.rm = T), mean(Curve67_data$Qin, na.rm = T),
                        Curve67_fit[[2]][1,1], Curve67_fit[[2]][1,2],
                        Curve67_fit[[2]][2,1], Curve67_fit[[2]][2,2],
                        Curve67_fit[[2]][3,1], Curve67_fit[[2]][3,2],
                        Curve67_fit$RMSE,
                        Curve67_fit$Ci_transition,
                        Curve67_fit$citransition,
                        Curve67_fit$Km,
                        Curve67_fit$GammaStar,
                        Curve67_fit$fitmethod,
                        Curve67_fit$Tcorrect,
                        Curve67_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve67_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve67_output) # add the curve fits to the larger data frame

### Curve68_data
Curve68_data <- subset(aci.df, unique_id == aci.df.unique_id[68] & Ci < 1000) # find correct curve from full dataframe and make new object
plot(Curve68_data$A~Curve68_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve68_fit <- fitaci(Curve68_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve68_fit) # take a look at fitted values, adjust as needed
plot(Curve68_fit) # plot the fitted curves over the raw data, adjust as needed
Curve68_output <- cbind('Curve68', Curve68_data$id[1], Curve68_data$unique_id[1], Curve68_data$machine[1], Curve68_data$baseline_yn[1],
                        Curve68_data$A[1], Curve68_data$Ci[1], Curve68_data$gsw[1],
                        mean(Curve68_data$VPDleaf, na.rm = T), mean(Curve68_data$Tleaf, na.rm = T), mean(Curve68_data$Qin, na.rm = T),
                        Curve68_fit[[2]][1,1], Curve68_fit[[2]][1,2],
                        Curve68_fit[[2]][2,1], Curve68_fit[[2]][2,2],
                        Curve68_fit[[2]][3,1], Curve68_fit[[2]][3,2],
                        Curve68_fit$RMSE,
                        Curve68_fit$Ci_transition,
                        Curve68_fit$citransition,
                        Curve68_fit$Km,
                        Curve68_fit$GammaStar,
                        Curve68_fit$fitmethod,
                        Curve68_fit$Tcorrect,
                        Curve68_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve68_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve68_output) # add the curve fits to the larger data frame

### Curve69_data
Curve69_data <- subset(aci.df, unique_id == aci.df.unique_id[69]) # find correct curve from full dataframe and make new object
plot(Curve69_data$A~Curve69_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve69_fit <- fitaci(Curve69_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve69_fit) # take a look at fitted values, adjust as needed
plot(Curve69_fit) # plot the fitted curves over the raw data, adjust as needed
Curve69_output <- cbind('Curve69', Curve69_data$id[1], Curve69_data$unique_id[1], Curve69_data$machine[1], Curve69_data$baseline_yn[1],
                        Curve69_data$A[1], Curve69_data$Ci[1], Curve69_data$gsw[1],
                        mean(Curve69_data$VPDleaf, na.rm = T), mean(Curve69_data$Tleaf, na.rm = T), mean(Curve69_data$Qin, na.rm = T),
                        Curve69_fit[[2]][1,1], Curve69_fit[[2]][1,2],
                        Curve69_fit[[2]][2,1], Curve69_fit[[2]][2,2],
                        Curve69_fit[[2]][3,1], Curve69_fit[[2]][3,2],
                        Curve69_fit$RMSE,
                        Curve69_fit$Ci_transition,
                        Curve69_fit$citransition,
                        Curve69_fit$Km,
                        Curve69_fit$GammaStar,
                        Curve69_fit$fitmethod,
                        Curve69_fit$Tcorrect,
                        Curve69_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve69_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve69_output) # add the curve fits to the larger data frame

### Curve70_data
Curve70_data <- subset(aci.df, unique_id == aci.df.unique_id[70]) # find correct curve from full dataframe and make new object
plot(Curve70_data$A~Curve70_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve70_fit <- fitaci(Curve70_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve70_fit) # take a look at fitted values, adjust as needed
plot(Curve70_fit) # plot the fitted curves over the raw data, adjust as needed
Curve70_output <- cbind('Curve70', Curve70_data$id[1], Curve70_data$unique_id[1], Curve70_data$machine[1], Curve70_data$baseline_yn[1],
                        Curve70_data$A[1], Curve70_data$Ci[1], Curve70_data$gsw[1],
                        mean(Curve70_data$VPDleaf, na.rm = T), mean(Curve70_data$Tleaf, na.rm = T), mean(Curve70_data$Qin, na.rm = T),
                        Curve70_fit[[2]][1,1], Curve70_fit[[2]][1,2],
                        Curve70_fit[[2]][2,1], Curve70_fit[[2]][2,2],
                        Curve70_fit[[2]][3,1], Curve70_fit[[2]][3,2],
                        Curve70_fit$RMSE,
                        Curve70_fit$Ci_transition,
                        Curve70_fit$citransition,
                        Curve70_fit$Km,
                        Curve70_fit$GammaStar,
                        Curve70_fit$fitmethod,
                        Curve70_fit$Tcorrect,
                        Curve70_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve70_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve70_output) # add the curve fits to the larger data frame

### Curve71_data
Curve71_data <- subset(aci.df, unique_id == aci.df.unique_id[71]) # find correct curve from full dataframe and make new object
plot(Curve71_data$A~Curve71_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve71_fit <- fitaci(Curve71_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve71_fit) # take a look at fitted values, adjust as needed
plot(Curve71_fit) # plot the fitted curves over the raw data, adjust as needed
Curve71_output <- cbind('Curve71', Curve71_data$id[1], Curve71_data$unique_id[1], Curve71_data$machine[1], Curve71_data$baseline_yn[1],
                        Curve71_data$A[1], Curve71_data$Ci[1], Curve71_data$gsw[1],
                        mean(Curve71_data$VPDleaf, na.rm = T), mean(Curve71_data$Tleaf, na.rm = T), mean(Curve71_data$Qin, na.rm = T),
                        Curve71_fit[[2]][1,1], Curve71_fit[[2]][1,2],
                        Curve71_fit[[2]][2,1], Curve71_fit[[2]][2,2],
                        Curve71_fit[[2]][3,1], Curve71_fit[[2]][3,2],
                        Curve71_fit$RMSE,
                        Curve71_fit$Ci_transition,
                        Curve71_fit$citransition,
                        Curve71_fit$Km,
                        Curve71_fit$GammaStar,
                        Curve71_fit$fitmethod,
                        Curve71_fit$Tcorrect,
                        Curve71_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve71_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve71_output) # add the curve fits to the larger data frame

### Curve72_data
Curve72_data <- subset(aci.df, unique_id == aci.df.unique_id[72]) # find correct curve from full dataframe and make new object
plot(Curve72_data$A~Curve72_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve72_fit <- fitaci(Curve72_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve72_fit) # take a look at fitted values, adjust as needed
plot(Curve72_fit) # plot the fitted curves over the raw data, adjust as needed
Curve72_output <- cbind('Curve72', Curve72_data$id[1], Curve72_data$unique_id[1], Curve72_data$machine[1], Curve72_data$baseline_yn[1],
                        Curve72_data$A[1], Curve72_data$Ci[1], Curve72_data$gsw[1],
                        mean(Curve72_data$VPDleaf, na.rm = T), mean(Curve72_data$Tleaf, na.rm = T), mean(Curve72_data$Qin, na.rm = T),
                        Curve72_fit[[2]][1,1], Curve72_fit[[2]][1,2],
                        Curve72_fit[[2]][2,1], Curve72_fit[[2]][2,2],
                        Curve72_fit[[2]][3,1], Curve72_fit[[2]][3,2],
                        Curve72_fit$RMSE,
                        Curve72_fit$Ci_transition,
                        Curve72_fit$citransition,
                        Curve72_fit$Km,
                        Curve72_fit$GammaStar,
                        Curve72_fit$fitmethod,
                        Curve72_fit$Tcorrect,
                        Curve72_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve72_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve72_output) # add the curve fits to the larger data frame

### Curve73_data
Curve73_data <- subset(aci.df, unique_id == aci.df.unique_id[73]) # find correct curve from full dataframe and make new object
plot(Curve73_data$A~Curve73_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve73_fit <- fitaci(Curve73_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve73_fit) # take a look at fitted values, adjust as needed
plot(Curve73_fit) # plot the fitted curves over the raw data, adjust as needed
Curve73_output <- cbind('Curve73', Curve73_data$id[1], Curve73_data$unique_id[1], Curve73_data$machine[1], Curve73_data$baseline_yn[1],
                        Curve73_data$A[1], Curve73_data$Ci[1], Curve73_data$gsw[1],
                        mean(Curve73_data$VPDleaf, na.rm = T), mean(Curve73_data$Tleaf, na.rm = T), mean(Curve73_data$Qin, na.rm = T),
                        Curve73_fit[[2]][1,1], Curve73_fit[[2]][1,2],
                        Curve73_fit[[2]][2,1], Curve73_fit[[2]][2,2],
                        Curve73_fit[[2]][3,1], Curve73_fit[[2]][3,2],
                        Curve73_fit$RMSE,
                        Curve73_fit$Ci_transition,
                        Curve73_fit$citransition,
                        Curve73_fit$Km,
                        Curve73_fit$GammaStar,
                        Curve73_fit$fitmethod,
                        Curve73_fit$Tcorrect,
                        Curve73_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve73_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve73_output) # add the curve fits to the larger data frame

### Curve74_data
Curve74_data <- subset(aci.df, unique_id == aci.df.unique_id[74] & Ci < 1000) # find correct curve from full dataframe and make new object
plot(Curve74_data$A~Curve74_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve74_fit <- fitaci(Curve74_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve74_fit) # take a look at fitted values, adjust as needed
plot(Curve74_fit) # plot the fitted curves over the raw data, adjust as needed
Curve74_output <- cbind('Curve74', Curve74_data$id[1], Curve74_data$unique_id[1], Curve74_data$machine[1], Curve74_data$baseline_yn[1],
                        Curve74_data$A[1], Curve74_data$Ci[1], Curve74_data$gsw[1],
                        mean(Curve74_data$VPDleaf, na.rm = T), mean(Curve74_data$Tleaf, na.rm = T), mean(Curve74_data$Qin, na.rm = T),
                        Curve74_fit[[2]][1,1], Curve74_fit[[2]][1,2],
                        Curve74_fit[[2]][2,1], Curve74_fit[[2]][2,2],
                        Curve74_fit[[2]][3,1], Curve74_fit[[2]][3,2],
                        Curve74_fit$RMSE,
                        Curve74_fit$Ci_transition,
                        Curve74_fit$citransition,
                        Curve74_fit$Km,
                        Curve74_fit$GammaStar,
                        Curve74_fit$fitmethod,
                        Curve74_fit$Tcorrect,
                        Curve74_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve74_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve74_output) # add the curve fits to the larger data frame

### Curve75_data
Curve75_data <- subset(aci.df, unique_id == aci.df.unique_id[75] & Ci < 1000) # find correct curve from full dataframe and make new object
plot(Curve75_data$A~Curve75_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve75_fit <- fitaci(Curve75_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve75_fit) # take a look at fitted values, adjust as needed
plot(Curve75_fit) # plot the fitted curves over the raw data, adjust as needed
Curve75_output <- cbind('Curve75', Curve75_data$id[1], Curve75_data$unique_id[1], Curve75_data$machine[1], Curve75_data$baseline_yn[1],
                        Curve75_data$A[1], Curve75_data$Ci[1], Curve75_data$gsw[1],
                        mean(Curve75_data$VPDleaf, na.rm = T), mean(Curve75_data$Tleaf, na.rm = T), mean(Curve75_data$Qin, na.rm = T),
                        Curve75_fit[[2]][1,1], Curve75_fit[[2]][1,2],
                        Curve75_fit[[2]][2,1], Curve75_fit[[2]][2,2],
                        Curve75_fit[[2]][3,1], Curve75_fit[[2]][3,2],
                        Curve75_fit$RMSE,
                        Curve75_fit$Ci_transition,
                        Curve75_fit$citransition,
                        Curve75_fit$Km,
                        Curve75_fit$GammaStar,
                        Curve75_fit$fitmethod,
                        Curve75_fit$Tcorrect,
                        Curve75_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve75_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve75_output) # add the curve fits to the larger data frame

### Curve76_data
Curve76_data <- subset(aci.df, unique_id == aci.df.unique_id[76]) # find correct curve from full dataframe and make new object
plot(Curve76_data$A~Curve76_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve76_fit <- fitaci(Curve76_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve76_fit) # take a look at fitted values, adjust as needed
plot(Curve76_fit) # plot the fitted curves over the raw data, adjust as needed
Curve76_output <- cbind('Curve76', Curve76_data$id[1], Curve76_data$unique_id[1], Curve76_data$machine[1], Curve76_data$baseline_yn[1],
                        Curve76_data$A[1], Curve76_data$Ci[1], Curve76_data$gsw[1],
                        mean(Curve76_data$VPDleaf, na.rm = T), mean(Curve76_data$Tleaf, na.rm = T), mean(Curve76_data$Qin, na.rm = T),
                        Curve76_fit[[2]][1,1], Curve76_fit[[2]][1,2],
                        Curve76_fit[[2]][2,1], Curve76_fit[[2]][2,2],
                        Curve76_fit[[2]][3,1], Curve76_fit[[2]][3,2],
                        Curve76_fit$RMSE,
                        Curve76_fit$Ci_transition,
                        Curve76_fit$citransition,
                        Curve76_fit$Km,
                        Curve76_fit$GammaStar,
                        Curve76_fit$fitmethod,
                        Curve76_fit$Tcorrect,
                        Curve76_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve76_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve76_output) # add the curve fits to the larger data frame

### Curve77_data
Curve77_data <- subset(aci.df, unique_id == aci.df.unique_id[77]) # find correct curve from full dataframe and make new object
plot(Curve77_data$A~Curve77_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve77_fit <- fitaci(Curve77_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve77_fit) # take a look at fitted values, adjust as needed
plot(Curve77_fit) # plot the fitted curves over the raw data, adjust as needed
Curve77_output <- cbind('Curve77', Curve77_data$id[1], Curve77_data$unique_id[1], Curve77_data$machine[1], Curve77_data$baseline_yn[1],
                        Curve77_data$A[1], Curve77_data$Ci[1], Curve77_data$gsw[1],
                        mean(Curve77_data$VPDleaf, na.rm = T), mean(Curve77_data$Tleaf, na.rm = T), mean(Curve77_data$Qin, na.rm = T),
                        Curve77_fit[[2]][1,1], Curve77_fit[[2]][1,2],
                        Curve77_fit[[2]][2,1], Curve77_fit[[2]][2,2],
                        Curve77_fit[[2]][3,1], Curve77_fit[[2]][3,2],
                        Curve77_fit$RMSE,
                        Curve77_fit$Ci_transition,
                        Curve77_fit$citransition,
                        Curve77_fit$Km,
                        Curve77_fit$GammaStar,
                        Curve77_fit$fitmethod,
                        Curve77_fit$Tcorrect,
                        Curve77_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve77_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve77_output) # add the curve fits to the larger data frame

### Curve78_data
Curve78_data <- subset(aci.df, unique_id == aci.df.unique_id[78]) # find correct curve from full dataframe and make new object
plot(Curve78_data$A~Curve78_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve78_fit <- fitaci(Curve78_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve78_fit) # take a look at fitted values, adjust as needed
plot(Curve78_fit) # plot the fitted curves over the raw data, adjust as needed
Curve78_output <- cbind('Curve78', Curve78_data$id[1], Curve78_data$unique_id[1], Curve78_data$machine[1], Curve78_data$baseline_yn[1],
                        Curve78_data$A[1], Curve78_data$Ci[1], Curve78_data$gsw[1],
                        mean(Curve78_data$VPDleaf, na.rm = T), mean(Curve78_data$Tleaf, na.rm = T), mean(Curve78_data$Qin, na.rm = T),
                        Curve78_fit[[2]][1,1], Curve78_fit[[2]][1,2],
                        Curve78_fit[[2]][2,1], Curve78_fit[[2]][2,2],
                        Curve78_fit[[2]][3,1], Curve78_fit[[2]][3,2],
                        Curve78_fit$RMSE,
                        Curve78_fit$Ci_transition,
                        Curve78_fit$citransition,
                        Curve78_fit$Km,
                        Curve78_fit$GammaStar,
                        Curve78_fit$fitmethod,
                        Curve78_fit$Tcorrect,
                        Curve78_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve78_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve78_output) # add the curve fits to the larger data frame

### Curve79_data
Curve79_data <- subset(aci.df, unique_id == aci.df.unique_id[79]) # find correct curve from full dataframe and make new object
plot(Curve79_data$A~Curve79_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve79_fit <- fitaci(Curve79_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve79_fit) # take a look at fitted values, adjust as needed
plot(Curve79_fit) # plot the fitted curves over the raw data, adjust as needed
Curve79_output <- cbind('Curve79', Curve79_data$id[1], Curve79_data$unique_id[1], Curve79_data$machine[1], Curve79_data$baseline_yn[1],
                        Curve79_data$A[1], Curve79_data$Ci[1], Curve79_data$gsw[1],
                        mean(Curve79_data$VPDleaf, na.rm = T), mean(Curve79_data$Tleaf, na.rm = T), mean(Curve79_data$Qin, na.rm = T),
                        Curve79_fit[[2]][1,1], Curve79_fit[[2]][1,2],
                        Curve79_fit[[2]][2,1], Curve79_fit[[2]][2,2],
                        Curve79_fit[[2]][3,1], Curve79_fit[[2]][3,2],
                        Curve79_fit$RMSE,
                        Curve79_fit$Ci_transition,
                        Curve79_fit$citransition,
                        Curve79_fit$Km,
                        Curve79_fit$GammaStar,
                        Curve79_fit$fitmethod,
                        Curve79_fit$Tcorrect,
                        Curve79_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve79_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve79_output) # add the curve fits to the larger data frame

### Curve80_data
Curve80_data <- subset(aci.df, unique_id == aci.df.unique_id[80]) # find correct curve from full dataframe and make new object
plot(Curve80_data$A~Curve80_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve80_fit <- fitaci(Curve80_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve80_fit) # take a look at fitted values, adjust as needed
plot(Curve80_fit) # plot the fitted curves over the raw data, adjust as needed
Curve80_output <- cbind('Curve80', Curve80_data$id[1], Curve80_data$unique_id[1], Curve80_data$machine[1], Curve80_data$baseline_yn[1],
                        Curve80_data$A[1], Curve80_data$Ci[1], Curve80_data$gsw[1],
                        mean(Curve80_data$VPDleaf, na.rm = T), mean(Curve80_data$Tleaf, na.rm = T), mean(Curve80_data$Qin, na.rm = T),
                        Curve80_fit[[2]][1,1], Curve80_fit[[2]][1,2],
                        Curve80_fit[[2]][2,1], Curve80_fit[[2]][2,2],
                        Curve80_fit[[2]][3,1], Curve80_fit[[2]][3,2],
                        Curve80_fit$RMSE,
                        Curve80_fit$Ci_transition,
                        Curve80_fit$citransition,
                        Curve80_fit$Km,
                        Curve80_fit$GammaStar,
                        Curve80_fit$fitmethod,
                        Curve80_fit$Tcorrect,
                        Curve80_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve80_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve80_output) # add the curve fits to the larger data frame

### Curve81_data
Curve81_data <- subset(aci.df, unique_id == aci.df.unique_id[81]) # find correct curve from full dataframe and make new object
plot(Curve81_data$A~Curve81_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve81_fit <- fitaci(Curve81_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve81_fit) # take a look at fitted values, adjust as needed
plot(Curve81_fit) # plot the fitted curves over the raw data, adjust as needed
Curve81_output <- cbind('Curve81', Curve81_data$id[1], Curve81_data$unique_id[1], Curve81_data$machine[1], Curve81_data$baseline_yn[1],
                        Curve81_data$A[1], Curve81_data$Ci[1], Curve81_data$gsw[1],
                        mean(Curve81_data$VPDleaf, na.rm = T), mean(Curve81_data$Tleaf, na.rm = T), mean(Curve81_data$Qin, na.rm = T),
                        Curve81_fit[[2]][1,1], Curve81_fit[[2]][1,2],
                        Curve81_fit[[2]][2,1], Curve81_fit[[2]][2,2],
                        Curve81_fit[[2]][3,1], Curve81_fit[[2]][3,2],
                        Curve81_fit$RMSE,
                        Curve81_fit$Ci_transition,
                        Curve81_fit$citransition,
                        Curve81_fit$Km,
                        Curve81_fit$GammaStar,
                        Curve81_fit$fitmethod,
                        Curve81_fit$Tcorrect,
                        Curve81_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve81_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve81_output) # add the curve fits to the larger data frame

### Curve82_data
Curve82_data <- subset(aci.df, unique_id == aci.df.unique_id[82]) # find correct curve from full dataframe and make new object
plot(Curve82_data$A~Curve82_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve82_fit <- fitaci(Curve82_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve82_fit) # take a look at fitted values, adjust as needed
plot(Curve82_fit) # plot the fitted curves over the raw data, adjust as needed
Curve82_output <- cbind('Curve82', Curve82_data$id[1], Curve82_data$unique_id[1], Curve82_data$machine[1], Curve82_data$baseline_yn[1],
                        Curve82_data$A[1], Curve82_data$Ci[1], Curve82_data$gsw[1],
                        mean(Curve82_data$VPDleaf, na.rm = T), mean(Curve82_data$Tleaf, na.rm = T), mean(Curve82_data$Qin, na.rm = T),
                        Curve82_fit[[2]][1,1], Curve82_fit[[2]][1,2],
                        Curve82_fit[[2]][2,1], Curve82_fit[[2]][2,2],
                        Curve82_fit[[2]][3,1], Curve82_fit[[2]][3,2],
                        Curve82_fit$RMSE,
                        Curve82_fit$Ci_transition,
                        Curve82_fit$citransition,
                        Curve82_fit$Km,
                        Curve82_fit$GammaStar,
                        Curve82_fit$fitmethod,
                        Curve82_fit$Tcorrect,
                        Curve82_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve82_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve82_output) # add the curve fits to the larger data frame

### Curve83_data
Curve83_data <- subset(aci.df, unique_id == aci.df.unique_id[83]) # find correct curve from full dataframe and make new object
plot(Curve83_data$A~Curve83_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve83_fit <- fitaci(Curve83_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve83_fit) # take a look at fitted values, adjust as needed
plot(Curve83_fit) # plot the fitted curves over the raw data, adjust as needed
Curve83_output <- cbind('Curve83', Curve83_data$id[1], Curve83_data$unique_id[1], Curve83_data$machine[1], Curve83_data$baseline_yn[1],
                        Curve83_data$A[1], Curve83_data$Ci[1], Curve83_data$gsw[1],
                        mean(Curve83_data$VPDleaf, na.rm = T), mean(Curve83_data$Tleaf, na.rm = T), mean(Curve83_data$Qin, na.rm = T),
                        Curve83_fit[[2]][1,1], Curve83_fit[[2]][1,2],
                        Curve83_fit[[2]][2,1], Curve83_fit[[2]][2,2],
                        Curve83_fit[[2]][3,1], Curve83_fit[[2]][3,2],
                        Curve83_fit$RMSE,
                        Curve83_fit$Ci_transition,
                        Curve83_fit$citransition,
                        Curve83_fit$Km,
                        Curve83_fit$GammaStar,
                        Curve83_fit$fitmethod,
                        Curve83_fit$Tcorrect,
                        Curve83_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve83_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve83_output) # add the curve fits to the larger data frame

### Curve84_data
Curve84_data <- subset(aci.df, unique_id == aci.df.unique_id[84]) # find correct curve from full dataframe and make new object
plot(Curve84_data$A~Curve84_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve84_fit <- fitaci(Curve84_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve84_fit) # take a look at fitted values, adjust as needed
plot(Curve84_fit) # plot the fitted curves over the raw data, adjust as needed
Curve84_output <- cbind('Curve84', Curve84_data$id[1], Curve84_data$unique_id[1], Curve84_data$machine[1], Curve84_data$baseline_yn[1],
                        Curve84_data$A[1], Curve84_data$Ci[1], Curve84_data$gsw[1],
                        mean(Curve84_data$VPDleaf, na.rm = T), mean(Curve84_data$Tleaf, na.rm = T), mean(Curve84_data$Qin, na.rm = T),
                        Curve84_fit[[2]][1,1], Curve84_fit[[2]][1,2],
                        Curve84_fit[[2]][2,1], Curve84_fit[[2]][2,2],
                        Curve84_fit[[2]][3,1], Curve84_fit[[2]][3,2],
                        Curve84_fit$RMSE,
                        Curve84_fit$Ci_transition,
                        Curve84_fit$citransition,
                        Curve84_fit$Km,
                        Curve84_fit$GammaStar,
                        Curve84_fit$fitmethod,
                        Curve84_fit$Tcorrect,
                        Curve84_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve84_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve84_output) # add the curve fits to the larger data frame

### Curve85_data
Curve85_data <- subset(aci.df, unique_id == aci.df.unique_id[85]) # find correct curve from full dataframe and make new object
plot(Curve85_data$A~Curve85_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve85_fit <- fitaci(Curve85_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve85_fit) # take a look at fitted values, adjust as needed
plot(Curve85_fit) # plot the fitted curves over the raw data, adjust as needed
Curve85_output <- cbind('Curve85', Curve85_data$id[1], Curve85_data$unique_id[1], Curve85_data$machine[1], Curve85_data$baseline_yn[1],
                        Curve85_data$A[1], Curve85_data$Ci[1], Curve85_data$gsw[1],
                        mean(Curve85_data$VPDleaf, na.rm = T), mean(Curve85_data$Tleaf, na.rm = T), mean(Curve85_data$Qin, na.rm = T),
                        Curve85_fit[[2]][1,1], Curve85_fit[[2]][1,2],
                        Curve85_fit[[2]][2,1], Curve85_fit[[2]][2,2],
                        Curve85_fit[[2]][3,1], Curve85_fit[[2]][3,2],
                        Curve85_fit$RMSE,
                        Curve85_fit$Ci_transition,
                        Curve85_fit$citransition,
                        Curve85_fit$Km,
                        Curve85_fit$GammaStar,
                        Curve85_fit$fitmethod,
                        Curve85_fit$Tcorrect,
                        Curve85_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve85_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve85_output) # add the curve fits to the larger data frame

### Curve86_data
Curve86_data <- subset(aci.df, unique_id == aci.df.unique_id[86]) # find correct curve from full dataframe and make new object
plot(Curve86_data$A~Curve86_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve86_fit <- fitaci(Curve86_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve86_fit) # take a look at fitted values, adjust as needed
plot(Curve86_fit) # plot the fitted curves over the raw data, adjust as needed
Curve86_output <- cbind('Curve86', Curve86_data$id[1], Curve86_data$unique_id[1], Curve86_data$machine[1], Curve86_data$baseline_yn[1],
                        Curve86_data$A[1], Curve86_data$Ci[1], Curve86_data$gsw[1],
                        mean(Curve86_data$VPDleaf, na.rm = T), mean(Curve86_data$Tleaf, na.rm = T), mean(Curve86_data$Qin, na.rm = T),
                        Curve86_fit[[2]][1,1], Curve86_fit[[2]][1,2],
                        Curve86_fit[[2]][2,1], Curve86_fit[[2]][2,2],
                        Curve86_fit[[2]][3,1], Curve86_fit[[2]][3,2],
                        Curve86_fit$RMSE,
                        Curve86_fit$Ci_transition,
                        Curve86_fit$citransition,
                        Curve86_fit$Km,
                        Curve86_fit$GammaStar,
                        Curve86_fit$fitmethod,
                        Curve86_fit$Tcorrect,
                        Curve86_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve86_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve86_output) # add the curve fits to the larger data frame

### Curve87_data
Curve87_data <- subset(aci.df, unique_id == aci.df.unique_id[87]) # find correct curve from full dataframe and make new object
plot(Curve87_data$A~Curve87_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve87_fit <- fitaci(Curve87_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve87_fit) # take a look at fitted values, adjust as needed
plot(Curve87_fit) # plot the fitted curves over the raw data, adjust as needed
Curve87_output <- cbind('Curve87', Curve87_data$id[1], Curve87_data$unique_id[1], Curve87_data$machine[1], Curve87_data$baseline_yn[1],
                        Curve87_data$A[1], Curve87_data$Ci[1], Curve87_data$gsw[1],
                        mean(Curve87_data$VPDleaf, na.rm = T), mean(Curve87_data$Tleaf, na.rm = T), mean(Curve87_data$Qin, na.rm = T),
                        Curve87_fit[[2]][1,1], Curve87_fit[[2]][1,2],
                        Curve87_fit[[2]][2,1], Curve87_fit[[2]][2,2],
                        Curve87_fit[[2]][3,1], Curve87_fit[[2]][3,2],
                        Curve87_fit$RMSE,
                        Curve87_fit$Ci_transition,
                        Curve87_fit$citransition,
                        Curve87_fit$Km,
                        Curve87_fit$GammaStar,
                        Curve87_fit$fitmethod,
                        Curve87_fit$Tcorrect,
                        Curve87_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve87_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve87_output) # add the curve fits to the larger data frame

### Curve88_data
Curve88_data <- subset(aci.df, unique_id == aci.df.unique_id[88]) # find correct curve from full dataframe and make new object
plot(Curve88_data$A~Curve88_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve88_fit <- fitaci(Curve88_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve88_fit) # take a look at fitted values, adjust as needed
plot(Curve88_fit) # plot the fitted curves over the raw data, adjust as needed
Curve88_output <- cbind('Curve88', Curve88_data$id[1], Curve88_data$unique_id[1], Curve88_data$machine[1], Curve88_data$baseline_yn[1],
                        Curve88_data$A[1], Curve88_data$Ci[1], Curve88_data$gsw[1],
                        mean(Curve88_data$VPDleaf, na.rm = T), mean(Curve88_data$Tleaf, na.rm = T), mean(Curve88_data$Qin, na.rm = T),
                        Curve88_fit[[2]][1,1], Curve88_fit[[2]][1,2],
                        Curve88_fit[[2]][2,1], Curve88_fit[[2]][2,2],
                        Curve88_fit[[2]][3,1], Curve88_fit[[2]][3,2],
                        Curve88_fit$RMSE,
                        Curve88_fit$Ci_transition,
                        Curve88_fit$citransition,
                        Curve88_fit$Km,
                        Curve88_fit$GammaStar,
                        Curve88_fit$fitmethod,
                        Curve88_fit$Tcorrect,
                        Curve88_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve88_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve88_output) # add the curve fits to the larger data frame

Curve89_data <- subset(aci.df, unique_id == aci.df.unique_id[89]) # find correct curve from full dataframe and make new object
plot(Curve89_data$A~Curve89_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve89_fit <- fitaci(Curve89_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve89_fit) # take a look at fitted values, adjust as needed
plot(Curve89_fit) # plot the fitted curves over the raw data, adjust as needed
Curve89_output <- cbind('Curve89', Curve89_data$id[1], Curve89_data$unique_id[1], Curve89_data$machine[1], Curve89_data$baseline_yn[1],
                        Curve89_data$A[1], Curve89_data$Ci[1], Curve89_data$gsw[1],
                        mean(Curve89_data$VPDleaf, na.rm = T), mean(Curve89_data$Tleaf, na.rm = T), mean(Curve89_data$Qin, na.rm = T),
                        Curve89_fit[[2]][1,1], Curve89_fit[[2]][1,2],
                        Curve89_fit[[2]][2,1], Curve89_fit[[2]][2,2],
                        Curve89_fit[[2]][3,1], Curve89_fit[[2]][3,2],
                        Curve89_fit$RMSE,
                        Curve89_fit$Ci_transition,
                        Curve89_fit$citransition,
                        Curve89_fit$Km,
                        Curve89_fit$GammaStar,
                        Curve89_fit$fitmethod,
                        Curve89_fit$Tcorrect,
                        Curve89_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve89_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve89_output) # add the curve fits to the larger data frame

### Curve90_data
Curve90_data <- subset(aci.df, unique_id == aci.df.unique_id[90]) # find correct curve from full dataframe and make new object
plot(Curve90_data$A~Curve90_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve90_fit <- fitaci(Curve90_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve90_fit) # take a look at fitted values, adjust as needed
plot(Curve90_fit) # plot the fitted curves over the raw data, adjust as needed
Curve90_output <- cbind('Curve90', Curve90_data$id[1], Curve90_data$unique_id[1], Curve90_data$machine[1], Curve90_data$baseline_yn[1],
                        Curve90_data$A[1], Curve90_data$Ci[1], Curve90_data$gsw[1],
                        mean(Curve90_data$VPDleaf, na.rm = T), mean(Curve90_data$Tleaf, na.rm = T), mean(Curve90_data$Qin, na.rm = T),
                        Curve90_fit[[2]][1,1], Curve90_fit[[2]][1,2],
                        Curve90_fit[[2]][2,1], Curve90_fit[[2]][2,2],
                        Curve90_fit[[2]][3,1], Curve90_fit[[2]][3,2],
                        Curve90_fit$RMSE,
                        Curve90_fit$Ci_transition,
                        Curve90_fit$citransition,
                        Curve90_fit$Km,
                        Curve90_fit$GammaStar,
                        Curve90_fit$fitmethod,
                        Curve90_fit$Tcorrect,
                        Curve90_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve90_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve90_output) # add the curve fits to the larger data frame

### Curve91_data
Curve91_data <- subset(aci.df, unique_id == aci.df.unique_id[91]) # find correct curve from full dataframe and make new object
plot(Curve91_data$A~Curve91_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve91_fit <- fitaci(Curve91_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve91_fit) # take a look at fitted values, adjust as needed
plot(Curve91_fit) # plot the fitted curves over the raw data, adjust as needed
Curve91_output <- cbind('Curve91', Curve91_data$id[1], Curve91_data$unique_id[1], Curve91_data$machine[1], Curve91_data$baseline_yn[1],
                        Curve91_data$A[1], Curve91_data$Ci[1], Curve91_data$gsw[1],
                        mean(Curve91_data$VPDleaf, na.rm = T), mean(Curve91_data$Tleaf, na.rm = T), mean(Curve91_data$Qin, na.rm = T),
                        Curve91_fit[[2]][1,1], Curve91_fit[[2]][1,2],
                        Curve91_fit[[2]][2,1], Curve91_fit[[2]][2,2],
                        Curve91_fit[[2]][3,1], Curve91_fit[[2]][3,2],
                        Curve91_fit$RMSE,
                        Curve91_fit$Ci_transition,
                        Curve91_fit$citransition,
                        Curve91_fit$Km,
                        Curve91_fit$GammaStar,
                        Curve91_fit$fitmethod,
                        Curve91_fit$Tcorrect,
                        Curve91_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve91_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve91_output) # add the curve fits to the larger data frame

### Curve92_data
Curve92_data <- subset(aci.df, unique_id == aci.df.unique_id[92]) # find correct curve from full dataframe and make new object
plot(Curve92_data$A~Curve92_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve92_fit <- fitaci(Curve92_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve92_fit) # take a look at fitted values, adjust as needed
plot(Curve92_fit) # plot the fitted curves over the raw data, adjust as needed
Curve92_output <- cbind('Curve92', Curve92_data$id[1], Curve92_data$unique_id[1], Curve92_data$machine[1], Curve92_data$baseline_yn[1],
                        Curve92_data$A[1], Curve92_data$Ci[1], Curve92_data$gsw[1],
                        mean(Curve92_data$VPDleaf, na.rm = T), mean(Curve92_data$Tleaf, na.rm = T), mean(Curve92_data$Qin, na.rm = T),
                        Curve92_fit[[2]][1,1], Curve92_fit[[2]][1,2],
                        Curve92_fit[[2]][2,1], Curve92_fit[[2]][2,2],
                        Curve92_fit[[2]][3,1], Curve92_fit[[2]][3,2],
                        Curve92_fit$RMSE,
                        Curve92_fit$Ci_transition,
                        Curve92_fit$citransition,
                        Curve92_fit$Km,
                        Curve92_fit$GammaStar,
                        Curve92_fit$fitmethod,
                        Curve92_fit$Tcorrect,
                        Curve92_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve92_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve92_output) # add the curve fits to the larger data frame

### Curve93_data
Curve93_data <- subset(aci.df, unique_id == aci.df.unique_id[93]) # find correct curve from full dataframe and make new object
plot(Curve93_data$A~Curve93_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve93_fit <- fitaci(Curve93_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve93_fit) # take a look at fitted values, adjust as needed
plot(Curve93_fit) # plot the fitted curves over the raw data, adjust as needed
Curve93_output <- cbind('Curve93', Curve93_data$id[1], Curve93_data$unique_id[1], Curve93_data$machine[1], Curve93_data$baseline_yn[1],
                        Curve93_data$A[1], Curve93_data$Ci[1], Curve93_data$gsw[1],
                        mean(Curve93_data$VPDleaf, na.rm = T), mean(Curve93_data$Tleaf, na.rm = T), mean(Curve93_data$Qin, na.rm = T),
                        Curve93_fit[[2]][1,1], Curve93_fit[[2]][1,2],
                        Curve93_fit[[2]][2,1], Curve93_fit[[2]][2,2],
                        Curve93_fit[[2]][3,1], Curve93_fit[[2]][3,2],
                        Curve93_fit$RMSE,
                        Curve93_fit$Ci_transition,
                        Curve93_fit$citransition,
                        Curve93_fit$Km,
                        Curve93_fit$GammaStar,
                        Curve93_fit$fitmethod,
                        Curve93_fit$Tcorrect,
                        Curve93_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve93_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve93_output) # add the curve fits to the larger data frame

### Curve94_data
Curve94_data <- subset(aci.df, unique_id == aci.df.unique_id[94]) # find correct curve from full dataframe and make new object
plot(Curve94_data$A~Curve94_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve94_fit <- fitaci(Curve94_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve94_fit) # take a look at fitted values, adjust as needed
plot(Curve94_fit) # plot the fitted curves over the raw data, adjust as needed
Curve94_output <- cbind('Curve94', Curve94_data$id[1], Curve94_data$unique_id[1], Curve94_data$machine[1], Curve94_data$baseline_yn[1],
                        Curve94_data$A[1], Curve94_data$Ci[1], Curve94_data$gsw[1],
                        mean(Curve94_data$VPDleaf, na.rm = T), mean(Curve94_data$Tleaf, na.rm = T), mean(Curve94_data$Qin, na.rm = T),
                        Curve94_fit[[2]][1,1], Curve94_fit[[2]][1,2],
                        Curve94_fit[[2]][2,1], Curve94_fit[[2]][2,2],
                        Curve94_fit[[2]][3,1], Curve94_fit[[2]][3,2],
                        Curve94_fit$RMSE,
                        Curve94_fit$Ci_transition,
                        Curve94_fit$citransition,
                        Curve94_fit$Km,
                        Curve94_fit$GammaStar,
                        Curve94_fit$fitmethod,
                        Curve94_fit$Tcorrect,
                        Curve94_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve94_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve94_output) # add the curve fits to the larger data frame

### Curve95_data
Curve95_data <- subset(aci.df, unique_id == aci.df.unique_id[95]) # find correct curve from full dataframe and make new object
plot(Curve95_data$A~Curve95_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve95_fit <- fitaci(Curve95_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve95_fit) # take a look at fitted values, adjust as needed
plot(Curve95_fit) # plot the fitted curves over the raw data, adjust as needed
Curve95_output <- cbind('Curve95', Curve95_data$id[1], Curve95_data$unique_id[1], Curve95_data$machine[1], Curve95_data$baseline_yn[1],
                        Curve95_data$A[1], Curve95_data$Ci[1], Curve95_data$gsw[1],
                        mean(Curve95_data$VPDleaf, na.rm = T), mean(Curve95_data$Tleaf, na.rm = T), mean(Curve95_data$Qin, na.rm = T),
                        Curve95_fit[[2]][1,1], Curve95_fit[[2]][1,2],
                        Curve95_fit[[2]][2,1], Curve95_fit[[2]][2,2],
                        Curve95_fit[[2]][3,1], Curve95_fit[[2]][3,2],
                        Curve95_fit$RMSE,
                        Curve95_fit$Ci_transition,
                        Curve95_fit$citransition,
                        Curve95_fit$Km,
                        Curve95_fit$GammaStar,
                        Curve95_fit$fitmethod,
                        Curve95_fit$Tcorrect,
                        Curve95_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve95_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve95_output) # add the curve fits to the larger data frame

### Curve96_data
Curve96_data <- subset(aci.df, unique_id == aci.df.unique_id[96]) # find correct curve from full dataframe and make new object
plot(Curve96_data$A~Curve96_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve96_fit <- fitaci(Curve96_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve96_fit) # take a look at fitted values, adjust as needed
plot(Curve96_fit) # plot the fitted curves over the raw data, adjust as needed
Curve96_output <- cbind('Curve96', Curve96_data$id[1], Curve96_data$unique_id[1], Curve96_data$machine[1], Curve96_data$baseline_yn[1],
                        Curve96_data$A[1], Curve96_data$Ci[1], Curve96_data$gsw[1],
                        mean(Curve96_data$VPDleaf, na.rm = T), mean(Curve96_data$Tleaf, na.rm = T), mean(Curve96_data$Qin, na.rm = T),
                        Curve96_fit[[2]][1,1], Curve96_fit[[2]][1,2],
                        Curve96_fit[[2]][2,1], Curve96_fit[[2]][2,2],
                        Curve96_fit[[2]][3,1], Curve96_fit[[2]][3,2],
                        Curve96_fit$RMSE,
                        Curve96_fit$Ci_transition,
                        Curve96_fit$citransition,
                        Curve96_fit$Km,
                        Curve96_fit$GammaStar,
                        Curve96_fit$fitmethod,
                        Curve96_fit$Tcorrect,
                        Curve96_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve96_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve96_output) # add the curve fits to the larger data frame

### Curve97_data
Curve97_data <- subset(aci.df, unique_id == aci.df.unique_id[97]) # find correct curve from full dataframe and make new object
plot(Curve97_data$A~Curve97_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve97_fit <- fitaci(Curve97_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve97_fit) # take a look at fitted values, adjust as needed
plot(Curve97_fit) # plot the fitted curves over the raw data, adjust as needed
Curve97_output <- cbind('Curve97', Curve97_data$id[1], Curve97_data$unique_id[1], Curve97_data$machine[1], Curve97_data$baseline_yn[1],
                        Curve97_data$A[1], Curve97_data$Ci[1], Curve97_data$gsw[1],
                        mean(Curve97_data$VPDleaf, na.rm = T), mean(Curve97_data$Tleaf, na.rm = T), mean(Curve97_data$Qin, na.rm = T),
                        Curve97_fit[[2]][1,1], Curve97_fit[[2]][1,2],
                        Curve97_fit[[2]][2,1], Curve97_fit[[2]][2,2],
                        Curve97_fit[[2]][3,1], Curve97_fit[[2]][3,2],
                        Curve97_fit$RMSE,
                        Curve97_fit$Ci_transition,
                        Curve97_fit$citransition,
                        Curve97_fit$Km,
                        Curve97_fit$GammaStar,
                        Curve97_fit$fitmethod,
                        Curve97_fit$Tcorrect,
                        Curve97_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve97_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve97_output) # add the curve fits to the larger data frame

### Curve98_data
Curve98_data <- subset(aci.df, unique_id == aci.df.unique_id[98]) # find correct curve from full dataframe and make new object
plot(Curve98_data$A~Curve98_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve98_fit <- fitaci(Curve98_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve98_fit) # take a look at fitted values, adjust as needed
plot(Curve98_fit) # plot the fitted curves over the raw data, adjust as needed
Curve98_output <- cbind('Curve98', Curve98_data$id[1], Curve98_data$unique_id[1], Curve98_data$machine[1], Curve98_data$baseline_yn[1],
                        Curve98_data$A[1], Curve98_data$Ci[1], Curve98_data$gsw[1],
                        mean(Curve98_data$VPDleaf, na.rm = T), mean(Curve98_data$Tleaf, na.rm = T), mean(Curve98_data$Qin, na.rm = T),
                        Curve98_fit[[2]][1,1], Curve98_fit[[2]][1,2],
                        Curve98_fit[[2]][2,1], Curve98_fit[[2]][2,2],
                        Curve98_fit[[2]][3,1], Curve98_fit[[2]][3,2],
                        Curve98_fit$RMSE,
                        Curve98_fit$Ci_transition,
                        Curve98_fit$citransition,
                        Curve98_fit$Km,
                        Curve98_fit$GammaStar,
                        Curve98_fit$fitmethod,
                        Curve98_fit$Tcorrect,
                        Curve98_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve98_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve98_output) # add the curve fits to the larger data frame

### Curve99_data
Curve99_data <- subset(aci.df, unique_id == aci.df.unique_id[99]) # find correct curve from full dataframe and make new object
plot(Curve99_data$A~Curve99_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve99_fit <- fitaci(Curve99_data, varnames = list(ALEAF = "A", # fit the curves
                                                    Tleaf = "Tleaf",
                                                    Ci = "Ci",
                                                    PPFD = "Qin"),
                      fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve99_fit) # take a look at fitted values, adjust as needed
plot(Curve99_fit) # plot the fitted curves over the raw data, adjust as needed
Curve99_output <- cbind('Curve99', Curve99_data$id[1], Curve99_data$unique_id[1], Curve99_data$machine[1], Curve99_data$baseline_yn[1],
                        Curve99_data$A[1], Curve99_data$Ci[1], Curve99_data$gsw[1],
                        mean(Curve99_data$VPDleaf, na.rm = T), mean(Curve99_data$Tleaf, na.rm = T), mean(Curve99_data$Qin, na.rm = T),
                        Curve99_fit[[2]][1,1], Curve99_fit[[2]][1,2],
                        Curve99_fit[[2]][2,1], Curve99_fit[[2]][2,2],
                        Curve99_fit[[2]][3,1], Curve99_fit[[2]][3,2],
                        Curve99_fit$RMSE,
                        Curve99_fit$Ci_transition,
                        Curve99_fit$citransition,
                        Curve99_fit$Km,
                        Curve99_fit$GammaStar,
                        Curve99_fit$fitmethod,
                        Curve99_fit$Tcorrect,
                        Curve99_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve99_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                              'anet_420', 'ci_420', 'gs_420',
                              'vpd_leaf', 'temperature_leaf', 'par_leaf',
                              'vcmax_tleaf', 'vcmax_tleaf_se',
                              'jmax_tleaf', 'jmax_tleaf_se', 
                              'rdfit_tleaf', 'rdfit_tleaf_se',
                              'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                              'aci_km', 'aci_gammastar', 'aci_fitmethod',
                              'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve99_output) # add the curve fits to the larger data frame

### Curve100_data
Curve100_data <- subset(aci.df, unique_id == aci.df.unique_id[100]) # find correct curve from full dataframe and make new object
plot(Curve100_data$A~Curve100_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve100_fit <- fitaci(Curve100_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve100_fit) # take a look at fitted values, adjust as needed
plot(Curve100_fit) # plot the fitted curves over the raw data, adjust as needed
Curve100_output <- cbind('Curve100', Curve100_data$id[1], Curve100_data$unique_id[1], Curve100_data$machine[1], Curve100_data$baseline_yn[1],
                         Curve100_data$A[1], Curve100_data$Ci[1], Curve100_data$gsw[1],
                         mean(Curve100_data$VPDleaf, na.rm = T), mean(Curve100_data$Tleaf, na.rm = T), mean(Curve100_data$Qin, na.rm = T),
                         Curve100_fit[[2]][1,1], Curve100_fit[[2]][1,2],
                         Curve100_fit[[2]][2,1], Curve100_fit[[2]][2,2],
                         Curve100_fit[[2]][3,1], Curve100_fit[[2]][3,2],
                         Curve100_fit$RMSE,
                         Curve100_fit$Ci_transition,
                         Curve100_fit$citransition,
                         Curve100_fit$Km,
                         Curve100_fit$GammaStar,
                         Curve100_fit$fitmethod,
                         Curve100_fit$Tcorrect,
                         Curve100_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve100_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve100_output) # add the curve fits to the larger data frame

### Curve101_data
Curve101_data <- subset(aci.df, unique_id == aci.df.unique_id[101]) # find correct curve from full dataframe and make new object
plot(Curve101_data$A~Curve101_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve101_fit <- fitaci(Curve101_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve101_fit) # take a look at fitted values, adjust as needed
plot(Curve101_fit) # plot the fitted curves over the raw data, adjust as needed
Curve101_output <- cbind('Curve101', Curve101_data$id[1], Curve101_data$unique_id[1], Curve101_data$machine[1], Curve101_data$baseline_yn[1],
                         Curve101_data$A[1], Curve101_data$Ci[1], Curve101_data$gsw[1],
                         mean(Curve101_data$VPDleaf, na.rm = T), mean(Curve101_data$Tleaf, na.rm = T), mean(Curve101_data$Qin, na.rm = T),
                         Curve101_fit[[2]][1,1], Curve101_fit[[2]][1,2],
                         Curve101_fit[[2]][2,1], Curve101_fit[[2]][2,2],
                         Curve101_fit[[2]][3,1], Curve101_fit[[2]][3,2],
                         Curve101_fit$RMSE,
                         Curve101_fit$Ci_transition,
                         Curve101_fit$citransition,
                         Curve101_fit$Km,
                         Curve101_fit$GammaStar,
                         Curve101_fit$fitmethod,
                         Curve101_fit$Tcorrect,
                         Curve101_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve101_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve101_output) # add the curve fits to the larger data frame

### Curve102_data
Curve102_data <- subset(aci.df, unique_id == aci.df.unique_id[102]) # find correct curve from full dataframe and make new object
plot(Curve102_data$A~Curve102_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve102_fit <- fitaci(Curve102_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve102_fit) # take a look at fitted values, adjust as needed
plot(Curve102_fit) # plot the fitted curves over the raw data, adjust as needed
Curve102_output <- cbind('Curve102', Curve102_data$id[1], Curve102_data$unique_id[1], Curve102_data$machine[1], Curve102_data$baseline_yn[1],
                         Curve102_data$A[1], Curve102_data$Ci[1], Curve102_data$gsw[1],
                         mean(Curve102_data$VPDleaf, na.rm = T), mean(Curve102_data$Tleaf, na.rm = T), mean(Curve102_data$Qin, na.rm = T),
                         Curve102_fit[[2]][1,1], Curve102_fit[[2]][1,2],
                         Curve102_fit[[2]][2,1], Curve102_fit[[2]][2,2],
                         Curve102_fit[[2]][3,1], Curve102_fit[[2]][3,2],
                         Curve102_fit$RMSE,
                         Curve102_fit$Ci_transition,
                         Curve102_fit$citransition,
                         Curve102_fit$Km,
                         Curve102_fit$GammaStar,
                         Curve102_fit$fitmethod,
                         Curve102_fit$Tcorrect,
                         Curve102_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve102_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve102_output) # add the curve fits to the larger data frame

### Curve103_data
Curve103_data <- subset(aci.df, unique_id == aci.df.unique_id[103]) # find correct curve from full dataframe and make new object
plot(Curve103_data$A~Curve103_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve103_fit <- fitaci(Curve103_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve103_fit) # take a look at fitted values, adjust as needed
plot(Curve103_fit) # plot the fitted curves over the raw data, adjust as needed
Curve103_output <- cbind('Curve103', Curve103_data$id[1], Curve103_data$unique_id[1], Curve103_data$machine[1], Curve103_data$baseline_yn[1],
                         Curve103_data$A[1], Curve103_data$Ci[1], Curve103_data$gsw[1],
                         mean(Curve103_data$VPDleaf, na.rm = T), mean(Curve103_data$Tleaf, na.rm = T), mean(Curve103_data$Qin, na.rm = T),
                         Curve103_fit[[2]][1,1], Curve103_fit[[2]][1,2],
                         Curve103_fit[[2]][2,1], Curve103_fit[[2]][2,2],
                         Curve103_fit[[2]][3,1], Curve103_fit[[2]][3,2],
                         Curve103_fit$RMSE,
                         Curve103_fit$Ci_transition,
                         Curve103_fit$citransition,
                         Curve103_fit$Km,
                         Curve103_fit$GammaStar,
                         Curve103_fit$fitmethod,
                         Curve103_fit$Tcorrect,
                         Curve103_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve103_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve103_output) # add the curve fits to the larger data frame

### Curve104_data
Curve104_data <- subset(aci.df, unique_id == aci.df.unique_id[104]) # find correct curve from full dataframe and make new object
plot(Curve104_data$A~Curve104_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve104_fit <- fitaci(Curve104_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve104_fit) # take a look at fitted values, adjust as needed
plot(Curve104_fit) # plot the fitted curves over the raw data, adjust as needed
Curve104_output <- cbind('Curve104', Curve104_data$id[1], Curve104_data$unique_id[1], Curve104_data$machine[1], Curve104_data$baseline_yn[1],
                         Curve104_data$A[1], Curve104_data$Ci[1], Curve104_data$gsw[1],
                         mean(Curve104_data$VPDleaf, na.rm = T), mean(Curve104_data$Tleaf, na.rm = T), mean(Curve104_data$Qin, na.rm = T),
                         Curve104_fit[[2]][1,1], Curve104_fit[[2]][1,2],
                         Curve104_fit[[2]][2,1], Curve104_fit[[2]][2,2],
                         Curve104_fit[[2]][3,1], Curve104_fit[[2]][3,2],
                         Curve104_fit$RMSE,
                         Curve104_fit$Ci_transition,
                         Curve104_fit$citransition,
                         Curve104_fit$Km,
                         Curve104_fit$GammaStar,
                         Curve104_fit$fitmethod,
                         Curve104_fit$Tcorrect,
                         Curve104_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve104_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve104_output) # add the curve fits to the larger data frame

### Curve105_data
Curve105_data <- subset(aci.df, unique_id == aci.df.unique_id[105]) # find correct curve from full dataframe and make new object
plot(Curve105_data$A~Curve105_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve105_fit <- fitaci(Curve105_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve105_fit) # take a look at fitted values, adjust as needed
plot(Curve105_fit) # plot the fitted curves over the raw data, adjust as needed
Curve105_output <- cbind('Curve105', Curve105_data$id[1], Curve105_data$unique_id[1], Curve105_data$machine[1], Curve105_data$baseline_yn[1],
                         Curve105_data$A[1], Curve105_data$Ci[1], Curve105_data$gsw[1],
                         mean(Curve105_data$VPDleaf, na.rm = T), mean(Curve105_data$Tleaf, na.rm = T), mean(Curve105_data$Qin, na.rm = T),
                         Curve105_fit[[2]][1,1], Curve105_fit[[2]][1,2],
                         Curve105_fit[[2]][2,1], Curve105_fit[[2]][2,2],
                         Curve105_fit[[2]][3,1], Curve105_fit[[2]][3,2],
                         Curve105_fit$RMSE,
                         Curve105_fit$Ci_transition,
                         Curve105_fit$citransition,
                         Curve105_fit$Km,
                         Curve105_fit$GammaStar,
                         Curve105_fit$fitmethod,
                         Curve105_fit$Tcorrect,
                         Curve105_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve105_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve105_output) # add the curve fits to the larger data frame

### Curve106_data
Curve106_data <- subset(aci.df, unique_id == aci.df.unique_id[106]) # find correct curve from full dataframe and make new object
plot(Curve106_data$A~Curve106_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve106_fit <- fitaci(Curve106_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve106_fit) # take a look at fitted values, adjust as needed
plot(Curve106_fit) # plot the fitted curves over the raw data, adjust as needed
Curve106_output <- cbind('Curve106', Curve106_data$id[1], Curve106_data$unique_id[1], Curve106_data$machine[1], Curve106_data$baseline_yn[1],
                         Curve106_data$A[1], Curve106_data$Ci[1], Curve106_data$gsw[1],
                         mean(Curve106_data$VPDleaf, na.rm = T), mean(Curve106_data$Tleaf, na.rm = T), mean(Curve106_data$Qin, na.rm = T),
                         Curve106_fit[[2]][1,1], Curve106_fit[[2]][1,2],
                         Curve106_fit[[2]][2,1], Curve106_fit[[2]][2,2],
                         Curve106_fit[[2]][3,1], Curve106_fit[[2]][3,2],
                         Curve106_fit$RMSE,
                         Curve106_fit$Ci_transition,
                         Curve106_fit$citransition,
                         Curve106_fit$Km,
                         Curve106_fit$GammaStar,
                         Curve106_fit$fitmethod,
                         Curve106_fit$Tcorrect,
                         Curve106_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve106_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve106_output) # add the curve fits to the larger data frame

### Curve107_data
Curve107_data <- subset(aci.df, unique_id == aci.df.unique_id[107]) # find correct curve from full dataframe and make new object
plot(Curve107_data$A~Curve107_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve107_fit <- fitaci(Curve107_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve107_fit) # take a look at fitted values, adjust as needed
plot(Curve107_fit) # plot the fitted curves over the raw data, adjust as needed
Curve107_output <- cbind('Curve107', Curve107_data$id[1], Curve107_data$unique_id[1], Curve107_data$machine[1], Curve107_data$baseline_yn[1],
                         Curve107_data$A[1], Curve107_data$Ci[1], Curve107_data$gsw[1],
                         mean(Curve107_data$VPDleaf, na.rm = T), mean(Curve107_data$Tleaf, na.rm = T), mean(Curve107_data$Qin, na.rm = T),
                         Curve107_fit[[2]][1,1], Curve107_fit[[2]][1,2],
                         Curve107_fit[[2]][2,1], Curve107_fit[[2]][2,2],
                         Curve107_fit[[2]][3,1], Curve107_fit[[2]][3,2],
                         Curve107_fit$RMSE,
                         Curve107_fit$Ci_transition,
                         Curve107_fit$citransition,
                         Curve107_fit$Km,
                         Curve107_fit$GammaStar,
                         Curve107_fit$fitmethod,
                         Curve107_fit$Tcorrect,
                         Curve107_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve107_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve107_output) # add the curve fits to the larger data frame

### Curve108_data
Curve108_data <- subset(aci.df, unique_id == aci.df.unique_id[108] & Ci < 1000) # find correct curve from full dataframe and make new object
plot(Curve108_data$A~Curve108_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve108_fit <- fitaci(Curve108_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve108_fit) # take a look at fitted values, adjust as needed
plot(Curve108_fit) # plot the fitted curves over the raw data, adjust as needed
Curve108_output <- cbind('Curve108', Curve108_data$id[1], Curve108_data$unique_id[1], Curve108_data$machine[1], Curve108_data$baseline_yn[1],
                         Curve108_data$A[1], Curve108_data$Ci[1], Curve108_data$gsw[1],
                         mean(Curve108_data$VPDleaf, na.rm = T), mean(Curve108_data$Tleaf, na.rm = T), mean(Curve108_data$Qin, na.rm = T),
                         Curve108_fit[[2]][1,1], Curve108_fit[[2]][1,2],
                         Curve108_fit[[2]][2,1], Curve108_fit[[2]][2,2],
                         Curve108_fit[[2]][3,1], Curve108_fit[[2]][3,2],
                         Curve108_fit$RMSE,
                         Curve108_fit$Ci_transition,
                         Curve108_fit$citransition,
                         Curve108_fit$Km,
                         Curve108_fit$GammaStar,
                         Curve108_fit$fitmethod,
                         Curve108_fit$Tcorrect,
                         Curve108_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve108_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve108_output) # add the curve fits to the larger data frame

### Curve109_data
Curve109_data <- subset(aci.df, unique_id == aci.df.unique_id[109] & Ci < 1000) # find correct curve from full dataframe and make new object
plot(Curve109_data$A~Curve109_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve109_fit <- fitaci(Curve109_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve109_fit) # take a look at fitted values, adjust as needed
plot(Curve109_fit) # plot the fitted curves over the raw data, adjust as needed
Curve109_output <- cbind('Curve109', Curve109_data$id[1], Curve109_data$unique_id[1], Curve109_data$machine[1], Curve109_data$baseline_yn[1],
                         Curve109_data$A[1], Curve109_data$Ci[1], Curve109_data$gsw[1],
                         mean(Curve109_data$VPDleaf, na.rm = T), mean(Curve109_data$Tleaf, na.rm = T), mean(Curve109_data$Qin, na.rm = T),
                         Curve109_fit[[2]][1,1], Curve109_fit[[2]][1,2],
                         Curve109_fit[[2]][2,1], Curve109_fit[[2]][2,2],
                         Curve109_fit[[2]][3,1], Curve109_fit[[2]][3,2],
                         Curve109_fit$RMSE,
                         Curve109_fit$Ci_transition,
                         Curve109_fit$citransition,
                         Curve109_fit$Km,
                         Curve109_fit$GammaStar,
                         Curve109_fit$fitmethod,
                         Curve109_fit$Tcorrect,
                         Curve109_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve109_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve109_output) # add the curve fits to the larger data frame

### Curve110_data
Curve110_data <- subset(aci.df, unique_id == aci.df.unique_id[110] & Ci < 1000) # find correct curve from full dataframe and make new object
plot(Curve110_data$A~Curve110_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve110_fit <- fitaci(Curve110_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve110_fit) # take a look at fitted values, adjust as needed
plot(Curve110_fit) # plot the fitted curves over the raw data, adjust as needed
Curve110_output <- cbind('Curve110', Curve110_data$id[1], Curve110_data$unique_id[1], Curve110_data$machine[1], Curve110_data$baseline_yn[1],
                         Curve110_data$A[1], Curve110_data$Ci[1], Curve110_data$gsw[1],
                         mean(Curve110_data$VPDleaf, na.rm = T), mean(Curve110_data$Tleaf, na.rm = T), mean(Curve110_data$Qin, na.rm = T),
                         Curve110_fit[[2]][1,1], Curve110_fit[[2]][1,2],
                         Curve110_fit[[2]][2,1], Curve110_fit[[2]][2,2],
                         Curve110_fit[[2]][3,1], Curve110_fit[[2]][3,2],
                         Curve110_fit$RMSE,
                         Curve110_fit$Ci_transition,
                         Curve110_fit$citransition,
                         Curve110_fit$Km,
                         Curve110_fit$GammaStar,
                         Curve110_fit$fitmethod,
                         Curve110_fit$Tcorrect,
                         Curve110_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve110_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve110_output) # add the curve fits to the larger data frame

### Curve111_data
Curve111_data <- subset(aci.df, unique_id == aci.df.unique_id[111]) # find correct curve from full dataframe and make new object
plot(Curve111_data$A~Curve111_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve111_fit <- fitaci(Curve111_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve111_fit) # take a look at fitted values, adjust as needed
plot(Curve111_fit) # plot the fitted curves over the raw data, adjust as needed
Curve111_output <- cbind('Curve111', Curve111_data$id[1], Curve111_data$unique_id[1], Curve111_data$machine[1], Curve111_data$baseline_yn[1],
                         Curve111_data$A[1], Curve111_data$Ci[1], Curve111_data$gsw[1],
                         mean(Curve111_data$VPDleaf, na.rm = T), mean(Curve111_data$Tleaf, na.rm = T), mean(Curve111_data$Qin, na.rm = T),
                         Curve111_fit[[2]][1,1], Curve111_fit[[2]][1,2],
                         Curve111_fit[[2]][2,1], Curve111_fit[[2]][2,2],
                         Curve111_fit[[2]][3,1], Curve111_fit[[2]][3,2],
                         Curve111_fit$RMSE,
                         Curve111_fit$Ci_transition,
                         Curve111_fit$citransition,
                         Curve111_fit$Km,
                         Curve111_fit$GammaStar,
                         Curve111_fit$fitmethod,
                         Curve111_fit$Tcorrect,
                         Curve111_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve111_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve111_output) # add the curve fits to the larger data frame

### Curve112_data
Curve112_data <- subset(aci.df, unique_id == aci.df.unique_id[112]) # find correct curve from full dataframe and make new object
plot(Curve112_data$A~Curve112_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve112_fit <- fitaci(Curve112_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve112_fit) # take a look at fitted values, adjust as needed
plot(Curve112_fit) # plot the fitted curves over the raw data, adjust as needed
Curve112_output <- cbind('Curve112', Curve112_data$id[1], Curve112_data$unique_id[1], Curve112_data$machine[1], Curve112_data$baseline_yn[1],
                         Curve112_data$A[1], Curve112_data$Ci[1], Curve112_data$gsw[1],
                         mean(Curve112_data$VPDleaf, na.rm = T), mean(Curve112_data$Tleaf, na.rm = T), mean(Curve112_data$Qin, na.rm = T),
                         Curve112_fit[[2]][1,1], Curve112_fit[[2]][1,2],
                         Curve112_fit[[2]][2,1], Curve112_fit[[2]][2,2],
                         Curve112_fit[[2]][3,1], Curve112_fit[[2]][3,2],
                         Curve112_fit$RMSE,
                         Curve112_fit$Ci_transition,
                         Curve112_fit$citransition,
                         Curve112_fit$Km,
                         Curve112_fit$GammaStar,
                         Curve112_fit$fitmethod,
                         Curve112_fit$Tcorrect,
                         Curve112_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve112_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve112_output) # add the curve fits to the larger data frame

### Curve113_data
Curve113_data <- subset(aci.df, unique_id == aci.df.unique_id[113]) # find correct curve from full dataframe and make new object
plot(Curve113_data$A~Curve113_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve113_fit <- fitaci(Curve113_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve113_fit) # take a look at fitted values, adjust as needed
plot(Curve113_fit) # plot the fitted curves over the raw data, adjust as needed
Curve113_output <- cbind('Curve113', Curve113_data$id[1], Curve113_data$unique_id[1], Curve113_data$machine[1], Curve113_data$baseline_yn[1],
                         Curve113_data$A[1], Curve113_data$Ci[1], Curve113_data$gsw[1],
                         mean(Curve113_data$VPDleaf, na.rm = T), mean(Curve113_data$Tleaf, na.rm = T), mean(Curve113_data$Qin, na.rm = T),
                         Curve113_fit[[2]][1,1], Curve113_fit[[2]][1,2],
                         Curve113_fit[[2]][2,1], Curve113_fit[[2]][2,2],
                         Curve113_fit[[2]][3,1], Curve113_fit[[2]][3,2],
                         Curve113_fit$RMSE,
                         Curve113_fit$Ci_transition,
                         Curve113_fit$citransition,
                         Curve113_fit$Km,
                         Curve113_fit$GammaStar,
                         Curve113_fit$fitmethod,
                         Curve113_fit$Tcorrect,
                         Curve113_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve113_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve113_output) # add the curve fits to the larger data frame

### Curve114_data
Curve114_data <- subset(aci.df, unique_id == aci.df.unique_id[114]) # find correct curve from full dataframe and make new object
plot(Curve114_data$A~Curve114_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve114_fit <- fitaci(Curve114_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve114_fit) # take a look at fitted values, adjust as needed
plot(Curve114_fit) # plot the fitted curves over the raw data, adjust as needed
Curve114_output <- cbind('Curve114', Curve114_data$id[1], Curve114_data$unique_id[1], Curve114_data$machine[1], Curve114_data$baseline_yn[1],
                         Curve114_data$A[1], Curve114_data$Ci[1], Curve114_data$gsw[1],
                         mean(Curve114_data$VPDleaf, na.rm = T), mean(Curve114_data$Tleaf, na.rm = T), mean(Curve114_data$Qin, na.rm = T),
                         Curve114_fit[[2]][1,1], Curve114_fit[[2]][1,2],
                         Curve114_fit[[2]][2,1], Curve114_fit[[2]][2,2],
                         Curve114_fit[[2]][3,1], Curve114_fit[[2]][3,2],
                         Curve114_fit$RMSE,
                         Curve114_fit$Ci_transition,
                         Curve114_fit$citransition,
                         Curve114_fit$Km,
                         Curve114_fit$GammaStar,
                         Curve114_fit$fitmethod,
                         Curve114_fit$Tcorrect,
                         Curve114_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve114_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve114_output) # add the curve fits to the larger data frame

### Curve115_data
Curve115_data <- subset(aci.df, unique_id == aci.df.unique_id[115]) # find correct curve from full dataframe and make new object
plot(Curve115_data$A~Curve115_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve115_fit <- fitaci(Curve115_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve115_fit) # take a look at fitted values, adjust as needed
plot(Curve115_fit) # plot the fitted curves over the raw data, adjust as needed
Curve115_output <- cbind('Curve115', Curve115_data$id[1], Curve115_data$unique_id[1], Curve115_data$machine[1], Curve115_data$baseline_yn[1],
                         Curve115_data$A[1], Curve115_data$Ci[1], Curve115_data$gsw[1],
                         mean(Curve115_data$VPDleaf, na.rm = T), mean(Curve115_data$Tleaf, na.rm = T), mean(Curve115_data$Qin, na.rm = T),
                         Curve115_fit[[2]][1,1], Curve115_fit[[2]][1,2],
                         Curve115_fit[[2]][2,1], Curve115_fit[[2]][2,2],
                         Curve115_fit[[2]][3,1], Curve115_fit[[2]][3,2],
                         Curve115_fit$RMSE,
                         Curve115_fit$Ci_transition,
                         Curve115_fit$citransition,
                         Curve115_fit$Km,
                         Curve115_fit$GammaStar,
                         Curve115_fit$fitmethod,
                         Curve115_fit$Tcorrect,
                         Curve115_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve115_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve115_output) # add the curve fits to the larger data frame

### Curve116_data
Curve116_data <- subset(aci.df, unique_id == aci.df.unique_id[116]) # find correct curve from full dataframe and make new object
plot(Curve116_data$A~Curve116_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve116_fit <- fitaci(Curve116_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve116_fit) # take a look at fitted values, adjust as needed
plot(Curve116_fit) # plot the fitted curves over the raw data, adjust as needed
Curve116_output <- cbind('Curve116', Curve116_data$id[1], Curve116_data$unique_id[1], Curve116_data$machine[1], Curve116_data$baseline_yn[1],
                         Curve116_data$A[1], Curve116_data$Ci[1], Curve116_data$gsw[1],
                         mean(Curve116_data$VPDleaf, na.rm = T), mean(Curve116_data$Tleaf, na.rm = T), mean(Curve116_data$Qin, na.rm = T),
                         Curve116_fit[[2]][1,1], Curve116_fit[[2]][1,2],
                         Curve116_fit[[2]][2,1], Curve116_fit[[2]][2,2],
                         Curve116_fit[[2]][3,1], Curve116_fit[[2]][3,2],
                         Curve116_fit$RMSE,
                         Curve116_fit$Ci_transition,
                         Curve116_fit$citransition,
                         Curve116_fit$Km,
                         Curve116_fit$GammaStar,
                         Curve116_fit$fitmethod,
                         Curve116_fit$Tcorrect,
                         Curve116_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve116_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve116_output) # add the curve fits to the larger data frame

### Curve117_data
Curve117_data <- subset(aci.df, unique_id == aci.df.unique_id[117]) # find correct curve from full dataframe and make new object
plot(Curve117_data$A~Curve117_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve117_fit <- fitaci(Curve117_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve117_fit) # take a look at fitted values, adjust as needed
plot(Curve117_fit) # plot the fitted curves over the raw data, adjust as needed
Curve117_output <- cbind('Curve117', Curve117_data$id[1], Curve117_data$unique_id[1], Curve117_data$machine[1], Curve117_data$baseline_yn[1],
                         Curve117_data$A[1], Curve117_data$Ci[1], Curve117_data$gsw[1],
                         mean(Curve117_data$VPDleaf, na.rm = T), mean(Curve117_data$Tleaf, na.rm = T), mean(Curve117_data$Qin, na.rm = T),
                         Curve117_fit[[2]][1,1], Curve117_fit[[2]][1,2],
                         Curve117_fit[[2]][2,1], Curve117_fit[[2]][2,2],
                         Curve117_fit[[2]][3,1], Curve117_fit[[2]][3,2],
                         Curve117_fit$RMSE,
                         Curve117_fit$Ci_transition,
                         Curve117_fit$citransition,
                         Curve117_fit$Km,
                         Curve117_fit$GammaStar,
                         Curve117_fit$fitmethod,
                         Curve117_fit$Tcorrect,
                         Curve117_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve117_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve117_output) # add the curve fits to the larger data frame

### Curve118_data
Curve118_data <- subset(aci.df, unique_id == aci.df.unique_id[118]) # find correct curve from full dataframe and make new object
plot(Curve118_data$A~Curve118_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve118_fit <- fitaci(Curve118_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve118_fit) # take a look at fitted values, adjust as needed
plot(Curve118_fit) # plot the fitted curves over the raw data, adjust as needed
Curve118_output <- cbind('Curve118', Curve118_data$id[1], Curve118_data$unique_id[1], Curve118_data$machine[1], Curve118_data$baseline_yn[1],
                         Curve118_data$A[1], Curve118_data$Ci[1], Curve118_data$gsw[1],
                         mean(Curve118_data$VPDleaf, na.rm = T), mean(Curve118_data$Tleaf, na.rm = T), mean(Curve118_data$Qin, na.rm = T),
                         Curve118_fit[[2]][1,1], Curve118_fit[[2]][1,2],
                         Curve118_fit[[2]][2,1], Curve118_fit[[2]][2,2],
                         Curve118_fit[[2]][3,1], Curve118_fit[[2]][3,2],
                         Curve118_fit$RMSE,
                         Curve118_fit$Ci_transition,
                         Curve118_fit$citransition,
                         Curve118_fit$Km,
                         Curve118_fit$GammaStar,
                         Curve118_fit$fitmethod,
                         Curve118_fit$Tcorrect,
                         Curve118_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve118_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve118_output) # add the curve fits to the larger data frame

### Curve119_data
Curve119_data <- subset(aci.df, unique_id == aci.df.unique_id[119]) # find correct curve from full dataframe and make new object
plot(Curve119_data$A~Curve119_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve119_fit <- fitaci(Curve119_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve119_fit) # take a look at fitted values, adjust as needed
plot(Curve119_fit) # plot the fitted curves over the raw data, adjust as needed
Curve119_output <- cbind('Curve119', Curve119_data$id[1], Curve119_data$unique_id[1], Curve119_data$machine[1], Curve119_data$baseline_yn[1],
                         Curve119_data$A[1], Curve119_data$Ci[1], Curve119_data$gsw[1],
                         mean(Curve119_data$VPDleaf, na.rm = T), mean(Curve119_data$Tleaf, na.rm = T), mean(Curve119_data$Qin, na.rm = T),
                         Curve119_fit[[2]][1,1], Curve119_fit[[2]][1,2],
                         Curve119_fit[[2]][2,1], Curve119_fit[[2]][2,2],
                         Curve119_fit[[2]][3,1], Curve119_fit[[2]][3,2],
                         Curve119_fit$RMSE,
                         Curve119_fit$Ci_transition,
                         Curve119_fit$citransition,
                         Curve119_fit$Km,
                         Curve119_fit$GammaStar,
                         Curve119_fit$fitmethod,
                         Curve119_fit$Tcorrect,
                         Curve119_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve119_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve119_output) # add the curve fits to the larger data frame

### Curve120_data
Curve120_data <- subset(aci.df, unique_id == aci.df.unique_id[120]) # find correct curve from full dataframe and make new object
plot(Curve120_data$A~Curve120_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve120_fit <- fitaci(Curve120_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve120_fit) # take a look at fitted values, adjust as needed
plot(Curve120_fit) # plot the fitted curves over the raw data, adjust as needed
Curve120_output <- cbind('Curve120', Curve120_data$id[1], Curve120_data$unique_id[1], Curve120_data$machine[1], Curve120_data$baseline_yn[1],
                         Curve120_data$A[1], Curve120_data$Ci[1], Curve120_data$gsw[1],
                         mean(Curve120_data$VPDleaf, na.rm = T), mean(Curve120_data$Tleaf, na.rm = T), mean(Curve120_data$Qin, na.rm = T),
                         Curve120_fit[[2]][1,1], Curve120_fit[[2]][1,2],
                         Curve120_fit[[2]][2,1], Curve120_fit[[2]][2,2],
                         Curve120_fit[[2]][3,1], Curve120_fit[[2]][3,2],
                         Curve120_fit$RMSE,
                         Curve120_fit$Ci_transition,
                         Curve120_fit$citransition,
                         Curve120_fit$Km,
                         Curve120_fit$GammaStar,
                         Curve120_fit$fitmethod,
                         Curve120_fit$Tcorrect,
                         Curve120_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve120_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve120_output) # add the curve fits to the larger data frame

### Curve121_data
Curve121_data <- subset(aci.df, unique_id == aci.df.unique_id[121]) # find correct curve from full dataframe and make new object
plot(Curve121_data$A~Curve121_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve121_fit <- fitaci(Curve121_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve121_fit) # take a look at fitted values, adjust as needed
plot(Curve121_fit) # plot the fitted curves over the raw data, adjust as needed
Curve121_output <- cbind('Curve121', Curve121_data$id[1], Curve121_data$unique_id[1], Curve121_data$machine[1], Curve121_data$baseline_yn[1],
                         Curve121_data$A[1], Curve121_data$Ci[1], Curve121_data$gsw[1],
                         mean(Curve121_data$VPDleaf, na.rm = T), mean(Curve121_data$Tleaf, na.rm = T), mean(Curve121_data$Qin, na.rm = T),
                         Curve121_fit[[2]][1,1], Curve121_fit[[2]][1,2],
                         Curve121_fit[[2]][2,1], Curve121_fit[[2]][2,2],
                         Curve121_fit[[2]][3,1], Curve121_fit[[2]][3,2],
                         Curve121_fit$RMSE,
                         Curve121_fit$Ci_transition,
                         Curve121_fit$citransition,
                         Curve121_fit$Km,
                         Curve121_fit$GammaStar,
                         Curve121_fit$fitmethod,
                         Curve121_fit$Tcorrect,
                         Curve121_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve121_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve121_output) # add the curve fits to the larger data frame

### Curve122_data
Curve122_data <- subset(aci.df, unique_id == aci.df.unique_id[122]) # find correct curve from full dataframe and make new object
plot(Curve122_data$A~Curve122_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve122_fit <- fitaci(Curve122_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve122_fit) # take a look at fitted values, adjust as needed
plot(Curve122_fit) # plot the fitted curves over the raw data, adjust as needed
Curve122_output <- cbind('Curve122', Curve122_data$id[1], Curve122_data$unique_id[1], Curve122_data$machine[1], Curve122_data$baseline_yn[1],
                         Curve122_data$A[1], Curve122_data$Ci[1], Curve122_data$gsw[1],
                         mean(Curve122_data$VPDleaf, na.rm = T), mean(Curve122_data$Tleaf, na.rm = T), mean(Curve122_data$Qin, na.rm = T),
                         Curve122_fit[[2]][1,1], Curve122_fit[[2]][1,2],
                         Curve122_fit[[2]][2,1], Curve122_fit[[2]][2,2],
                         Curve122_fit[[2]][3,1], Curve122_fit[[2]][3,2],
                         Curve122_fit$RMSE,
                         Curve122_fit$Ci_transition,
                         Curve122_fit$citransition,
                         Curve122_fit$Km,
                         Curve122_fit$GammaStar,
                         Curve122_fit$fitmethod,
                         Curve122_fit$Tcorrect,
                         Curve122_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve122_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve122_output) # add the curve fits to the larger data frame

### Curve123_data
Curve123_data <- subset(aci.df, unique_id == aci.df.unique_id[123]) # find correct curve from full dataframe and make new object
plot(Curve123_data$A~Curve123_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve123_fit <- fitaci(Curve123_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve123_fit) # take a look at fitted values, adjust as needed
plot(Curve123_fit) # plot the fitted curves over the raw data, adjust as needed
Curve123_output <- cbind('Curve123', Curve123_data$id[1], Curve123_data$unique_id[1], Curve123_data$machine[1], Curve123_data$baseline_yn[1],
                         Curve123_data$A[1], Curve123_data$Ci[1], Curve123_data$gsw[1],
                         mean(Curve123_data$VPDleaf, na.rm = T), mean(Curve123_data$Tleaf, na.rm = T), mean(Curve123_data$Qin, na.rm = T),
                         Curve123_fit[[2]][1,1], Curve123_fit[[2]][1,2],
                         Curve123_fit[[2]][2,1], Curve123_fit[[2]][2,2],
                         Curve123_fit[[2]][3,1], Curve123_fit[[2]][3,2],
                         Curve123_fit$RMSE,
                         Curve123_fit$Ci_transition,
                         Curve123_fit$citransition,
                         Curve123_fit$Km,
                         Curve123_fit$GammaStar,
                         Curve123_fit$fitmethod,
                         Curve123_fit$Tcorrect,
                         Curve123_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve123_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve123_output) # add the curve fits to the larger data frame

### Curve124_data
Curve124_data <- subset(aci.df, unique_id == aci.df.unique_id[124]) # find correct curve from full dataframe and make new object
plot(Curve124_data$A~Curve124_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve124_fit <- fitaci(Curve124_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve124_fit) # take a look at fitted values, adjust as needed
plot(Curve124_fit) # plot the fitted curves over the raw data, adjust as needed
Curve124_output <- cbind('Curve124', Curve124_data$id[1], Curve124_data$unique_id[1], Curve124_data$machine[1], Curve124_data$baseline_yn[1],
                         Curve124_data$A[1], Curve124_data$Ci[1], Curve124_data$gsw[1],
                         mean(Curve124_data$VPDleaf, na.rm = T), mean(Curve124_data$Tleaf, na.rm = T), mean(Curve124_data$Qin, na.rm = T),
                         Curve124_fit[[2]][1,1], Curve124_fit[[2]][1,2],
                         Curve124_fit[[2]][2,1], Curve124_fit[[2]][2,2],
                         Curve124_fit[[2]][3,1], Curve124_fit[[2]][3,2],
                         Curve124_fit$RMSE,
                         Curve124_fit$Ci_transition,
                         Curve124_fit$citransition,
                         Curve124_fit$Km,
                         Curve124_fit$GammaStar,
                         Curve124_fit$fitmethod,
                         Curve124_fit$Tcorrect,
                         Curve124_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve124_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve124_output) # add the curve fits to the larger data frame

### Curve125_data
Curve125_data <- subset(aci.df, unique_id == aci.df.unique_id[125]) # find correct curve from full dataframe and make new object
plot(Curve125_data$A~Curve125_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve125_fit <- fitaci(Curve125_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve125_fit) # take a look at fitted values, adjust as needed
plot(Curve125_fit) # plot the fitted curves over the raw data, adjust as needed
Curve125_output <- cbind('Curve125', Curve125_data$id[1], Curve125_data$unique_id[1], Curve125_data$machine[1], Curve125_data$baseline_yn[1],
                         Curve125_data$A[1], Curve125_data$Ci[1], Curve125_data$gsw[1],
                         mean(Curve125_data$VPDleaf, na.rm = T), mean(Curve125_data$Tleaf, na.rm = T), mean(Curve125_data$Qin, na.rm = T),
                         Curve125_fit[[2]][1,1], Curve125_fit[[2]][1,2],
                         Curve125_fit[[2]][2,1], Curve125_fit[[2]][2,2],
                         Curve125_fit[[2]][3,1], Curve125_fit[[2]][3,2],
                         Curve125_fit$RMSE,
                         Curve125_fit$Ci_transition,
                         Curve125_fit$citransition,
                         Curve125_fit$Km,
                         Curve125_fit$GammaStar,
                         Curve125_fit$fitmethod,
                         Curve125_fit$Tcorrect,
                         Curve125_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve125_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve125_output) # add the curve fits to the larger data frame

### Curve126_data
Curve126_data <- subset(aci.df, unique_id == aci.df.unique_id[126]) # find correct curve from full dataframe and make new object
plot(Curve126_data$A~Curve126_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve126_fit <- fitaci(Curve126_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve126_fit) # take a look at fitted values, adjust as needed
plot(Curve126_fit) # plot the fitted curves over the raw data, adjust as needed
Curve126_output <- cbind('Curve126', Curve126_data$id[1], Curve126_data$unique_id[1], Curve126_data$machine[1], Curve126_data$baseline_yn[1],
                         Curve126_data$A[1], Curve126_data$Ci[1], Curve126_data$gsw[1],
                         mean(Curve126_data$VPDleaf, na.rm = T), mean(Curve126_data$Tleaf, na.rm = T), mean(Curve126_data$Qin, na.rm = T),
                         Curve126_fit[[2]][1,1], Curve126_fit[[2]][1,2],
                         Curve126_fit[[2]][2,1], Curve126_fit[[2]][2,2],
                         Curve126_fit[[2]][3,1], Curve126_fit[[2]][3,2],
                         Curve126_fit$RMSE,
                         Curve126_fit$Ci_transition,
                         Curve126_fit$citransition,
                         Curve126_fit$Km,
                         Curve126_fit$GammaStar,
                         Curve126_fit$fitmethod,
                         Curve126_fit$Tcorrect,
                         Curve126_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve126_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve126_output) # add the curve fits to the larger data frame

### Curve127_data
Curve127_data <- subset(aci.df, unique_id == aci.df.unique_id[127]) # find correct curve from full dataframe and make new object
plot(Curve127_data$A~Curve127_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve127_fit <- fitaci(Curve127_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve127_fit) # take a look at fitted values, adjust as needed
plot(Curve127_fit) # plot the fitted curves over the raw data, adjust as needed
Curve127_output <- cbind('Curve127', Curve127_data$id[1], Curve127_data$unique_id[1], Curve127_data$machine[1], Curve127_data$baseline_yn[1],
                         Curve127_data$A[1], Curve127_data$Ci[1], Curve127_data$gsw[1],
                         mean(Curve127_data$VPDleaf, na.rm = T), mean(Curve127_data$Tleaf, na.rm = T), mean(Curve127_data$Qin, na.rm = T),
                         Curve127_fit[[2]][1,1], Curve127_fit[[2]][1,2],
                         Curve127_fit[[2]][2,1], Curve127_fit[[2]][2,2],
                         Curve127_fit[[2]][3,1], Curve127_fit[[2]][3,2],
                         Curve127_fit$RMSE,
                         Curve127_fit$Ci_transition,
                         Curve127_fit$citransition,
                         Curve127_fit$Km,
                         Curve127_fit$GammaStar,
                         Curve127_fit$fitmethod,
                         Curve127_fit$Tcorrect,
                         Curve127_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve127_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve127_output) # add the curve fits to the larger data frame

### Curve128_data
Curve128_data <- subset(aci.df, unique_id == aci.df.unique_id[128]) # find correct curve from full dataframe and make new object
plot(Curve128_data$A~Curve128_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve128_fit <- fitaci(Curve128_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve128_fit) # take a look at fitted values, adjust as needed
plot(Curve128_fit) # plot the fitted curves over the raw data, adjust as needed
Curve128_output <- cbind('Curve128', Curve128_data$id[1], Curve128_data$unique_id[1], Curve128_data$machine[1], Curve128_data$baseline_yn[1],
                         Curve128_data$A[1], Curve128_data$Ci[1], Curve128_data$gsw[1],
                         mean(Curve128_data$VPDleaf, na.rm = T), mean(Curve128_data$Tleaf, na.rm = T), mean(Curve128_data$Qin, na.rm = T),
                         Curve128_fit[[2]][1,1], Curve128_fit[[2]][1,2],
                         Curve128_fit[[2]][2,1], Curve128_fit[[2]][2,2],
                         Curve128_fit[[2]][3,1], Curve128_fit[[2]][3,2],
                         Curve128_fit$RMSE,
                         Curve128_fit$Ci_transition,
                         Curve128_fit$citransition,
                         Curve128_fit$Km,
                         Curve128_fit$GammaStar,
                         Curve128_fit$fitmethod,
                         Curve128_fit$Tcorrect,
                         Curve128_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve128_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve128_output) # add the curve fits to the larger data frame

### Curve129_data
Curve129_data <- subset(aci.df, unique_id == aci.df.unique_id[129]) # find correct curve from full dataframe and make new object
plot(Curve129_data$A~Curve129_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve129_fit <- fitaci(Curve129_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve129_fit) # take a look at fitted values, adjust as needed
plot(Curve129_fit) # plot the fitted curves over the raw data, adjust as needed
Curve129_output <- cbind('Curve129', Curve129_data$id[1], Curve129_data$unique_id[1], Curve129_data$machine[1], Curve129_data$baseline_yn[1],
                         Curve129_data$A[1], Curve129_data$Ci[1], Curve129_data$gsw[1],
                         mean(Curve129_data$VPDleaf, na.rm = T), mean(Curve129_data$Tleaf, na.rm = T), mean(Curve129_data$Qin, na.rm = T),
                         Curve129_fit[[2]][1,1], Curve129_fit[[2]][1,2],
                         Curve129_fit[[2]][2,1], Curve129_fit[[2]][2,2],
                         Curve129_fit[[2]][3,1], Curve129_fit[[2]][3,2],
                         Curve129_fit$RMSE,
                         Curve129_fit$Ci_transition,
                         Curve129_fit$citransition,
                         Curve129_fit$Km,
                         Curve129_fit$GammaStar,
                         Curve129_fit$fitmethod,
                         Curve129_fit$Tcorrect,
                         Curve129_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve129_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve129_output) # add the curve fits to the larger data frame

### Curve130_data
Curve130_data <- subset(aci.df, unique_id == aci.df.unique_id[130]  & Ci < 1000) # find correct curve from full dataframe and make new object
plot(Curve130_data$A~Curve130_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve130_fit <- fitaci(Curve130_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve130_fit) # take a look at fitted values, adjust as needed
plot(Curve130_fit) # plot the fitted curves over the raw data, adjust as needed
Curve130_output <- cbind('Curve130', Curve130_data$id[1], Curve130_data$unique_id[1], Curve130_data$machine[1], Curve130_data$baseline_yn[1],
                         Curve130_data$A[1], Curve130_data$Ci[1], Curve130_data$gsw[1],
                         mean(Curve130_data$VPDleaf, na.rm = T), mean(Curve130_data$Tleaf, na.rm = T), mean(Curve130_data$Qin, na.rm = T),
                         Curve130_fit[[2]][1,1], Curve130_fit[[2]][1,2],
                         Curve130_fit[[2]][2,1], Curve130_fit[[2]][2,2],
                         Curve130_fit[[2]][3,1], Curve130_fit[[2]][3,2],
                         Curve130_fit$RMSE,
                         Curve130_fit$Ci_transition,
                         Curve130_fit$citransition,
                         Curve130_fit$Km,
                         Curve130_fit$GammaStar,
                         Curve130_fit$fitmethod,
                         Curve130_fit$Tcorrect,
                         Curve130_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve130_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve130_output) # add the curve fits to the larger data frame

### Curve131_data
Curve131_data <- subset(aci.df, unique_id == aci.df.unique_id[131]) # find correct curve from full dataframe and make new object
plot(Curve131_data$A~Curve131_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve131_fit <- fitaci(Curve131_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve131_fit) # take a look at fitted values, adjust as needed
plot(Curve131_fit) # plot the fitted curves over the raw data, adjust as needed
Curve131_output <- cbind('Curve131', Curve131_data$id[1], Curve131_data$unique_id[1], Curve131_data$machine[1], Curve131_data$baseline_yn[1],
                         Curve131_data$A[1], Curve131_data$Ci[1], Curve131_data$gsw[1],
                         mean(Curve131_data$VPDleaf, na.rm = T), mean(Curve131_data$Tleaf, na.rm = T), mean(Curve131_data$Qin, na.rm = T),
                         Curve131_fit[[2]][1,1], Curve131_fit[[2]][1,2],
                         Curve131_fit[[2]][2,1], Curve131_fit[[2]][2,2],
                         Curve131_fit[[2]][3,1], Curve131_fit[[2]][3,2],
                         Curve131_fit$RMSE,
                         Curve131_fit$Ci_transition,
                         Curve131_fit$citransition,
                         Curve131_fit$Km,
                         Curve131_fit$GammaStar,
                         Curve131_fit$fitmethod,
                         Curve131_fit$Tcorrect,
                         Curve131_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve131_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve131_output) # add the curve fits to the larger data frame

### Curve132_data
Curve132_data <- subset(aci.df, unique_id == aci.df.unique_id[132]) # find correct curve from full dataframe and make new object
plot(Curve132_data$A~Curve132_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve132_fit <- fitaci(Curve132_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve132_fit) # take a look at fitted values, adjust as needed
plot(Curve132_fit) # plot the fitted curves over the raw data, adjust as needed
Curve132_output <- cbind('Curve132', Curve132_data$id[1], Curve132_data$unique_id[1], Curve132_data$machine[1], Curve132_data$baseline_yn[1],
                         Curve132_data$A[1], Curve132_data$Ci[1], Curve132_data$gsw[1],
                         mean(Curve132_data$VPDleaf, na.rm = T), mean(Curve132_data$Tleaf, na.rm = T), mean(Curve132_data$Qin, na.rm = T),
                         Curve132_fit[[2]][1,1], Curve132_fit[[2]][1,2],
                         Curve132_fit[[2]][2,1], Curve132_fit[[2]][2,2],
                         Curve132_fit[[2]][3,1], Curve132_fit[[2]][3,2],
                         Curve132_fit$RMSE,
                         Curve132_fit$Ci_transition,
                         Curve132_fit$citransition,
                         Curve132_fit$Km,
                         Curve132_fit$GammaStar,
                         Curve132_fit$fitmethod,
                         Curve132_fit$Tcorrect,
                         Curve132_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve132_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve132_output) # add the curve fits to the larger data frame

### Curve133_data
Curve133_data <- subset(aci.df, unique_id == aci.df.unique_id[133]) # find correct curve from full dataframe and make new object
plot(Curve133_data$A~Curve133_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve133_fit <- fitaci(Curve133_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve133_fit) # take a look at fitted values, adjust as needed
plot(Curve133_fit) # plot the fitted curves over the raw data, adjust as needed
Curve133_output <- cbind('Curve133', Curve133_data$id[1], Curve133_data$unique_id[1], Curve133_data$machine[1], Curve133_data$baseline_yn[1],
                         Curve133_data$A[1], Curve133_data$Ci[1], Curve133_data$gsw[1],
                         mean(Curve133_data$VPDleaf, na.rm = T), mean(Curve133_data$Tleaf, na.rm = T), mean(Curve133_data$Qin, na.rm = T),
                         Curve133_fit[[2]][1,1], Curve133_fit[[2]][1,2],
                         Curve133_fit[[2]][2,1], Curve133_fit[[2]][2,2],
                         Curve133_fit[[2]][3,1], Curve133_fit[[2]][3,2],
                         Curve133_fit$RMSE,
                         Curve133_fit$Ci_transition,
                         Curve133_fit$citransition,
                         Curve133_fit$Km,
                         Curve133_fit$GammaStar,
                         Curve133_fit$fitmethod,
                         Curve133_fit$Tcorrect,
                         Curve133_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve133_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve133_output) # add the curve fits to the larger data frame

### Curve134_data
Curve134_data <- subset(aci.df, unique_id == aci.df.unique_id[134]) # find correct curve from full dataframe and make new object
plot(Curve134_data$A~Curve134_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve134_fit <- fitaci(Curve134_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve134_fit) # take a look at fitted values, adjust as needed
plot(Curve134_fit) # plot the fitted curves over the raw data, adjust as needed
Curve134_output <- cbind('Curve134', Curve134_data$id[1], Curve134_data$unique_id[1], Curve134_data$machine[1], Curve134_data$baseline_yn[1],
                         Curve134_data$A[1], Curve134_data$Ci[1], Curve134_data$gsw[1],
                         mean(Curve134_data$VPDleaf, na.rm = T), mean(Curve134_data$Tleaf, na.rm = T), mean(Curve134_data$Qin, na.rm = T),
                         Curve134_fit[[2]][1,1], Curve134_fit[[2]][1,2],
                         Curve134_fit[[2]][2,1], Curve134_fit[[2]][2,2],
                         Curve134_fit[[2]][3,1], Curve134_fit[[2]][3,2],
                         Curve134_fit$RMSE,
                         Curve134_fit$Ci_transition,
                         Curve134_fit$citransition,
                         Curve134_fit$Km,
                         Curve134_fit$GammaStar,
                         Curve134_fit$fitmethod,
                         Curve134_fit$Tcorrect,
                         Curve134_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve134_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve134_output) # add the curve fits to the larger data frame

### Curve135_data
Curve135_data <- subset(aci.df, unique_id == aci.df.unique_id[135]) # find correct curve from full dataframe and make new object
plot(Curve135_data$A~Curve135_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve135_fit <- fitaci(Curve135_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve135_fit) # take a look at fitted values, adjust as needed
plot(Curve135_fit) # plot the fitted curves over the raw data, adjust as needed
Curve135_output <- cbind('Curve135', Curve135_data$id[1], Curve135_data$unique_id[1], Curve135_data$machine[1], Curve135_data$baseline_yn[1],
                         Curve135_data$A[1], Curve135_data$Ci[1], Curve135_data$gsw[1],
                         mean(Curve135_data$VPDleaf, na.rm = T), mean(Curve135_data$Tleaf, na.rm = T), mean(Curve135_data$Qin, na.rm = T),
                         Curve135_fit[[2]][1,1], Curve135_fit[[2]][1,2],
                         Curve135_fit[[2]][2,1], Curve135_fit[[2]][2,2],
                         Curve135_fit[[2]][3,1], Curve135_fit[[2]][3,2],
                         Curve135_fit$RMSE,
                         Curve135_fit$Ci_transition,
                         Curve135_fit$citransition,
                         Curve135_fit$Km,
                         Curve135_fit$GammaStar,
                         Curve135_fit$fitmethod,
                         Curve135_fit$Tcorrect,
                         Curve135_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve135_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve135_output) # add the curve fits to the larger data frame

### Curve136_data
Curve136_data <- subset(aci.df, unique_id == aci.df.unique_id[136]) # find correct curve from full dataframe and make new object
plot(Curve136_data$A~Curve136_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve136_fit <- fitaci(Curve136_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve136_fit) # take a look at fitted values, adjust as needed
plot(Curve136_fit) # plot the fitted curves over the raw data, adjust as needed
Curve136_output <- cbind('Curve136', Curve136_data$id[1], Curve136_data$unique_id[1], Curve136_data$machine[1], Curve136_data$baseline_yn[1],
                         Curve136_data$A[1], Curve136_data$Ci[1], Curve136_data$gsw[1],
                         mean(Curve136_data$VPDleaf, na.rm = T), mean(Curve136_data$Tleaf, na.rm = T), mean(Curve136_data$Qin, na.rm = T),
                         Curve136_fit[[2]][1,1], Curve136_fit[[2]][1,2],
                         Curve136_fit[[2]][2,1], Curve136_fit[[2]][2,2],
                         Curve136_fit[[2]][3,1], Curve136_fit[[2]][3,2],
                         Curve136_fit$RMSE,
                         Curve136_fit$Ci_transition,
                         Curve136_fit$citransition,
                         Curve136_fit$Km,
                         Curve136_fit$GammaStar,
                         Curve136_fit$fitmethod,
                         Curve136_fit$Tcorrect,
                         Curve136_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve136_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve136_output) # add the curve fits to the larger data frame

### Curve137_data
Curve137_data <- subset(aci.df, unique_id == aci.df.unique_id[137]) # find correct curve from full dataframe and make new object
plot(Curve137_data$A~Curve137_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve137_fit <- fitaci(Curve137_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve137_fit) # take a look at fitted values, adjust as needed
plot(Curve137_fit) # plot the fitted curves over the raw data, adjust as needed
Curve137_output <- cbind('Curve137', Curve137_data$id[1], Curve137_data$unique_id[1], Curve137_data$machine[1], Curve137_data$baseline_yn[1],
                         Curve137_data$A[1], Curve137_data$Ci[1], Curve137_data$gsw[1],
                         mean(Curve137_data$VPDleaf, na.rm = T), mean(Curve137_data$Tleaf, na.rm = T), mean(Curve137_data$Qin, na.rm = T),
                         Curve137_fit[[2]][1,1], Curve137_fit[[2]][1,2],
                         Curve137_fit[[2]][2,1], Curve137_fit[[2]][2,2],
                         Curve137_fit[[2]][3,1], Curve137_fit[[2]][3,2],
                         Curve137_fit$RMSE,
                         Curve137_fit$Ci_transition,
                         Curve137_fit$citransition,
                         Curve137_fit$Km,
                         Curve137_fit$GammaStar,
                         Curve137_fit$fitmethod,
                         Curve137_fit$Tcorrect,
                         Curve137_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve137_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve137_output) # add the curve fits to the larger data frame

### Curve138_data
Curve138_data <- subset(aci.df, unique_id == aci.df.unique_id[138]) # find correct curve from full dataframe and make new object
plot(Curve138_data$A~Curve138_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve138_fit <- fitaci(Curve138_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve138_fit) # take a look at fitted values, adjust as needed
plot(Curve138_fit) # plot the fitted curves over the raw data, adjust as needed
Curve138_output <- cbind('Curve138', Curve138_data$id[1], Curve138_data$unique_id[1], Curve138_data$machine[1], Curve138_data$baseline_yn[1],
                         Curve138_data$A[1], Curve138_data$Ci[1], Curve138_data$gsw[1],
                         mean(Curve138_data$VPDleaf, na.rm = T), mean(Curve138_data$Tleaf, na.rm = T), mean(Curve138_data$Qin, na.rm = T),
                         Curve138_fit[[2]][1,1], Curve138_fit[[2]][1,2],
                         Curve138_fit[[2]][2,1], Curve138_fit[[2]][2,2],
                         Curve138_fit[[2]][3,1], Curve138_fit[[2]][3,2],
                         Curve138_fit$RMSE,
                         Curve138_fit$Ci_transition,
                         Curve138_fit$citransition,
                         Curve138_fit$Km,
                         Curve138_fit$GammaStar,
                         Curve138_fit$fitmethod,
                         Curve138_fit$Tcorrect,
                         Curve138_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve138_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve138_output) # add the curve fits to the larger data frame

### Curve139_data
Curve139_data <- subset(aci.df, unique_id == aci.df.unique_id[139] & Ci < 1000) # find correct curve from full dataframe and make new object
plot(Curve139_data$A~Curve139_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve139_fit <- fitaci(Curve139_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve139_fit) # take a look at fitted values, adjust as needed
plot(Curve139_fit) # plot the fitted curves over the raw data, adjust as needed
Curve139_output <- cbind('Curve139', Curve139_data$id[1], Curve139_data$unique_id[1], Curve139_data$machine[1], Curve139_data$baseline_yn[1],
                         Curve139_data$A[1], Curve139_data$Ci[1], Curve139_data$gsw[1],
                         mean(Curve139_data$VPDleaf, na.rm = T), mean(Curve139_data$Tleaf, na.rm = T), mean(Curve139_data$Qin, na.rm = T),
                         Curve139_fit[[2]][1,1], Curve139_fit[[2]][1,2],
                         Curve139_fit[[2]][2,1], Curve139_fit[[2]][2,2],
                         Curve139_fit[[2]][3,1], Curve139_fit[[2]][3,2],
                         Curve139_fit$RMSE,
                         Curve139_fit$Ci_transition,
                         Curve139_fit$citransition,
                         Curve139_fit$Km,
                         Curve139_fit$GammaStar,
                         Curve139_fit$fitmethod,
                         Curve139_fit$Tcorrect,
                         Curve139_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve139_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve139_output) # add the curve fits to the larger data frame

### Curve140_data
Curve140_data <- subset(aci.df, unique_id == aci.df.unique_id[140] & Ci < 1000) # find correct curve from full dataframe and make new object
plot(Curve140_data$A~Curve140_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve140_fit <- fitaci(Curve140_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve140_fit) # take a look at fitted values, adjust as needed
plot(Curve140_fit) # plot the fitted curves over the raw data, adjust as needed
Curve140_output <- cbind('Curve140', Curve140_data$id[1], Curve140_data$unique_id[1], Curve140_data$machine[1], Curve140_data$baseline_yn[1],
                         Curve140_data$A[1], Curve140_data$Ci[1], Curve140_data$gsw[1],
                         mean(Curve140_data$VPDleaf, na.rm = T), mean(Curve140_data$Tleaf, na.rm = T), mean(Curve140_data$Qin, na.rm = T),
                         Curve140_fit[[2]][1,1], Curve140_fit[[2]][1,2],
                         Curve140_fit[[2]][2,1], Curve140_fit[[2]][2,2],
                         Curve140_fit[[2]][3,1], Curve140_fit[[2]][3,2],
                         Curve140_fit$RMSE,
                         Curve140_fit$Ci_transition,
                         Curve140_fit$citransition,
                         Curve140_fit$Km,
                         Curve140_fit$GammaStar,
                         Curve140_fit$fitmethod,
                         Curve140_fit$Tcorrect,
                         Curve140_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve140_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve140_output) # add the curve fits to the larger data frame

### Curve141_data
Curve141_data <- subset(aci.df, unique_id == aci.df.unique_id[141]) # find correct curve from full dataframe and make new object
plot(Curve141_data$A~Curve141_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve141_fit <- fitaci(Curve141_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve141_fit) # take a look at fitted values, adjust as needed
plot(Curve141_fit) # plot the fitted curves over the raw data, adjust as needed
Curve141_output <- cbind('Curve141', Curve141_data$id[1], Curve141_data$unique_id[1], Curve141_data$machine[1], Curve141_data$baseline_yn[1],
                         Curve141_data$A[1], Curve141_data$Ci[1], Curve141_data$gsw[1],
                         mean(Curve141_data$VPDleaf, na.rm = T), mean(Curve141_data$Tleaf, na.rm = T), mean(Curve141_data$Qin, na.rm = T),
                         Curve141_fit[[2]][1,1], Curve141_fit[[2]][1,2],
                         Curve141_fit[[2]][2,1], Curve141_fit[[2]][2,2],
                         Curve141_fit[[2]][3,1], Curve141_fit[[2]][3,2],
                         Curve141_fit$RMSE,
                         Curve141_fit$Ci_transition,
                         Curve141_fit$citransition,
                         Curve141_fit$Km,
                         Curve141_fit$GammaStar,
                         Curve141_fit$fitmethod,
                         Curve141_fit$Tcorrect,
                         Curve141_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve141_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve141_output) # add the curve fits to the larger data frame

### Curve142_data
Curve142_data <- subset(aci.df, unique_id == aci.df.unique_id[142]) # find correct curve from full dataframe and make new object
plot(Curve142_data$A~Curve142_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve142_fit <- fitaci(Curve142_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve142_fit) # take a look at fitted values, adjust as needed
plot(Curve142_fit) # plot the fitted curves over the raw data, adjust as needed
Curve142_output <- cbind('Curve142', Curve142_data$id[1], Curve142_data$unique_id[1], Curve142_data$machine[1], Curve142_data$baseline_yn[1],
                         Curve142_data$A[1], Curve142_data$Ci[1], Curve142_data$gsw[1],
                         mean(Curve142_data$VPDleaf, na.rm = T), mean(Curve142_data$Tleaf, na.rm = T), mean(Curve142_data$Qin, na.rm = T),
                         Curve142_fit[[2]][1,1], Curve142_fit[[2]][1,2],
                         Curve142_fit[[2]][2,1], Curve142_fit[[2]][2,2],
                         Curve142_fit[[2]][3,1], Curve142_fit[[2]][3,2],
                         Curve142_fit$RMSE,
                         Curve142_fit$Ci_transition,
                         Curve142_fit$citransition,
                         Curve142_fit$Km,
                         Curve142_fit$GammaStar,
                         Curve142_fit$fitmethod,
                         Curve142_fit$Tcorrect,
                         Curve142_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve142_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve142_output) # add the curve fits to the larger data frame

### Curve143_data
Curve143_data <- subset(aci.df, unique_id == aci.df.unique_id[143]) # find correct curve from full dataframe and make new object
plot(Curve143_data$A~Curve143_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve143_fit <- fitaci(Curve143_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve143_fit) # take a look at fitted values, adjust as needed
plot(Curve143_fit) # plot the fitted curves over the raw data, adjust as needed
Curve143_output <- cbind('Curve143', Curve143_data$id[1], Curve143_data$unique_id[1], Curve143_data$machine[1], Curve143_data$baseline_yn[1],
                         Curve143_data$A[1], Curve143_data$Ci[1], Curve143_data$gsw[1],
                         mean(Curve143_data$VPDleaf, na.rm = T), mean(Curve143_data$Tleaf, na.rm = T), mean(Curve143_data$Qin, na.rm = T),
                         Curve143_fit[[2]][1,1], Curve143_fit[[2]][1,2],
                         Curve143_fit[[2]][2,1], Curve143_fit[[2]][2,2],
                         Curve143_fit[[2]][3,1], Curve143_fit[[2]][3,2],
                         Curve143_fit$RMSE,
                         Curve143_fit$Ci_transition,
                         Curve143_fit$citransition,
                         Curve143_fit$Km,
                         Curve143_fit$GammaStar,
                         Curve143_fit$fitmethod,
                         Curve143_fit$Tcorrect,
                         Curve143_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve143_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve143_output) # add the curve fits to the larger data frame

### Curve144_data
Curve144_data <- subset(aci.df, unique_id == aci.df.unique_id[144]) # find correct curve from full dataframe and make new object
plot(Curve144_data$A~Curve144_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve144_fit <- fitaci(Curve144_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve144_fit) # take a look at fitted values, adjust as needed
plot(Curve144_fit) # plot the fitted curves over the raw data, adjust as needed
Curve144_output <- cbind('Curve144', Curve144_data$id[1], Curve144_data$unique_id[1], Curve144_data$machine[1], Curve144_data$baseline_yn[1],
                         Curve144_data$A[1], Curve144_data$Ci[1], Curve144_data$gsw[1],
                         mean(Curve144_data$VPDleaf, na.rm = T), mean(Curve144_data$Tleaf, na.rm = T), mean(Curve144_data$Qin, na.rm = T),
                         Curve144_fit[[2]][1,1], Curve144_fit[[2]][1,2],
                         Curve144_fit[[2]][2,1], Curve144_fit[[2]][2,2],
                         Curve144_fit[[2]][3,1], Curve144_fit[[2]][3,2],
                         Curve144_fit$RMSE,
                         Curve144_fit$Ci_transition,
                         Curve144_fit$citransition,
                         Curve144_fit$Km,
                         Curve144_fit$GammaStar,
                         Curve144_fit$fitmethod,
                         Curve144_fit$Tcorrect,
                         Curve144_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve144_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve144_output) # add the curve fits to the larger data frame

### Curve145_data
Curve145_data <- subset(aci.df, unique_id == aci.df.unique_id[145]) # find correct curve from full dataframe and make new object
plot(Curve145_data$A~Curve145_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve145_fit <- fitaci(Curve145_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve145_fit) # take a look at fitted values, adjust as needed
plot(Curve145_fit) # plot the fitted curves over the raw data, adjust as needed
Curve145_output <- cbind('Curve145', Curve145_data$id[1], Curve145_data$unique_id[1], Curve145_data$machine[1], Curve145_data$baseline_yn[1],
                         Curve145_data$A[1], Curve145_data$Ci[1], Curve145_data$gsw[1],
                         mean(Curve145_data$VPDleaf, na.rm = T), mean(Curve145_data$Tleaf, na.rm = T), mean(Curve145_data$Qin, na.rm = T),
                         Curve145_fit[[2]][1,1], Curve145_fit[[2]][1,2],
                         Curve145_fit[[2]][2,1], Curve145_fit[[2]][2,2],
                         Curve145_fit[[2]][3,1], Curve145_fit[[2]][3,2],
                         Curve145_fit$RMSE,
                         Curve145_fit$Ci_transition,
                         Curve145_fit$citransition,
                         Curve145_fit$Km,
                         Curve145_fit$GammaStar,
                         Curve145_fit$fitmethod,
                         Curve145_fit$Tcorrect,
                         Curve145_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve145_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve145_output) # add the curve fits to the larger data frame

### Curve146_data
Curve146_data <- subset(aci.df, unique_id == aci.df.unique_id[146]) # find correct curve from full dataframe and make new object
plot(Curve146_data$A~Curve146_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve146_fit <- fitaci(Curve146_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve146_fit) # take a look at fitted values, adjust as needed
plot(Curve146_fit) # plot the fitted curves over the raw data, adjust as needed
Curve146_output <- cbind('Curve146', Curve146_data$id[1], Curve146_data$unique_id[1], Curve146_data$machine[1], Curve146_data$baseline_yn[1],
                         Curve146_data$A[1], Curve146_data$Ci[1], Curve146_data$gsw[1],
                         mean(Curve146_data$VPDleaf, na.rm = T), mean(Curve146_data$Tleaf, na.rm = T), mean(Curve146_data$Qin, na.rm = T),
                         Curve146_fit[[2]][1,1], Curve146_fit[[2]][1,2],
                         Curve146_fit[[2]][2,1], Curve146_fit[[2]][2,2],
                         Curve146_fit[[2]][3,1], Curve146_fit[[2]][3,2],
                         Curve146_fit$RMSE,
                         Curve146_fit$Ci_transition,
                         Curve146_fit$citransition,
                         Curve146_fit$Km,
                         Curve146_fit$GammaStar,
                         Curve146_fit$fitmethod,
                         Curve146_fit$Tcorrect,
                         Curve146_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve146_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve146_output) # add the curve fits to the larger data frame

### Curve147_data
Curve147_data <- subset(aci.df, unique_id == aci.df.unique_id[147] & Ci < 1000) # find correct curve from full dataframe and make new object
plot(Curve147_data$A~Curve147_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve147_fit <- fitaci(Curve147_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve147_fit) # take a look at fitted values, adjust as needed
plot(Curve147_fit) # plot the fitted curves over the raw data, adjust as needed
Curve147_output <- cbind('Curve147', Curve147_data$id[1], Curve147_data$unique_id[1], Curve147_data$machine[1], Curve147_data$baseline_yn[1],
                         Curve147_data$A[1], Curve147_data$Ci[1], Curve147_data$gsw[1],
                         mean(Curve147_data$VPDleaf, na.rm = T), mean(Curve147_data$Tleaf, na.rm = T), mean(Curve147_data$Qin, na.rm = T),
                         Curve147_fit[[2]][1,1], Curve147_fit[[2]][1,2],
                         Curve147_fit[[2]][2,1], Curve147_fit[[2]][2,2],
                         Curve147_fit[[2]][3,1], Curve147_fit[[2]][3,2],
                         Curve147_fit$RMSE,
                         Curve147_fit$Ci_transition,
                         Curve147_fit$citransition,
                         Curve147_fit$Km,
                         Curve147_fit$GammaStar,
                         Curve147_fit$fitmethod,
                         Curve147_fit$Tcorrect,
                         Curve147_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve147_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve147_output) # add the curve fits to the larger data frame

### Curve148_data
Curve148_data <- subset(aci.df, unique_id == aci.df.unique_id[148] & Ci < 1000) # find correct curve from full dataframe and make new object
plot(Curve148_data$A~Curve148_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve148_fit <- fitaci(Curve148_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve148_fit) # take a look at fitted values, adjust as needed
plot(Curve148_fit) # plot the fitted curves over the raw data, adjust as needed
Curve148_output <- cbind('Curve148', Curve148_data$id[1], Curve148_data$unique_id[1], Curve148_data$machine[1], Curve148_data$baseline_yn[1],
                         Curve148_data$A[1], Curve148_data$Ci[1], Curve148_data$gsw[1],
                         mean(Curve148_data$VPDleaf, na.rm = T), mean(Curve148_data$Tleaf, na.rm = T), mean(Curve148_data$Qin, na.rm = T),
                         Curve148_fit[[2]][1,1], Curve148_fit[[2]][1,2],
                         Curve148_fit[[2]][2,1], Curve148_fit[[2]][2,2],
                         Curve148_fit[[2]][3,1], Curve148_fit[[2]][3,2],
                         Curve148_fit$RMSE,
                         Curve148_fit$Ci_transition,
                         Curve148_fit$citransition,
                         Curve148_fit$Km,
                         Curve148_fit$GammaStar,
                         Curve148_fit$fitmethod,
                         Curve148_fit$Tcorrect,
                         Curve148_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve148_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve148_output) # add the curve fits to the larger data frame

### Curve149_data
Curve149_data <- subset(aci.df, unique_id == aci.df.unique_id[149] & Ci < 1000) # find correct curve from full dataframe and make new object
plot(Curve149_data$A~Curve149_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve149_fit <- fitaci(Curve149_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve149_fit) # take a look at fitted values, adjust as needed
plot(Curve149_fit) # plot the fitted curves over the raw data, adjust as needed
Curve149_output <- cbind('Curve149', Curve149_data$id[1], Curve149_data$unique_id[1], Curve149_data$machine[1], Curve149_data$baseline_yn[1],
                         Curve149_data$A[1], Curve149_data$Ci[1], Curve149_data$gsw[1],
                         mean(Curve149_data$VPDleaf, na.rm = T), mean(Curve149_data$Tleaf, na.rm = T), mean(Curve149_data$Qin, na.rm = T),
                         Curve149_fit[[2]][1,1], Curve149_fit[[2]][1,2],
                         Curve149_fit[[2]][2,1], Curve149_fit[[2]][2,2],
                         Curve149_fit[[2]][3,1], Curve149_fit[[2]][3,2],
                         Curve149_fit$RMSE,
                         Curve149_fit$Ci_transition,
                         Curve149_fit$citransition,
                         Curve149_fit$Km,
                         Curve149_fit$GammaStar,
                         Curve149_fit$fitmethod,
                         Curve149_fit$Tcorrect,
                         Curve149_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve149_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve149_output) # add the curve fits to the larger data frame

### Curve150_data
Curve150_data <- subset(aci.df, unique_id == aci.df.unique_id[150]) # find correct curve from full dataframe and make new object
plot(Curve150_data$A~Curve150_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve150_fit <- fitaci(Curve150_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve150_fit) # take a look at fitted values, adjust as needed
plot(Curve150_fit) # plot the fitted curves over the raw data, adjust as needed
Curve150_output <- cbind('Curve150', Curve150_data$id[1], Curve150_data$unique_id[1], Curve150_data$machine[1], Curve150_data$baseline_yn[1],
                         Curve150_data$A[1], Curve150_data$Ci[1], Curve150_data$gsw[1],
                         mean(Curve150_data$VPDleaf, na.rm = T), mean(Curve150_data$Tleaf, na.rm = T), mean(Curve150_data$Qin, na.rm = T),
                         Curve150_fit[[2]][1,1], Curve150_fit[[2]][1,2],
                         Curve150_fit[[2]][2,1], Curve150_fit[[2]][2,2],
                         Curve150_fit[[2]][3,1], Curve150_fit[[2]][3,2],
                         Curve150_fit$RMSE,
                         Curve150_fit$Ci_transition,
                         Curve150_fit$citransition,
                         Curve150_fit$Km,
                         Curve150_fit$GammaStar,
                         Curve150_fit$fitmethod,
                         Curve150_fit$Tcorrect,
                         Curve150_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve150_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve150_output) # add the curve fits to the larger data frame

### Curve151_data
Curve151_data <- subset(aci.df, unique_id == aci.df.unique_id[151]) # find correct curve from full dataframe and make new object
plot(Curve151_data$A~Curve151_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve151_fit <- fitaci(Curve151_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve151_fit) # take a look at fitted values, adjust as needed
plot(Curve151_fit) # plot the fitted curves over the raw data, adjust as needed
Curve151_output <- cbind('Curve151', Curve151_data$id[1], Curve151_data$unique_id[1], Curve151_data$machine[1], Curve151_data$baseline_yn[1],
                         Curve151_data$A[1], Curve151_data$Ci[1], Curve151_data$gsw[1],
                         mean(Curve151_data$VPDleaf, na.rm = T), mean(Curve151_data$Tleaf, na.rm = T), mean(Curve151_data$Qin, na.rm = T),
                         Curve151_fit[[2]][1,1], Curve151_fit[[2]][1,2],
                         Curve151_fit[[2]][2,1], Curve151_fit[[2]][2,2],
                         Curve151_fit[[2]][3,1], Curve151_fit[[2]][3,2],
                         Curve151_fit$RMSE,
                         Curve151_fit$Ci_transition,
                         Curve151_fit$citransition,
                         Curve151_fit$Km,
                         Curve151_fit$GammaStar,
                         Curve151_fit$fitmethod,
                         Curve151_fit$Tcorrect,
                         Curve151_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve151_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve151_output) # add the curve fits to the larger data frame

### Curve152_data
Curve152_data <- subset(aci.df, unique_id == aci.df.unique_id[152]) # find correct curve from full dataframe and make new object
plot(Curve152_data$A~Curve152_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve152_fit <- fitaci(Curve152_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve152_fit) # take a look at fitted values, adjust as needed
plot(Curve152_fit) # plot the fitted curves over the raw data, adjust as needed
Curve152_output <- cbind('Curve152', Curve152_data$id[1], Curve152_data$unique_id[1], Curve152_data$machine[1], Curve152_data$baseline_yn[1],
                         Curve152_data$A[1], Curve152_data$Ci[1], Curve152_data$gsw[1],
                         mean(Curve152_data$VPDleaf, na.rm = T), mean(Curve152_data$Tleaf, na.rm = T), mean(Curve152_data$Qin, na.rm = T),
                         Curve152_fit[[2]][1,1], Curve152_fit[[2]][1,2],
                         Curve152_fit[[2]][2,1], Curve152_fit[[2]][2,2],
                         Curve152_fit[[2]][3,1], Curve152_fit[[2]][3,2],
                         Curve152_fit$RMSE,
                         Curve152_fit$Ci_transition,
                         Curve152_fit$citransition,
                         Curve152_fit$Km,
                         Curve152_fit$GammaStar,
                         Curve152_fit$fitmethod,
                         Curve152_fit$Tcorrect,
                         Curve152_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve152_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve152_output) # add the curve fits to the larger data frame

### Curve153_data
Curve153_data <- subset(aci.df, unique_id == aci.df.unique_id[153]) # find correct curve from full dataframe and make new object
plot(Curve153_data$A~Curve153_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve153_fit <- fitaci(Curve153_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve153_fit) # take a look at fitted values, adjust as needed
plot(Curve153_fit) # plot the fitted curves over the raw data, adjust as needed
Curve153_output <- cbind('Curve153', Curve153_data$id[1], Curve153_data$unique_id[1], Curve153_data$machine[1], Curve153_data$baseline_yn[1],
                         Curve153_data$A[1], Curve153_data$Ci[1], Curve153_data$gsw[1],
                         mean(Curve153_data$VPDleaf, na.rm = T), mean(Curve153_data$Tleaf, na.rm = T), mean(Curve153_data$Qin, na.rm = T),
                         Curve153_fit[[2]][1,1], Curve153_fit[[2]][1,2],
                         Curve153_fit[[2]][2,1], Curve153_fit[[2]][2,2],
                         Curve153_fit[[2]][3,1], Curve153_fit[[2]][3,2],
                         Curve153_fit$RMSE,
                         Curve153_fit$Ci_transition,
                         Curve153_fit$citransition,
                         Curve153_fit$Km,
                         Curve153_fit$GammaStar,
                         Curve153_fit$fitmethod,
                         Curve153_fit$Tcorrect,
                         Curve153_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve153_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve153_output) # add the curve fits to the larger data frame

### Curve154_data
Curve154_data <- subset(aci.df, unique_id == aci.df.unique_id[154]) # find correct curve from full dataframe and make new object
plot(Curve154_data$A~Curve154_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve154_fit <- fitaci(Curve154_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve154_fit) # take a look at fitted values, adjust as needed
plot(Curve154_fit) # plot the fitted curves over the raw data, adjust as needed
Curve154_output <- cbind('Curve154', Curve154_data$id[1], Curve154_data$unique_id[1], Curve154_data$machine[1], Curve154_data$baseline_yn[1],
                         Curve154_data$A[1], Curve154_data$Ci[1], Curve154_data$gsw[1],
                         mean(Curve154_data$VPDleaf, na.rm = T), mean(Curve154_data$Tleaf, na.rm = T), mean(Curve154_data$Qin, na.rm = T),
                         Curve154_fit[[2]][1,1], Curve154_fit[[2]][1,2],
                         Curve154_fit[[2]][2,1], Curve154_fit[[2]][2,2],
                         Curve154_fit[[2]][3,1], Curve154_fit[[2]][3,2],
                         Curve154_fit$RMSE,
                         Curve154_fit$Ci_transition,
                         Curve154_fit$citransition,
                         Curve154_fit$Km,
                         Curve154_fit$GammaStar,
                         Curve154_fit$fitmethod,
                         Curve154_fit$Tcorrect,
                         Curve154_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve154_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve154_output) # add the curve fits to the larger data frame

### Curve155_data
Curve155_data <- subset(aci.df, unique_id == aci.df.unique_id[155]) # find correct curve from full dataframe and make new object
plot(Curve155_data$A~Curve155_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve155_fit <- fitaci(Curve155_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve155_fit) # take a look at fitted values, adjust as needed
plot(Curve155_fit) # plot the fitted curves over the raw data, adjust as needed
Curve155_output <- cbind('Curve155', Curve155_data$id[1], Curve155_data$unique_id[1], Curve155_data$machine[1], Curve155_data$baseline_yn[1],
                         Curve155_data$A[1], Curve155_data$Ci[1], Curve155_data$gsw[1],
                         mean(Curve155_data$VPDleaf, na.rm = T), mean(Curve155_data$Tleaf, na.rm = T), mean(Curve155_data$Qin, na.rm = T),
                         Curve155_fit[[2]][1,1], Curve155_fit[[2]][1,2],
                         Curve155_fit[[2]][2,1], Curve155_fit[[2]][2,2],
                         Curve155_fit[[2]][3,1], Curve155_fit[[2]][3,2],
                         Curve155_fit$RMSE,
                         Curve155_fit$Ci_transition,
                         Curve155_fit$citransition,
                         Curve155_fit$Km,
                         Curve155_fit$GammaStar,
                         Curve155_fit$fitmethod,
                         Curve155_fit$Tcorrect,
                         Curve155_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve155_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve155_output) # add the curve fits to the larger data frame

### Curve156_data
Curve156_data <- subset(aci.df, unique_id == aci.df.unique_id[156]) # find correct curve from full dataframe and make new object
plot(Curve156_data$A~Curve156_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve156_fit <- fitaci(Curve156_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve156_fit) # take a look at fitted values, adjust as needed
plot(Curve156_fit) # plot the fitted curves over the raw data, adjust as needed
Curve156_output <- cbind('Curve156', Curve156_data$id[1], Curve156_data$unique_id[1], Curve156_data$machine[1], Curve156_data$baseline_yn[1],
                         Curve156_data$A[1], Curve156_data$Ci[1], Curve156_data$gsw[1],
                         mean(Curve156_data$VPDleaf, na.rm = T), mean(Curve156_data$Tleaf, na.rm = T), mean(Curve156_data$Qin, na.rm = T),
                         Curve156_fit[[2]][1,1], Curve156_fit[[2]][1,2],
                         Curve156_fit[[2]][2,1], Curve156_fit[[2]][2,2],
                         Curve156_fit[[2]][3,1], Curve156_fit[[2]][3,2],
                         Curve156_fit$RMSE,
                         Curve156_fit$Ci_transition,
                         Curve156_fit$citransition,
                         Curve156_fit$Km,
                         Curve156_fit$GammaStar,
                         Curve156_fit$fitmethod,
                         Curve156_fit$Tcorrect,
                         Curve156_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve156_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve156_output) # add the curve fits to the larger data frame

### Curve157_data
Curve157_data <- subset(aci.df, unique_id == aci.df.unique_id[157]) # find correct curve from full dataframe and make new object
plot(Curve157_data$A~Curve157_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve157_fit <- fitaci(Curve157_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve157_fit) # take a look at fitted values, adjust as needed
plot(Curve157_fit) # plot the fitted curves over the raw data, adjust as needed
Curve157_output <- cbind('Curve157', Curve157_data$id[1], Curve157_data$unique_id[1], Curve157_data$machine[1], Curve157_data$baseline_yn[1],
                         Curve157_data$A[1], Curve157_data$Ci[1], Curve157_data$gsw[1],
                         mean(Curve157_data$VPDleaf, na.rm = T), mean(Curve157_data$Tleaf, na.rm = T), mean(Curve157_data$Qin, na.rm = T),
                         Curve157_fit[[2]][1,1], Curve157_fit[[2]][1,2],
                         Curve157_fit[[2]][2,1], Curve157_fit[[2]][2,2],
                         Curve157_fit[[2]][3,1], Curve157_fit[[2]][3,2],
                         Curve157_fit$RMSE,
                         Curve157_fit$Ci_transition,
                         Curve157_fit$citransition,
                         Curve157_fit$Km,
                         Curve157_fit$GammaStar,
                         Curve157_fit$fitmethod,
                         Curve157_fit$Tcorrect,
                         Curve157_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve157_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve157_output) # add the curve fits to the larger data frame

### Curve158_data
Curve158_data <- subset(aci.df, unique_id == aci.df.unique_id[158] & Ci < 1000) # find correct curve from full dataframe and make new object
plot(Curve158_data$A~Curve158_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve158_fit <- fitaci(Curve158_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve158_fit) # take a look at fitted values, adjust as needed
plot(Curve158_fit) # plot the fitted curves over the raw data, adjust as needed
Curve158_output <- cbind('Curve158', Curve158_data$id[1], Curve158_data$unique_id[1], Curve158_data$machine[1], Curve158_data$baseline_yn[1],
                         Curve158_data$A[1], Curve158_data$Ci[1], Curve158_data$gsw[1],
                         mean(Curve158_data$VPDleaf, na.rm = T), mean(Curve158_data$Tleaf, na.rm = T), mean(Curve158_data$Qin, na.rm = T),
                         Curve158_fit[[2]][1,1], Curve158_fit[[2]][1,2],
                         Curve158_fit[[2]][2,1], Curve158_fit[[2]][2,2],
                         Curve158_fit[[2]][3,1], Curve158_fit[[2]][3,2],
                         Curve158_fit$RMSE,
                         Curve158_fit$Ci_transition,
                         Curve158_fit$citransition,
                         Curve158_fit$Km,
                         Curve158_fit$GammaStar,
                         Curve158_fit$fitmethod,
                         Curve158_fit$Tcorrect,
                         Curve158_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve158_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve158_output) # add the curve fits to the larger data frame

### Curve159_data
Curve159_data <- subset(aci.df, unique_id == aci.df.unique_id[159]) # find correct curve from full dataframe and make new object
plot(Curve159_data$A~Curve159_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve159_fit <- fitaci(Curve159_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve159_fit) # take a look at fitted values, adjust as needed
plot(Curve159_fit) # plot the fitted curves over the raw data, adjust as needed
Curve159_output <- cbind('Curve159', Curve159_data$id[1], Curve159_data$unique_id[1], Curve159_data$machine[1], Curve159_data$baseline_yn[1],
                         Curve159_data$A[1], Curve159_data$Ci[1], Curve159_data$gsw[1],
                         mean(Curve159_data$VPDleaf, na.rm = T), mean(Curve159_data$Tleaf, na.rm = T), mean(Curve159_data$Qin, na.rm = T),
                         Curve159_fit[[2]][1,1], Curve159_fit[[2]][1,2],
                         Curve159_fit[[2]][2,1], Curve159_fit[[2]][2,2],
                         Curve159_fit[[2]][3,1], Curve159_fit[[2]][3,2],
                         Curve159_fit$RMSE,
                         Curve159_fit$Ci_transition,
                         Curve159_fit$citransition,
                         Curve159_fit$Km,
                         Curve159_fit$GammaStar,
                         Curve159_fit$fitmethod,
                         Curve159_fit$Tcorrect,
                         Curve159_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve159_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve159_output) # add the curve fits to the larger data frame

### Curve160_data
Curve160_data <- subset(aci.df, unique_id == aci.df.unique_id[160]) # find correct curve from full dataframe and make new object
plot(Curve160_data$A~Curve160_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve160_fit <- fitaci(Curve160_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve160_fit) # take a look at fitted values, adjust as needed
plot(Curve160_fit) # plot the fitted curves over the raw data, adjust as needed
Curve160_output <- cbind('Curve160', Curve160_data$id[1], Curve160_data$unique_id[1], Curve160_data$machine[1], Curve160_data$baseline_yn[1],
                         Curve160_data$A[1], Curve160_data$Ci[1], Curve160_data$gsw[1],
                         mean(Curve160_data$VPDleaf, na.rm = T), mean(Curve160_data$Tleaf, na.rm = T), mean(Curve160_data$Qin, na.rm = T),
                         Curve160_fit[[2]][1,1], Curve160_fit[[2]][1,2],
                         Curve160_fit[[2]][2,1], Curve160_fit[[2]][2,2],
                         Curve160_fit[[2]][3,1], Curve160_fit[[2]][3,2],
                         Curve160_fit$RMSE,
                         Curve160_fit$Ci_transition,
                         Curve160_fit$citransition,
                         Curve160_fit$Km,
                         Curve160_fit$GammaStar,
                         Curve160_fit$fitmethod,
                         Curve160_fit$Tcorrect,
                         Curve160_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve160_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve160_output) # add the curve fits to the larger data frame

### Curve161_data
Curve161_data <- subset(aci.df, unique_id == aci.df.unique_id[161]) # find correct curve from full dataframe and make new object
plot(Curve161_data$A~Curve161_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve161_fit <- fitaci(Curve161_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve161_fit) # take a look at fitted values, adjust as needed
plot(Curve161_fit) # plot the fitted curves over the raw data, adjust as needed
Curve161_output <- cbind('Curve161', Curve161_data$id[1], Curve161_data$unique_id[1], Curve161_data$machine[1], Curve161_data$baseline_yn[1],
                         Curve161_data$A[1], Curve161_data$Ci[1], Curve161_data$gsw[1],
                         mean(Curve161_data$VPDleaf, na.rm = T), mean(Curve161_data$Tleaf, na.rm = T), mean(Curve161_data$Qin, na.rm = T),
                         Curve161_fit[[2]][1,1], Curve161_fit[[2]][1,2],
                         Curve161_fit[[2]][2,1], Curve161_fit[[2]][2,2],
                         Curve161_fit[[2]][3,1], Curve161_fit[[2]][3,2],
                         Curve161_fit$RMSE,
                         Curve161_fit$Ci_transition,
                         Curve161_fit$citransition,
                         Curve161_fit$Km,
                         Curve161_fit$GammaStar,
                         Curve161_fit$fitmethod,
                         Curve161_fit$Tcorrect,
                         Curve161_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve161_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve161_output) # add the curve fits to the larger data frame

### Curve162_data
Curve162_data <- subset(aci.df, unique_id == aci.df.unique_id[162]) # find correct curve from full dataframe and make new object
plot(Curve162_data$A~Curve162_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve162_fit <- fitaci(Curve162_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve162_fit) # take a look at fitted values, adjust as needed
plot(Curve162_fit) # plot the fitted curves over the raw data, adjust as needed
Curve162_output <- cbind('Curve162', Curve162_data$id[1], Curve162_data$unique_id[1], Curve162_data$machine[1], Curve162_data$baseline_yn[1],
                         Curve162_data$A[1], Curve162_data$Ci[1], Curve162_data$gsw[1],
                         mean(Curve162_data$VPDleaf, na.rm = T), mean(Curve162_data$Tleaf, na.rm = T), mean(Curve162_data$Qin, na.rm = T),
                         Curve162_fit[[2]][1,1], Curve162_fit[[2]][1,2],
                         Curve162_fit[[2]][2,1], Curve162_fit[[2]][2,2],
                         Curve162_fit[[2]][3,1], Curve162_fit[[2]][3,2],
                         Curve162_fit$RMSE,
                         Curve162_fit$Ci_transition,
                         Curve162_fit$citransition,
                         Curve162_fit$Km,
                         Curve162_fit$GammaStar,
                         Curve162_fit$fitmethod,
                         Curve162_fit$Tcorrect,
                         Curve162_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve162_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve162_output) # add the curve fits to the larger data frame

### Curve163_data
Curve163_data <- subset(aci.df, unique_id == aci.df.unique_id[163]) # find correct curve from full dataframe and make new object
plot(Curve163_data$A~Curve163_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve163_fit <- fitaci(Curve163_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve163_fit) # take a look at fitted values, adjust as needed
plot(Curve163_fit) # plot the fitted curves over the raw data, adjust as needed
Curve163_output <- cbind('Curve163', Curve163_data$id[1], Curve163_data$unique_id[1], Curve163_data$machine[1], Curve163_data$baseline_yn[1],
                         Curve163_data$A[1], Curve163_data$Ci[1], Curve163_data$gsw[1],
                         mean(Curve163_data$VPDleaf, na.rm = T), mean(Curve163_data$Tleaf, na.rm = T), mean(Curve163_data$Qin, na.rm = T),
                         Curve163_fit[[2]][1,1], Curve163_fit[[2]][1,2],
                         Curve163_fit[[2]][2,1], Curve163_fit[[2]][2,2],
                         Curve163_fit[[2]][3,1], Curve163_fit[[2]][3,2],
                         Curve163_fit$RMSE,
                         Curve163_fit$Ci_transition,
                         Curve163_fit$citransition,
                         Curve163_fit$Km,
                         Curve163_fit$GammaStar,
                         Curve163_fit$fitmethod,
                         Curve163_fit$Tcorrect,
                         Curve163_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve163_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve163_output) # add the curve fits to the larger data frame

### Curve164_data
Curve164_data <- subset(aci.df, unique_id == aci.df.unique_id[164]) # find correct curve from full dataframe and make new object
plot(Curve164_data$A~Curve164_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve164_fit <- fitaci(Curve164_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve164_fit) # take a look at fitted values, adjust as needed
plot(Curve164_fit) # plot the fitted curves over the raw data, adjust as needed
Curve164_output <- cbind('Curve164', Curve164_data$id[1], Curve164_data$unique_id[1], Curve164_data$machine[1], Curve164_data$baseline_yn[1],
                         Curve164_data$A[1], Curve164_data$Ci[1], Curve164_data$gsw[1],
                         mean(Curve164_data$VPDleaf, na.rm = T), mean(Curve164_data$Tleaf, na.rm = T), mean(Curve164_data$Qin, na.rm = T),
                         Curve164_fit[[2]][1,1], Curve164_fit[[2]][1,2],
                         Curve164_fit[[2]][2,1], Curve164_fit[[2]][2,2],
                         Curve164_fit[[2]][3,1], Curve164_fit[[2]][3,2],
                         Curve164_fit$RMSE,
                         Curve164_fit$Ci_transition,
                         Curve164_fit$citransition,
                         Curve164_fit$Km,
                         Curve164_fit$GammaStar,
                         Curve164_fit$fitmethod,
                         Curve164_fit$Tcorrect,
                         Curve164_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve164_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve164_output) # add the curve fits to the larger data frame

### Curve165_data
Curve165_data <- subset(aci.df, unique_id == aci.df.unique_id[165] & Ci < 1000) # find correct curve from full dataframe and make new object
plot(Curve165_data$A~Curve165_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve165_fit <- fitaci(Curve165_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve165_fit) # take a look at fitted values, adjust as needed
plot(Curve165_fit) # plot the fitted curves over the raw data, adjust as needed
Curve165_output <- cbind('Curve165', Curve165_data$id[1], Curve165_data$unique_id[1], Curve165_data$machine[1], Curve165_data$baseline_yn[1],
                         Curve165_data$A[1], Curve165_data$Ci[1], Curve165_data$gsw[1],
                         mean(Curve165_data$VPDleaf, na.rm = T), mean(Curve165_data$Tleaf, na.rm = T), mean(Curve165_data$Qin, na.rm = T),
                         Curve165_fit[[2]][1,1], Curve165_fit[[2]][1,2],
                         Curve165_fit[[2]][2,1], Curve165_fit[[2]][2,2],
                         Curve165_fit[[2]][3,1], Curve165_fit[[2]][3,2],
                         Curve165_fit$RMSE,
                         Curve165_fit$Ci_transition,
                         Curve165_fit$citransition,
                         Curve165_fit$Km,
                         Curve165_fit$GammaStar,
                         Curve165_fit$fitmethod,
                         Curve165_fit$Tcorrect,
                         Curve165_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve165_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve165_output) # add the curve fits to the larger data frame

### Curve166_data
Curve166_data <- subset(aci.df, unique_id == aci.df.unique_id[166]) # find correct curve from full dataframe and make new object
plot(Curve166_data$A~Curve166_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve166_fit <- fitaci(Curve166_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve166_fit) # take a look at fitted values, adjust as needed
plot(Curve166_fit) # plot the fitted curves over the raw data, adjust as needed
Curve166_output <- cbind('Curve166', Curve166_data$id[1], Curve166_data$unique_id[1], Curve166_data$machine[1], Curve166_data$baseline_yn[1],
                         Curve166_data$A[1], Curve166_data$Ci[1], Curve166_data$gsw[1],
                         mean(Curve166_data$VPDleaf, na.rm = T), mean(Curve166_data$Tleaf, na.rm = T), mean(Curve166_data$Qin, na.rm = T),
                         Curve166_fit[[2]][1,1], Curve166_fit[[2]][1,2],
                         Curve166_fit[[2]][2,1], Curve166_fit[[2]][2,2],
                         Curve166_fit[[2]][3,1], Curve166_fit[[2]][3,2],
                         Curve166_fit$RMSE,
                         Curve166_fit$Ci_transition,
                         Curve166_fit$citransition,
                         Curve166_fit$Km,
                         Curve166_fit$GammaStar,
                         Curve166_fit$fitmethod,
                         Curve166_fit$Tcorrect,
                         Curve166_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve166_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve166_output) # add the curve fits to the larger data frame

### Curve167_data
Curve167_data <- subset(aci.df, unique_id == aci.df.unique_id[167] & Ci < 1000) # find correct curve from full dataframe and make new object
plot(Curve167_data$A~Curve167_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve167_fit <- fitaci(Curve167_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve167_fit) # take a look at fitted values, adjust as needed
plot(Curve167_fit) # plot the fitted curves over the raw data, adjust as needed
Curve167_output <- cbind('Curve167', Curve167_data$id[1], Curve167_data$unique_id[1], Curve167_data$machine[1], Curve167_data$baseline_yn[1],
                         Curve167_data$A[1], Curve167_data$Ci[1], Curve167_data$gsw[1],
                         mean(Curve167_data$VPDleaf, na.rm = T), mean(Curve167_data$Tleaf, na.rm = T), mean(Curve167_data$Qin, na.rm = T),
                         Curve167_fit[[2]][1,1], Curve167_fit[[2]][1,2],
                         Curve167_fit[[2]][2,1], Curve167_fit[[2]][2,2],
                         Curve167_fit[[2]][3,1], Curve167_fit[[2]][3,2],
                         Curve167_fit$RMSE,
                         Curve167_fit$Ci_transition,
                         Curve167_fit$citransition,
                         Curve167_fit$Km,
                         Curve167_fit$GammaStar,
                         Curve167_fit$fitmethod,
                         Curve167_fit$Tcorrect,
                         Curve167_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve167_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve167_output) # add the curve fits to the larger data frame

### Curve168_data
Curve168_data <- subset(aci.df, unique_id == aci.df.unique_id[168]) # find correct curve from full dataframe and make new object
plot(Curve168_data$A~Curve168_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve168_fit <- fitaci(Curve168_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve168_fit) # take a look at fitted values, adjust as needed
plot(Curve168_fit) # plot the fitted curves over the raw data, adjust as needed
Curve168_output <- cbind('Curve168', Curve168_data$id[1], Curve168_data$unique_id[1], Curve168_data$machine[1], Curve168_data$baseline_yn[1],
                         Curve168_data$A[1], Curve168_data$Ci[1], Curve168_data$gsw[1],
                         mean(Curve168_data$VPDleaf, na.rm = T), mean(Curve168_data$Tleaf, na.rm = T), mean(Curve168_data$Qin, na.rm = T),
                         Curve168_fit[[2]][1,1], Curve168_fit[[2]][1,2],
                         Curve168_fit[[2]][2,1], Curve168_fit[[2]][2,2],
                         Curve168_fit[[2]][3,1], Curve168_fit[[2]][3,2],
                         Curve168_fit$RMSE,
                         Curve168_fit$Ci_transition,
                         Curve168_fit$citransition,
                         Curve168_fit$Km,
                         Curve168_fit$GammaStar,
                         Curve168_fit$fitmethod,
                         Curve168_fit$Tcorrect,
                         Curve168_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve168_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve168_output) # add the curve fits to the larger data frame

### Curve169_data
Curve169_data <- subset(aci.df, unique_id == aci.df.unique_id[169]) # find correct curve from full dataframe and make new object
plot(Curve169_data$A~Curve169_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve169_fit <- fitaci(Curve169_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve169_fit) # take a look at fitted values, adjust as needed
plot(Curve169_fit) # plot the fitted curves over the raw data, adjust as needed
Curve169_output <- cbind('Curve169', Curve169_data$id[1], Curve169_data$unique_id[1], Curve169_data$machine[1], Curve169_data$baseline_yn[1],
                         Curve169_data$A[1], Curve169_data$Ci[1], Curve169_data$gsw[1],
                         mean(Curve169_data$VPDleaf, na.rm = T), mean(Curve169_data$Tleaf, na.rm = T), mean(Curve169_data$Qin, na.rm = T),
                         Curve169_fit[[2]][1,1], Curve169_fit[[2]][1,2],
                         Curve169_fit[[2]][2,1], Curve169_fit[[2]][2,2],
                         Curve169_fit[[2]][3,1], Curve169_fit[[2]][3,2],
                         Curve169_fit$RMSE,
                         Curve169_fit$Ci_transition,
                         Curve169_fit$citransition,
                         Curve169_fit$Km,
                         Curve169_fit$GammaStar,
                         Curve169_fit$fitmethod,
                         Curve169_fit$Tcorrect,
                         Curve169_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve169_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve169_output) # add the curve fits to the larger data frame

### Curve170_data
Curve170_data <- subset(aci.df, unique_id == aci.df.unique_id[170]) # find correct curve from full dataframe and make new object
plot(Curve170_data$A~Curve170_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve170_fit <- fitaci(Curve170_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve170_fit) # take a look at fitted values, adjust as needed
plot(Curve170_fit) # plot the fitted curves over the raw data, adjust as needed
Curve170_output <- cbind('Curve170', Curve170_data$id[1], Curve170_data$unique_id[1], Curve170_data$machine[1], Curve170_data$baseline_yn[1],
                         Curve170_data$A[1], Curve170_data$Ci[1], Curve170_data$gsw[1],
                         mean(Curve170_data$VPDleaf, na.rm = T), mean(Curve170_data$Tleaf, na.rm = T), mean(Curve170_data$Qin, na.rm = T),
                         Curve170_fit[[2]][1,1], Curve170_fit[[2]][1,2],
                         Curve170_fit[[2]][2,1], Curve170_fit[[2]][2,2],
                         Curve170_fit[[2]][3,1], Curve170_fit[[2]][3,2],
                         Curve170_fit$RMSE,
                         Curve170_fit$Ci_transition,
                         Curve170_fit$citransition,
                         Curve170_fit$Km,
                         Curve170_fit$GammaStar,
                         Curve170_fit$fitmethod,
                         Curve170_fit$Tcorrect,
                         Curve170_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve170_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve170_output) # add the curve fits to the larger data frame

### Curve171_data
Curve171_data <- subset(aci.df, unique_id == aci.df.unique_id[171]) # find correct curve from full dataframe and make new object
plot(Curve171_data$A~Curve171_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve171_fit <- fitaci(Curve171_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve171_fit) # take a look at fitted values, adjust as needed
plot(Curve171_fit) # plot the fitted curves over the raw data, adjust as needed
Curve171_output <- cbind('Curve171', Curve171_data$id[1], Curve171_data$unique_id[1], Curve171_data$machine[1], Curve171_data$baseline_yn[1],
                         Curve171_data$A[1], Curve171_data$Ci[1], Curve171_data$gsw[1],
                         mean(Curve171_data$VPDleaf, na.rm = T), mean(Curve171_data$Tleaf, na.rm = T), mean(Curve171_data$Qin, na.rm = T),
                         Curve171_fit[[2]][1,1], Curve171_fit[[2]][1,2],
                         Curve171_fit[[2]][2,1], Curve171_fit[[2]][2,2],
                         Curve171_fit[[2]][3,1], Curve171_fit[[2]][3,2],
                         Curve171_fit$RMSE,
                         Curve171_fit$Ci_transition,
                         Curve171_fit$citransition,
                         Curve171_fit$Km,
                         Curve171_fit$GammaStar,
                         Curve171_fit$fitmethod,
                         Curve171_fit$Tcorrect,
                         Curve171_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve171_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve171_output) # add the curve fits to the larger data frame

### Curve172_data
Curve172_data <- subset(aci.df, unique_id == aci.df.unique_id[172]) # find correct curve from full dataframe and make new object
plot(Curve172_data$A~Curve172_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve172_fit <- fitaci(Curve172_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve172_fit) # take a look at fitted values, adjust as needed
plot(Curve172_fit) # plot the fitted curves over the raw data, adjust as needed
Curve172_output <- cbind('Curve172', Curve172_data$id[1], Curve172_data$unique_id[1], Curve172_data$machine[1], Curve172_data$baseline_yn[1],
                         Curve172_data$A[1], Curve172_data$Ci[1], Curve172_data$gsw[1],
                         mean(Curve172_data$VPDleaf, na.rm = T), mean(Curve172_data$Tleaf, na.rm = T), mean(Curve172_data$Qin, na.rm = T),
                         Curve172_fit[[2]][1,1], Curve172_fit[[2]][1,2],
                         Curve172_fit[[2]][2,1], Curve172_fit[[2]][2,2],
                         Curve172_fit[[2]][3,1], Curve172_fit[[2]][3,2],
                         Curve172_fit$RMSE,
                         Curve172_fit$Ci_transition,
                         Curve172_fit$citransition,
                         Curve172_fit$Km,
                         Curve172_fit$GammaStar,
                         Curve172_fit$fitmethod,
                         Curve172_fit$Tcorrect,
                         Curve172_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve172_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve172_output) # add the curve fits to the larger data frame

### Curve173_data
Curve173_data <- subset(aci.df, unique_id == aci.df.unique_id[173]  & Ci < 1000) # find correct curve from full dataframe and make new object
plot(Curve173_data$A~Curve173_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve173_fit <- fitaci(Curve173_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve173_fit) # take a look at fitted values, adjust as needed
plot(Curve173_fit) # plot the fitted curves over the raw data, adjust as needed
Curve173_output <- cbind('Curve173', Curve173_data$id[1], Curve173_data$unique_id[1], Curve173_data$machine[1], Curve173_data$baseline_yn[1],
                         Curve173_data$A[1], Curve173_data$Ci[1], Curve173_data$gsw[1],
                         mean(Curve173_data$VPDleaf, na.rm = T), mean(Curve173_data$Tleaf, na.rm = T), mean(Curve173_data$Qin, na.rm = T),
                         Curve173_fit[[2]][1,1], Curve173_fit[[2]][1,2],
                         Curve173_fit[[2]][2,1], Curve173_fit[[2]][2,2],
                         Curve173_fit[[2]][3,1], Curve173_fit[[2]][3,2],
                         Curve173_fit$RMSE,
                         Curve173_fit$Ci_transition,
                         Curve173_fit$citransition,
                         Curve173_fit$Km,
                         Curve173_fit$GammaStar,
                         Curve173_fit$fitmethod,
                         Curve173_fit$Tcorrect,
                         Curve173_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve173_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve173_output) # add the curve fits to the larger data frame

### Curve174_data
Curve174_data <- subset(aci.df, unique_id == aci.df.unique_id[174]) # find correct curve from full dataframe and make new object
plot(Curve174_data$A~Curve174_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve174_fit <- fitaci(Curve174_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve174_fit) # take a look at fitted values, adjust as needed
plot(Curve174_fit) # plot the fitted curves over the raw data, adjust as needed
Curve174_output <- cbind('Curve174', Curve174_data$id[1], Curve174_data$unique_id[1], Curve174_data$machine[1], Curve174_data$baseline_yn[1],
                         Curve174_data$A[1], Curve174_data$Ci[1], Curve174_data$gsw[1],
                         mean(Curve174_data$VPDleaf, na.rm = T), mean(Curve174_data$Tleaf, na.rm = T), mean(Curve174_data$Qin, na.rm = T),
                         Curve174_fit[[2]][1,1], Curve174_fit[[2]][1,2],
                         Curve174_fit[[2]][2,1], Curve174_fit[[2]][2,2],
                         Curve174_fit[[2]][3,1], Curve174_fit[[2]][3,2],
                         Curve174_fit$RMSE,
                         Curve174_fit$Ci_transition,
                         Curve174_fit$citransition,
                         Curve174_fit$Km,
                         Curve174_fit$GammaStar,
                         Curve174_fit$fitmethod,
                         Curve174_fit$Tcorrect,
                         Curve174_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve174_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve174_output) # add the curve fits to the larger data frame

### Curve175_data
Curve175_data <- subset(aci.df, unique_id == aci.df.unique_id[175] & Ci < 1000) # find correct curve from full dataframe and make new object
plot(Curve175_data$A~Curve175_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve175_fit <- fitaci(Curve175_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve175_fit) # take a look at fitted values, adjust as needed
plot(Curve175_fit) # plot the fitted curves over the raw data, adjust as needed
Curve175_output <- cbind('Curve175', Curve175_data$id[1], Curve175_data$unique_id[1], Curve175_data$machine[1], Curve175_data$baseline_yn[1],
                         Curve175_data$A[1], Curve175_data$Ci[1], Curve175_data$gsw[1],
                         mean(Curve175_data$VPDleaf, na.rm = T), mean(Curve175_data$Tleaf, na.rm = T), mean(Curve175_data$Qin, na.rm = T),
                         Curve175_fit[[2]][1,1], Curve175_fit[[2]][1,2],
                         Curve175_fit[[2]][2,1], Curve175_fit[[2]][2,2],
                         Curve175_fit[[2]][3,1], Curve175_fit[[2]][3,2],
                         Curve175_fit$RMSE,
                         Curve175_fit$Ci_transition,
                         Curve175_fit$citransition,
                         Curve175_fit$Km,
                         Curve175_fit$GammaStar,
                         Curve175_fit$fitmethod,
                         Curve175_fit$Tcorrect,
                         Curve175_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve175_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve175_output) # add the curve fits to the larger data frame

### Curve176_data
Curve176_data <- subset(aci.df, unique_id == aci.df.unique_id[176] & Ci < 1000) # find correct curve from full dataframe and make new object
plot(Curve176_data$A~Curve176_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve176_fit <- fitaci(Curve176_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve176_fit) # take a look at fitted values, adjust as needed
plot(Curve176_fit) # plot the fitted curves over the raw data, adjust as needed
Curve176_output <- cbind('Curve176', Curve176_data$id[1], Curve176_data$unique_id[1], Curve176_data$machine[1], Curve176_data$baseline_yn[1],
                         Curve176_data$A[1], Curve176_data$Ci[1], Curve176_data$gsw[1],
                         mean(Curve176_data$VPDleaf, na.rm = T), mean(Curve176_data$Tleaf, na.rm = T), mean(Curve176_data$Qin, na.rm = T),
                         Curve176_fit[[2]][1,1], Curve176_fit[[2]][1,2],
                         Curve176_fit[[2]][2,1], Curve176_fit[[2]][2,2],
                         Curve176_fit[[2]][3,1], Curve176_fit[[2]][3,2],
                         Curve176_fit$RMSE,
                         Curve176_fit$Ci_transition,
                         Curve176_fit$citransition,
                         Curve176_fit$Km,
                         Curve176_fit$GammaStar,
                         Curve176_fit$fitmethod,
                         Curve176_fit$Tcorrect,
                         Curve176_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve176_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve176_output) # add the curve fits to the larger data frame

### Curve177_data
Curve177_data <- subset(aci.df, unique_id == aci.df.unique_id[177]) # find correct curve from full dataframe and make new object
plot(Curve177_data$A~Curve177_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve177_fit <- fitaci(Curve177_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve177_fit) # take a look at fitted values, adjust as needed
plot(Curve177_fit) # plot the fitted curves over the raw data, adjust as needed
Curve177_output <- cbind('Curve177', Curve177_data$id[1], Curve177_data$unique_id[1], Curve177_data$machine[1], Curve177_data$baseline_yn[1],
                         Curve177_data$A[1], Curve177_data$Ci[1], Curve177_data$gsw[1],
                         mean(Curve177_data$VPDleaf, na.rm = T), mean(Curve177_data$Tleaf, na.rm = T), mean(Curve177_data$Qin, na.rm = T),
                         Curve177_fit[[2]][1,1], Curve177_fit[[2]][1,2],
                         Curve177_fit[[2]][2,1], Curve177_fit[[2]][2,2],
                         Curve177_fit[[2]][3,1], Curve177_fit[[2]][3,2],
                         Curve177_fit$RMSE,
                         Curve177_fit$Ci_transition,
                         Curve177_fit$citransition,
                         Curve177_fit$Km,
                         Curve177_fit$GammaStar,
                         Curve177_fit$fitmethod,
                         Curve177_fit$Tcorrect,
                         Curve177_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve177_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve177_output) # add the curve fits to the larger data frame

### Curve178_data
Curve178_data <- subset(aci.df, unique_id == aci.df.unique_id[178]) # find correct curve from full dataframe and make new object
plot(Curve178_data$A~Curve178_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve178_fit <- fitaci(Curve178_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve178_fit) # take a look at fitted values, adjust as needed
plot(Curve178_fit) # plot the fitted curves over the raw data, adjust as needed
Curve178_output <- cbind('Curve178', Curve178_data$id[1], Curve178_data$unique_id[1], Curve178_data$machine[1], Curve178_data$baseline_yn[1],
                         Curve178_data$A[1], Curve178_data$Ci[1], Curve178_data$gsw[1],
                         mean(Curve178_data$VPDleaf, na.rm = T), mean(Curve178_data$Tleaf, na.rm = T), mean(Curve178_data$Qin, na.rm = T),
                         Curve178_fit[[2]][1,1], Curve178_fit[[2]][1,2],
                         Curve178_fit[[2]][2,1], Curve178_fit[[2]][2,2],
                         Curve178_fit[[2]][3,1], Curve178_fit[[2]][3,2],
                         Curve178_fit$RMSE,
                         Curve178_fit$Ci_transition,
                         Curve178_fit$citransition,
                         Curve178_fit$Km,
                         Curve178_fit$GammaStar,
                         Curve178_fit$fitmethod,
                         Curve178_fit$Tcorrect,
                         Curve178_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve178_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve178_output) # add the curve fits to the larger data frame

### Curve179_data
Curve179_data <- subset(aci.df, unique_id == aci.df.unique_id[179]) # find correct curve from full dataframe and make new object
plot(Curve179_data$A~Curve179_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve179_fit <- fitaci(Curve179_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve179_fit) # take a look at fitted values, adjust as needed
plot(Curve179_fit) # plot the fitted curves over the raw data, adjust as needed
Curve179_output <- cbind('Curve179', Curve179_data$id[1], Curve179_data$unique_id[1], Curve179_data$machine[1], Curve179_data$baseline_yn[1],
                         Curve179_data$A[1], Curve179_data$Ci[1], Curve179_data$gsw[1],
                         mean(Curve179_data$VPDleaf, na.rm = T), mean(Curve179_data$Tleaf, na.rm = T), mean(Curve179_data$Qin, na.rm = T),
                         Curve179_fit[[2]][1,1], Curve179_fit[[2]][1,2],
                         Curve179_fit[[2]][2,1], Curve179_fit[[2]][2,2],
                         Curve179_fit[[2]][3,1], Curve179_fit[[2]][3,2],
                         Curve179_fit$RMSE,
                         Curve179_fit$Ci_transition,
                         Curve179_fit$citransition,
                         Curve179_fit$Km,
                         Curve179_fit$GammaStar,
                         Curve179_fit$fitmethod,
                         Curve179_fit$Tcorrect,
                         Curve179_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve179_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve179_output) # add the curve fits to the larger data frame

### Curve180_data
Curve180_data <- subset(aci.df, unique_id == aci.df.unique_id[180]) # find correct curve from full dataframe and make new object
plot(Curve180_data$A~Curve180_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve180_fit <- fitaci(Curve180_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve180_fit) # take a look at fitted values, adjust as needed
plot(Curve180_fit) # plot the fitted curves over the raw data, adjust as needed
Curve180_output <- cbind('Curve180', Curve180_data$id[1], Curve180_data$unique_id[1], Curve180_data$machine[1], Curve180_data$baseline_yn[1],
                         Curve180_data$A[1], Curve180_data$Ci[1], Curve180_data$gsw[1],
                         mean(Curve180_data$VPDleaf, na.rm = T), mean(Curve180_data$Tleaf, na.rm = T), mean(Curve180_data$Qin, na.rm = T),
                         Curve180_fit[[2]][1,1], Curve180_fit[[2]][1,2],
                         Curve180_fit[[2]][2,1], Curve180_fit[[2]][2,2],
                         Curve180_fit[[2]][3,1], Curve180_fit[[2]][3,2],
                         Curve180_fit$RMSE,
                         Curve180_fit$Ci_transition,
                         Curve180_fit$citransition,
                         Curve180_fit$Km,
                         Curve180_fit$GammaStar,
                         Curve180_fit$fitmethod,
                         Curve180_fit$Tcorrect,
                         Curve180_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve180_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve180_output) # add the curve fits to the larger data frame

### Curve181_data
Curve181_data <- subset(aci.df, unique_id == aci.df.unique_id[181]) # find correct curve from full dataframe and make new object
plot(Curve181_data$A~Curve181_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve181_fit <- fitaci(Curve181_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve181_fit) # take a look at fitted values, adjust as needed
plot(Curve181_fit) # plot the fitted curves over the raw data, adjust as needed
Curve181_output <- cbind('Curve181', Curve181_data$id[1], Curve181_data$unique_id[1], Curve181_data$machine[1], Curve181_data$baseline_yn[1],
                         Curve181_data$A[1], Curve181_data$Ci[1], Curve181_data$gsw[1],
                         mean(Curve181_data$VPDleaf, na.rm = T), mean(Curve181_data$Tleaf, na.rm = T), mean(Curve181_data$Qin, na.rm = T),
                         Curve181_fit[[2]][1,1], Curve181_fit[[2]][1,2],
                         Curve181_fit[[2]][2,1], Curve181_fit[[2]][2,2],
                         Curve181_fit[[2]][3,1], Curve181_fit[[2]][3,2],
                         Curve181_fit$RMSE,
                         Curve181_fit$Ci_transition,
                         Curve181_fit$citransition,
                         Curve181_fit$Km,
                         Curve181_fit$GammaStar,
                         Curve181_fit$fitmethod,
                         Curve181_fit$Tcorrect,
                         Curve181_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve181_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve181_output) # add the curve fits to the larger data frame

### Curve182_data
Curve182_data <- subset(aci.df, unique_id == aci.df.unique_id[182]) # find correct curve from full dataframe and make new object
plot(Curve182_data$A~Curve182_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve182_fit <- fitaci(Curve182_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve182_fit) # take a look at fitted values, adjust as needed
plot(Curve182_fit) # plot the fitted curves over the raw data, adjust as needed
Curve182_output <- cbind('Curve182', Curve182_data$id[1], Curve182_data$unique_id[1], Curve182_data$machine[1], Curve182_data$baseline_yn[1],
                         Curve182_data$A[1], Curve182_data$Ci[1], Curve182_data$gsw[1],
                         mean(Curve182_data$VPDleaf, na.rm = T), mean(Curve182_data$Tleaf, na.rm = T), mean(Curve182_data$Qin, na.rm = T),
                         Curve182_fit[[2]][1,1], Curve182_fit[[2]][1,2],
                         Curve182_fit[[2]][2,1], Curve182_fit[[2]][2,2],
                         Curve182_fit[[2]][3,1], Curve182_fit[[2]][3,2],
                         Curve182_fit$RMSE,
                         Curve182_fit$Ci_transition,
                         Curve182_fit$citransition,
                         Curve182_fit$Km,
                         Curve182_fit$GammaStar,
                         Curve182_fit$fitmethod,
                         Curve182_fit$Tcorrect,
                         Curve182_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve182_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve182_output) # add the curve fits to the larger data frame

### Curve183_data
Curve183_data <- subset(aci.df, unique_id == aci.df.unique_id[183]) # find correct curve from full dataframe and make new object
plot(Curve183_data$A~Curve183_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve183_fit <- fitaci(Curve183_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve183_fit) # take a look at fitted values, adjust as needed
plot(Curve183_fit) # plot the fitted curves over the raw data, adjust as needed
Curve183_output <- cbind('Curve183', Curve183_data$id[1], Curve183_data$unique_id[1], Curve183_data$machine[1], Curve183_data$baseline_yn[1],
                         Curve183_data$A[1], Curve183_data$Ci[1], Curve183_data$gsw[1],
                         mean(Curve183_data$VPDleaf, na.rm = T), mean(Curve183_data$Tleaf, na.rm = T), mean(Curve183_data$Qin, na.rm = T),
                         Curve183_fit[[2]][1,1], Curve183_fit[[2]][1,2],
                         Curve183_fit[[2]][2,1], Curve183_fit[[2]][2,2],
                         Curve183_fit[[2]][3,1], Curve183_fit[[2]][3,2],
                         Curve183_fit$RMSE,
                         Curve183_fit$Ci_transition,
                         Curve183_fit$citransition,
                         Curve183_fit$Km,
                         Curve183_fit$GammaStar,
                         Curve183_fit$fitmethod,
                         Curve183_fit$Tcorrect,
                         Curve183_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve183_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve183_output) # add the curve fits to the larger data frame

### Curve184_data
Curve184_data <- subset(aci.df, unique_id == aci.df.unique_id[184]) # find correct curve from full dataframe and make new object
plot(Curve184_data$A~Curve184_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve184_fit <- fitaci(Curve184_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve184_fit) # take a look at fitted values, adjust as needed
plot(Curve184_fit) # plot the fitted curves over the raw data, adjust as needed
Curve184_output <- cbind('Curve184', Curve184_data$id[1], Curve184_data$unique_id[1], Curve184_data$machine[1], Curve184_data$baseline_yn[1],
                         Curve184_data$A[1], Curve184_data$Ci[1], Curve184_data$gsw[1],
                         mean(Curve184_data$VPDleaf, na.rm = T), mean(Curve184_data$Tleaf, na.rm = T), mean(Curve184_data$Qin, na.rm = T),
                         Curve184_fit[[2]][1,1], Curve184_fit[[2]][1,2],
                         Curve184_fit[[2]][2,1], Curve184_fit[[2]][2,2],
                         Curve184_fit[[2]][3,1], Curve184_fit[[2]][3,2],
                         Curve184_fit$RMSE,
                         Curve184_fit$Ci_transition,
                         Curve184_fit$citransition,
                         Curve184_fit$Km,
                         Curve184_fit$GammaStar,
                         Curve184_fit$fitmethod,
                         Curve184_fit$Tcorrect,
                         Curve184_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve184_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve184_output) # add the curve fits to the larger data frame

### Curve184_data
Curve184_data <- subset(aci.df, unique_id == aci.df.unique_id[185]) # find correct curve from full dataframe and make new object
plot(Curve184_data$A~Curve184_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve184_fit <- fitaci(Curve184_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve184_fit) # take a look at fitted values, adjust as needed
plot(Curve184_fit) # plot the fitted curves over the raw data, adjust as needed
Curve184_output <- cbind('Curve184', Curve184_data$id[1], Curve184_data$unique_id[1], Curve184_data$machine[1], Curve184_data$baseline_yn[1],
                         Curve184_data$A[1], Curve184_data$Ci[1], Curve184_data$gsw[1],
                         mean(Curve184_data$VPDleaf, na.rm = T), mean(Curve184_data$Tleaf, na.rm = T), mean(Curve184_data$Qin, na.rm = T),
                         Curve184_fit[[2]][1,1], Curve184_fit[[2]][1,2],
                         Curve184_fit[[2]][2,1], Curve184_fit[[2]][2,2],
                         Curve184_fit[[2]][3,1], Curve184_fit[[2]][3,2],
                         Curve184_fit$RMSE,
                         Curve184_fit$Ci_transition,
                         Curve184_fit$citransition,
                         Curve184_fit$Km,
                         Curve184_fit$GammaStar,
                         Curve184_fit$fitmethod,
                         Curve184_fit$Tcorrect,
                         Curve184_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve184_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve184_output) # add the curve fits to the larger data frame

### Curve185_data
Curve185_data <- subset(aci.df, unique_id == aci.df.unique_id[185]) # find correct curve from full dataframe and make new object
plot(Curve185_data$A~Curve185_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve185_fit <- fitaci(Curve185_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve185_fit) # take a look at fitted values, adjust as needed
plot(Curve185_fit) # plot the fitted curves over the raw data, adjust as needed
Curve185_output <- cbind('Curve185', Curve185_data$id[1], Curve185_data$unique_id[1], Curve185_data$machine[1], Curve185_data$baseline_yn[1],
                         Curve185_data$A[1], Curve185_data$Ci[1], Curve185_data$gsw[1],
                         mean(Curve185_data$VPDleaf, na.rm = T), mean(Curve185_data$Tleaf, na.rm = T), mean(Curve185_data$Qin, na.rm = T),
                         Curve185_fit[[2]][1,1], Curve185_fit[[2]][1,2],
                         Curve185_fit[[2]][2,1], Curve185_fit[[2]][2,2],
                         Curve185_fit[[2]][3,1], Curve185_fit[[2]][3,2],
                         Curve185_fit$RMSE,
                         Curve185_fit$Ci_transition,
                         Curve185_fit$citransition,
                         Curve185_fit$Km,
                         Curve185_fit$GammaStar,
                         Curve185_fit$fitmethod,
                         Curve185_fit$Tcorrect,
                         Curve185_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve185_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve185_output) # add the curve fits to the larger data frame

### Curve186_data
Curve186_data <- subset(aci.df, unique_id == aci.df.unique_id[186] & Ci < 1000) # find correct curve from full dataframe and make new object
plot(Curve186_data$A~Curve186_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve186_fit <- fitaci(Curve186_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve186_fit) # take a look at fitted values, adjust as needed
plot(Curve186_fit) # plot the fitted curves over the raw data, adjust as needed
Curve186_output <- cbind('Curve186', Curve186_data$id[1], Curve186_data$unique_id[1], Curve186_data$machine[1], Curve186_data$baseline_yn[1],
                         Curve186_data$A[1], Curve186_data$Ci[1], Curve186_data$gsw[1],
                         mean(Curve186_data$VPDleaf, na.rm = T), mean(Curve186_data$Tleaf, na.rm = T), mean(Curve186_data$Qin, na.rm = T),
                         Curve186_fit[[2]][1,1], Curve186_fit[[2]][1,2],
                         Curve186_fit[[2]][2,1], Curve186_fit[[2]][2,2],
                         Curve186_fit[[2]][3,1], Curve186_fit[[2]][3,2],
                         Curve186_fit$RMSE,
                         Curve186_fit$Ci_transition,
                         Curve186_fit$citransition,
                         Curve186_fit$Km,
                         Curve186_fit$GammaStar,
                         Curve186_fit$fitmethod,
                         Curve186_fit$Tcorrect,
                         Curve186_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve186_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve186_output) # add the curve fits to the larger data frame

### Curve187_data
Curve187_data <- subset(aci.df, unique_id == aci.df.unique_id[187]) # find correct curve from full dataframe and make new object
plot(Curve187_data$A~Curve187_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve187_fit <- fitaci(Curve187_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve187_fit) # take a look at fitted values, adjust as needed
plot(Curve187_fit) # plot the fitted curves over the raw data, adjust as needed
Curve187_output <- cbind('Curve187', Curve187_data$id[1], Curve187_data$unique_id[1], Curve187_data$machine[1], Curve187_data$baseline_yn[1],
                         Curve187_data$A[1], Curve187_data$Ci[1], Curve187_data$gsw[1],
                         mean(Curve187_data$VPDleaf, na.rm = T), mean(Curve187_data$Tleaf, na.rm = T), mean(Curve187_data$Qin, na.rm = T),
                         Curve187_fit[[2]][1,1], Curve187_fit[[2]][1,2],
                         Curve187_fit[[2]][2,1], Curve187_fit[[2]][2,2],
                         Curve187_fit[[2]][3,1], Curve187_fit[[2]][3,2],
                         Curve187_fit$RMSE,
                         Curve187_fit$Ci_transition,
                         Curve187_fit$citransition,
                         Curve187_fit$Km,
                         Curve187_fit$GammaStar,
                         Curve187_fit$fitmethod,
                         Curve187_fit$Tcorrect,
                         Curve187_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve187_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve187_output) # add the curve fits to the larger data frame

### Curve188_data
Curve188_data <- subset(aci.df, unique_id == aci.df.unique_id[188]) # find correct curve from full dataframe and make new object
plot(Curve188_data$A~Curve188_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve188_fit <- fitaci(Curve188_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve188_fit) # take a look at fitted values, adjust as needed
plot(Curve188_fit) # plot the fitted curves over the raw data, adjust as needed
Curve188_output <- cbind('Curve188', Curve188_data$id[1], Curve188_data$unique_id[1], Curve188_data$machine[1], Curve188_data$baseline_yn[1],
                         Curve188_data$A[1], Curve188_data$Ci[1], Curve188_data$gsw[1],
                         mean(Curve188_data$VPDleaf, na.rm = T), mean(Curve188_data$Tleaf, na.rm = T), mean(Curve188_data$Qin, na.rm = T),
                         Curve188_fit[[2]][1,1], Curve188_fit[[2]][1,2],
                         Curve188_fit[[2]][2,1], Curve188_fit[[2]][2,2],
                         Curve188_fit[[2]][3,1], Curve188_fit[[2]][3,2],
                         Curve188_fit$RMSE,
                         Curve188_fit$Ci_transition,
                         Curve188_fit$citransition,
                         Curve188_fit$Km,
                         Curve188_fit$GammaStar,
                         Curve188_fit$fitmethod,
                         Curve188_fit$Tcorrect,
                         Curve188_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve188_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve188_output) # add the curve fits to the larger data frame

### Curve189_data
Curve189_data <- subset(aci.df, unique_id == aci.df.unique_id[189]) # find correct curve from full dataframe and make new object
plot(Curve189_data$A~Curve189_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve189_fit <- fitaci(Curve189_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve189_fit) # take a look at fitted values, adjust as needed
plot(Curve189_fit) # plot the fitted curves over the raw data, adjust as needed
Curve189_output <- cbind('Curve189', Curve189_data$id[1], Curve189_data$unique_id[1], Curve189_data$machine[1], Curve189_data$baseline_yn[1],
                         Curve189_data$A[1], Curve189_data$Ci[1], Curve189_data$gsw[1],
                         mean(Curve189_data$VPDleaf, na.rm = T), mean(Curve189_data$Tleaf, na.rm = T), mean(Curve189_data$Qin, na.rm = T),
                         Curve189_fit[[2]][1,1], Curve189_fit[[2]][1,2],
                         Curve189_fit[[2]][2,1], Curve189_fit[[2]][2,2],
                         Curve189_fit[[2]][3,1], Curve189_fit[[2]][3,2],
                         Curve189_fit$RMSE,
                         Curve189_fit$Ci_transition,
                         Curve189_fit$citransition,
                         Curve189_fit$Km,
                         Curve189_fit$GammaStar,
                         Curve189_fit$fitmethod,
                         Curve189_fit$Tcorrect,
                         Curve189_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve189_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve189_output) # add the curve fits to the larger data frame

### Curve190_data
Curve190_data <- subset(aci.df, unique_id == aci.df.unique_id[190]) # find correct curve from full dataframe and make new object
plot(Curve190_data$A~Curve190_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve190_fit <- fitaci(Curve190_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve190_fit) # take a look at fitted values, adjust as needed
plot(Curve190_fit) # plot the fitted curves over the raw data, adjust as needed
Curve190_output <- cbind('Curve190', Curve190_data$id[1], Curve190_data$unique_id[1], Curve190_data$machine[1], Curve190_data$baseline_yn[1],
                         Curve190_data$A[1], Curve190_data$Ci[1], Curve190_data$gsw[1],
                         mean(Curve190_data$VPDleaf, na.rm = T), mean(Curve190_data$Tleaf, na.rm = T), mean(Curve190_data$Qin, na.rm = T),
                         Curve190_fit[[2]][1,1], Curve190_fit[[2]][1,2],
                         Curve190_fit[[2]][2,1], Curve190_fit[[2]][2,2],
                         Curve190_fit[[2]][3,1], Curve190_fit[[2]][3,2],
                         Curve190_fit$RMSE,
                         Curve190_fit$Ci_transition,
                         Curve190_fit$citransition,
                         Curve190_fit$Km,
                         Curve190_fit$GammaStar,
                         Curve190_fit$fitmethod,
                         Curve190_fit$Tcorrect,
                         Curve190_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve190_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve190_output) # add the curve fits to the larger data frame

### Curve191_data
Curve191_data <- subset(aci.df, unique_id == aci.df.unique_id[191] & Ci < 1000) # find correct curve from full dataframe and make new object
plot(Curve191_data$A~Curve191_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve191_fit <- fitaci(Curve191_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve191_fit) # take a look at fitted values, adjust as needed
plot(Curve191_fit) # plot the fitted curves over the raw data, adjust as needed
Curve191_output <- cbind('Curve191', Curve191_data$id[1], Curve191_data$unique_id[1], Curve191_data$machine[1], Curve191_data$baseline_yn[1],
                         Curve191_data$A[1], Curve191_data$Ci[1], Curve191_data$gsw[1],
                         mean(Curve191_data$VPDleaf, na.rm = T), mean(Curve191_data$Tleaf, na.rm = T), mean(Curve191_data$Qin, na.rm = T),
                         Curve191_fit[[2]][1,1], Curve191_fit[[2]][1,2],
                         Curve191_fit[[2]][2,1], Curve191_fit[[2]][2,2],
                         Curve191_fit[[2]][3,1], Curve191_fit[[2]][3,2],
                         Curve191_fit$RMSE,
                         Curve191_fit$Ci_transition,
                         Curve191_fit$citransition,
                         Curve191_fit$Km,
                         Curve191_fit$GammaStar,
                         Curve191_fit$fitmethod,
                         Curve191_fit$Tcorrect,
                         Curve191_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve191_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve191_output) # add the curve fits to the larger data frame

### Curve192_data
Curve192_data <- subset(aci.df, unique_id == aci.df.unique_id[192]) # find correct curve from full dataframe and make new object
plot(Curve192_data$A~Curve192_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve192_fit <- fitaci(Curve192_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve192_fit) # take a look at fitted values, adjust as needed
plot(Curve192_fit) # plot the fitted curves over the raw data, adjust as needed
Curve192_output <- cbind('Curve192', Curve192_data$id[1], Curve192_data$unique_id[1], Curve192_data$machine[1], Curve192_data$baseline_yn[1],
                         Curve192_data$A[1], Curve192_data$Ci[1], Curve192_data$gsw[1],
                         mean(Curve192_data$VPDleaf, na.rm = T), mean(Curve192_data$Tleaf, na.rm = T), mean(Curve192_data$Qin, na.rm = T),
                         Curve192_fit[[2]][1,1], Curve192_fit[[2]][1,2],
                         Curve192_fit[[2]][2,1], Curve192_fit[[2]][2,2],
                         Curve192_fit[[2]][3,1], Curve192_fit[[2]][3,2],
                         Curve192_fit$RMSE,
                         Curve192_fit$Ci_transition,
                         Curve192_fit$citransition,
                         Curve192_fit$Km,
                         Curve192_fit$GammaStar,
                         Curve192_fit$fitmethod,
                         Curve192_fit$Tcorrect,
                         Curve192_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve192_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve192_output) # add the curve fits to the larger data frame

### Curve193_data
Curve193_data <- subset(aci.df, unique_id == aci.df.unique_id[193] & Ci < 1000) # find correct curve from full dataframe and make new object
plot(Curve193_data$A~Curve193_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve193_fit <- fitaci(Curve193_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve193_fit) # take a look at fitted values, adjust as needed
plot(Curve193_fit) # plot the fitted curves over the raw data, adjust as needed
Curve193_output <- cbind('Curve193', Curve193_data$id[1], Curve193_data$unique_id[1], Curve193_data$machine[1], Curve193_data$baseline_yn[1],
                         Curve193_data$A[1], Curve193_data$Ci[1], Curve193_data$gsw[1],
                         mean(Curve193_data$VPDleaf, na.rm = T), mean(Curve193_data$Tleaf, na.rm = T), mean(Curve193_data$Qin, na.rm = T),
                         Curve193_fit[[2]][1,1], Curve193_fit[[2]][1,2],
                         Curve193_fit[[2]][2,1], Curve193_fit[[2]][2,2],
                         Curve193_fit[[2]][3,1], Curve193_fit[[2]][3,2],
                         Curve193_fit$RMSE,
                         Curve193_fit$Ci_transition,
                         Curve193_fit$citransition,
                         Curve193_fit$Km,
                         Curve193_fit$GammaStar,
                         Curve193_fit$fitmethod,
                         Curve193_fit$Tcorrect,
                         Curve193_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve193_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve193_output) # add the curve fits to the larger data frame

### Curve194_data
Curve194_data <- subset(aci.df, unique_id == aci.df.unique_id[194] & Ci < 1000) # find correct curve from full dataframe and make new object
plot(Curve194_data$A~Curve194_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve194_fit <- fitaci(Curve194_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve194_fit) # take a look at fitted values, adjust as needed
plot(Curve194_fit) # plot the fitted curves over the raw data, adjust as needed
Curve194_output <- cbind('Curve194', Curve194_data$id[1], Curve194_data$unique_id[1], Curve194_data$machine[1], Curve194_data$baseline_yn[1],
                         Curve194_data$A[1], Curve194_data$Ci[1], Curve194_data$gsw[1],
                         mean(Curve194_data$VPDleaf, na.rm = T), mean(Curve194_data$Tleaf, na.rm = T), mean(Curve194_data$Qin, na.rm = T),
                         Curve194_fit[[2]][1,1], Curve194_fit[[2]][1,2],
                         Curve194_fit[[2]][2,1], Curve194_fit[[2]][2,2],
                         Curve194_fit[[2]][3,1], Curve194_fit[[2]][3,2],
                         Curve194_fit$RMSE,
                         Curve194_fit$Ci_transition,
                         Curve194_fit$citransition,
                         Curve194_fit$Km,
                         Curve194_fit$GammaStar,
                         Curve194_fit$fitmethod,
                         Curve194_fit$Tcorrect,
                         Curve194_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve194_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve194_output) # add the curve fits to the larger data frame

### Curve195_data
Curve195_data <- subset(aci.df, unique_id == aci.df.unique_id[195]) # find correct curve from full dataframe and make new object
plot(Curve195_data$A~Curve195_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve195_fit <- fitaci(Curve195_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve195_fit) # take a look at fitted values, adjust as needed
plot(Curve195_fit) # plot the fitted curves over the raw data, adjust as needed
Curve195_output <- cbind('Curve195', Curve195_data$id[1], Curve195_data$unique_id[1], Curve195_data$machine[1], Curve195_data$baseline_yn[1],
                         Curve195_data$A[1], Curve195_data$Ci[1], Curve195_data$gsw[1],
                         mean(Curve195_data$VPDleaf, na.rm = T), mean(Curve195_data$Tleaf, na.rm = T), mean(Curve195_data$Qin, na.rm = T),
                         Curve195_fit[[2]][1,1], Curve195_fit[[2]][1,2],
                         Curve195_fit[[2]][2,1], Curve195_fit[[2]][2,2],
                         Curve195_fit[[2]][3,1], Curve195_fit[[2]][3,2],
                         Curve195_fit$RMSE,
                         Curve195_fit$Ci_transition,
                         Curve195_fit$citransition,
                         Curve195_fit$Km,
                         Curve195_fit$GammaStar,
                         Curve195_fit$fitmethod,
                         Curve195_fit$Tcorrect,
                         Curve195_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve195_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve195_output) # add the curve fits to the larger data frame

### Curve196_data
Curve196_data <- subset(aci.df, unique_id == aci.df.unique_id[196]) # find correct curve from full dataframe and make new object
plot(Curve196_data$A~Curve196_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve196_fit <- fitaci(Curve196_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve196_fit) # take a look at fitted values, adjust as needed
plot(Curve196_fit) # plot the fitted curves over the raw data, adjust as needed
Curve196_output <- cbind('Curve196', Curve196_data$id[1], Curve196_data$unique_id[1], Curve196_data$machine[1], Curve196_data$baseline_yn[1],
                         Curve196_data$A[1], Curve196_data$Ci[1], Curve196_data$gsw[1],
                         mean(Curve196_data$VPDleaf, na.rm = T), mean(Curve196_data$Tleaf, na.rm = T), mean(Curve196_data$Qin, na.rm = T),
                         Curve196_fit[[2]][1,1], Curve196_fit[[2]][1,2],
                         Curve196_fit[[2]][2,1], Curve196_fit[[2]][2,2],
                         Curve196_fit[[2]][3,1], Curve196_fit[[2]][3,2],
                         Curve196_fit$RMSE,
                         Curve196_fit$Ci_transition,
                         Curve196_fit$citransition,
                         Curve196_fit$Km,
                         Curve196_fit$GammaStar,
                         Curve196_fit$fitmethod,
                         Curve196_fit$Tcorrect,
                         Curve196_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve196_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve196_output) # add the curve fits to the larger data frame

### Curve197_data
Curve197_data <- subset(aci.df, unique_id == aci.df.unique_id[197]) # find correct curve from full dataframe and make new object
plot(Curve197_data$A~Curve197_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve197_fit <- fitaci(Curve197_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve197_fit) # take a look at fitted values, adjust as needed
plot(Curve197_fit) # plot the fitted curves over the raw data, adjust as needed
Curve197_output <- cbind('Curve197', Curve197_data$id[1], Curve197_data$unique_id[1], Curve197_data$machine[1], Curve197_data$baseline_yn[1],
                         Curve197_data$A[1], Curve197_data$Ci[1], Curve197_data$gsw[1],
                         mean(Curve197_data$VPDleaf, na.rm = T), mean(Curve197_data$Tleaf, na.rm = T), mean(Curve197_data$Qin, na.rm = T),
                         Curve197_fit[[2]][1,1], Curve197_fit[[2]][1,2],
                         Curve197_fit[[2]][2,1], Curve197_fit[[2]][2,2],
                         Curve197_fit[[2]][3,1], Curve197_fit[[2]][3,2],
                         Curve197_fit$RMSE,
                         Curve197_fit$Ci_transition,
                         Curve197_fit$citransition,
                         Curve197_fit$Km,
                         Curve197_fit$GammaStar,
                         Curve197_fit$fitmethod,
                         Curve197_fit$Tcorrect,
                         Curve197_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve197_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve197_output) # add the curve fits to the larger data frame

### Curve198_data
Curve198_data <- subset(aci.df, unique_id == aci.df.unique_id[198]) # find correct curve from full dataframe and make new object
plot(Curve198_data$A~Curve198_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve198_fit <- fitaci(Curve198_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve198_fit) # take a look at fitted values, adjust as needed
plot(Curve198_fit) # plot the fitted curves over the raw data, adjust as needed
Curve198_output <- cbind('Curve198', Curve198_data$id[1], Curve198_data$unique_id[1], Curve198_data$machine[1], Curve198_data$baseline_yn[1],
                         Curve198_data$A[1], Curve198_data$Ci[1], Curve198_data$gsw[1],
                         mean(Curve198_data$VPDleaf, na.rm = T), mean(Curve198_data$Tleaf, na.rm = T), mean(Curve198_data$Qin, na.rm = T),
                         Curve198_fit[[2]][1,1], Curve198_fit[[2]][1,2],
                         Curve198_fit[[2]][2,1], Curve198_fit[[2]][2,2],
                         Curve198_fit[[2]][3,1], Curve198_fit[[2]][3,2],
                         Curve198_fit$RMSE,
                         Curve198_fit$Ci_transition,
                         Curve198_fit$citransition,
                         Curve198_fit$Km,
                         Curve198_fit$GammaStar,
                         Curve198_fit$fitmethod,
                         Curve198_fit$Tcorrect,
                         Curve198_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve198_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve198_output) # add the curve fits to the larger data frame

### Curve199_data
Curve199_data <- subset(aci.df, unique_id == aci.df.unique_id[199]) # find correct curve from full dataframe and make new object
plot(Curve199_data$A~Curve199_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve199_fit <- fitaci(Curve199_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve199_fit) # take a look at fitted values, adjust as needed
plot(Curve199_fit) # plot the fitted curves over the raw data, adjust as needed
Curve199_output <- cbind('Curve199', Curve199_data$id[1], Curve199_data$unique_id[1], Curve199_data$machine[1], Curve199_data$baseline_yn[1],
                         Curve199_data$A[1], Curve199_data$Ci[1], Curve199_data$gsw[1],
                         mean(Curve199_data$VPDleaf, na.rm = T), mean(Curve199_data$Tleaf, na.rm = T), mean(Curve199_data$Qin, na.rm = T),
                         Curve199_fit[[2]][1,1], Curve199_fit[[2]][1,2],
                         Curve199_fit[[2]][2,1], Curve199_fit[[2]][2,2],
                         Curve199_fit[[2]][3,1], Curve199_fit[[2]][3,2],
                         Curve199_fit$RMSE,
                         Curve199_fit$Ci_transition,
                         Curve199_fit$citransition,
                         Curve199_fit$Km,
                         Curve199_fit$GammaStar,
                         Curve199_fit$fitmethod,
                         Curve199_fit$Tcorrect,
                         Curve199_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve199_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve199_output) # add the curve fits to the larger data frame

### Curve199_data
Curve199_data <- subset(aci.df, unique_id == aci.df.unique_id[200]) # find correct curve from full dataframe and make new object
plot(Curve199_data$A~Curve199_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve199_fit <- fitaci(Curve199_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve199_fit) # take a look at fitted values, adjust as needed
plot(Curve199_fit) # plot the fitted curves over the raw data, adjust as needed
Curve199_output <- cbind('Curve199', Curve199_data$id[1], Curve199_data$unique_id[1], Curve199_data$machine[1], Curve199_data$baseline_yn[1],
                         Curve199_data$A[1], Curve199_data$Ci[1], Curve199_data$gsw[1],
                         mean(Curve199_data$VPDleaf, na.rm = T), mean(Curve199_data$Tleaf, na.rm = T), mean(Curve199_data$Qin, na.rm = T),
                         Curve199_fit[[2]][1,1], Curve199_fit[[2]][1,2],
                         Curve199_fit[[2]][2,1], Curve199_fit[[2]][2,2],
                         Curve199_fit[[2]][3,1], Curve199_fit[[2]][3,2],
                         Curve199_fit$RMSE,
                         Curve199_fit$Ci_transition,
                         Curve199_fit$citransition,
                         Curve199_fit$Km,
                         Curve199_fit$GammaStar,
                         Curve199_fit$fitmethod,
                         Curve199_fit$Tcorrect,
                         Curve199_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve199_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve199_output) # add the curve fits to the larger data frame

### Curve200_data
Curve200_data <- subset(aci.df, unique_id == aci.df.unique_id[200]) # find correct curve from full dataframe and make new object
plot(Curve200_data$A~Curve200_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve200_fit <- fitaci(Curve200_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve200_fit) # take a look at fitted values, adjust as needed
plot(Curve200_fit) # plot the fitted curves over the raw data, adjust as needed
Curve200_output <- cbind('Curve200', Curve200_data$id[1], Curve200_data$unique_id[1], Curve200_data$machine[1], Curve200_data$baseline_yn[1],
                         Curve200_data$A[1], Curve200_data$Ci[1], Curve200_data$gsw[1],
                         mean(Curve200_data$VPDleaf, na.rm = T), mean(Curve200_data$Tleaf, na.rm = T), mean(Curve200_data$Qin, na.rm = T),
                         Curve200_fit[[2]][1,1], Curve200_fit[[2]][1,2],
                         Curve200_fit[[2]][2,1], Curve200_fit[[2]][2,2],
                         Curve200_fit[[2]][3,1], Curve200_fit[[2]][3,2],
                         Curve200_fit$RMSE,
                         Curve200_fit$Ci_transition,
                         Curve200_fit$citransition,
                         Curve200_fit$Km,
                         Curve200_fit$GammaStar,
                         Curve200_fit$fitmethod,
                         Curve200_fit$Tcorrect,
                         Curve200_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve200_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve200_output) # add the curve fits to the larger data frame

### Curve201_data
Curve201_data <- subset(aci.df, unique_id == aci.df.unique_id[201]) # find correct curve from full dataframe and make new object
plot(Curve201_data$A~Curve201_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve201_fit <- fitaci(Curve201_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve201_fit) # take a look at fitted values, adjust as needed
plot(Curve201_fit) # plot the fitted curves over the raw data, adjust as needed
Curve201_output <- cbind('Curve201', Curve201_data$id[1], Curve201_data$unique_id[1], Curve201_data$machine[1], Curve201_data$baseline_yn[1],
                         Curve201_data$A[1], Curve201_data$Ci[1], Curve201_data$gsw[1],
                         mean(Curve201_data$VPDleaf, na.rm = T), mean(Curve201_data$Tleaf, na.rm = T), mean(Curve201_data$Qin, na.rm = T),
                         Curve201_fit[[2]][1,1], Curve201_fit[[2]][1,2],
                         Curve201_fit[[2]][2,1], Curve201_fit[[2]][2,2],
                         Curve201_fit[[2]][3,1], Curve201_fit[[2]][3,2],
                         Curve201_fit$RMSE,
                         Curve201_fit$Ci_transition,
                         Curve201_fit$citransition,
                         Curve201_fit$Km,
                         Curve201_fit$GammaStar,
                         Curve201_fit$fitmethod,
                         Curve201_fit$Tcorrect,
                         Curve201_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve201_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve201_output) # add the curve fits to the larger data frame

### Curve202_data
Curve202_data <- subset(aci.df, unique_id == aci.df.unique_id[202]) # find correct curve from full dataframe and make new object
plot(Curve202_data$A~Curve202_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve202_fit <- fitaci(Curve202_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve202_fit) # take a look at fitted values, adjust as needed
plot(Curve202_fit) # plot the fitted curves over the raw data, adjust as needed
Curve202_output <- cbind('Curve202', Curve202_data$id[1], Curve202_data$unique_id[1], Curve202_data$machine[1], Curve202_data$baseline_yn[1],
                         Curve202_data$A[1], Curve202_data$Ci[1], Curve202_data$gsw[1],
                         mean(Curve202_data$VPDleaf, na.rm = T), mean(Curve202_data$Tleaf, na.rm = T), mean(Curve202_data$Qin, na.rm = T),
                         Curve202_fit[[2]][1,1], Curve202_fit[[2]][1,2],
                         Curve202_fit[[2]][2,1], Curve202_fit[[2]][2,2],
                         Curve202_fit[[2]][3,1], Curve202_fit[[2]][3,2],
                         Curve202_fit$RMSE,
                         Curve202_fit$Ci_transition,
                         Curve202_fit$citransition,
                         Curve202_fit$Km,
                         Curve202_fit$GammaStar,
                         Curve202_fit$fitmethod,
                         Curve202_fit$Tcorrect,
                         Curve202_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve202_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve202_output) # add the curve fits to the larger data frame

### Curve203_data
Curve203_data <- subset(aci.df, unique_id == aci.df.unique_id[203]) # find correct curve from full dataframe and make new object
plot(Curve203_data$A~Curve203_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve203_fit <- fitaci(Curve203_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve203_fit) # take a look at fitted values, adjust as needed
plot(Curve203_fit) # plot the fitted curves over the raw data, adjust as needed
Curve203_output <- cbind('Curve203', Curve203_data$id[1], Curve203_data$unique_id[1], Curve203_data$machine[1], Curve203_data$baseline_yn[1],
                         Curve203_data$A[1], Curve203_data$Ci[1], Curve203_data$gsw[1],
                         mean(Curve203_data$VPDleaf, na.rm = T), mean(Curve203_data$Tleaf, na.rm = T), mean(Curve203_data$Qin, na.rm = T),
                         Curve203_fit[[2]][1,1], Curve203_fit[[2]][1,2],
                         Curve203_fit[[2]][2,1], Curve203_fit[[2]][2,2],
                         Curve203_fit[[2]][3,1], Curve203_fit[[2]][3,2],
                         Curve203_fit$RMSE,
                         Curve203_fit$Ci_transition,
                         Curve203_fit$citransition,
                         Curve203_fit$Km,
                         Curve203_fit$GammaStar,
                         Curve203_fit$fitmethod,
                         Curve203_fit$Tcorrect,
                         Curve203_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve203_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve203_output) # add the curve fits to the larger data frame

### Curve204_data
Curve204_data <- subset(aci.df, unique_id == aci.df.unique_id[204]) # find correct curve from full dataframe and make new object
plot(Curve204_data$A~Curve204_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve204_fit <- fitaci(Curve204_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve204_fit) # take a look at fitted values, adjust as needed
plot(Curve204_fit) # plot the fitted curves over the raw data, adjust as needed
Curve204_output <- cbind('Curve204', Curve204_data$id[1], Curve204_data$unique_id[1], Curve204_data$machine[1], Curve204_data$baseline_yn[1],
                         Curve204_data$A[1], Curve204_data$Ci[1], Curve204_data$gsw[1],
                         mean(Curve204_data$VPDleaf, na.rm = T), mean(Curve204_data$Tleaf, na.rm = T), mean(Curve204_data$Qin, na.rm = T),
                         Curve204_fit[[2]][1,1], Curve204_fit[[2]][1,2],
                         Curve204_fit[[2]][2,1], Curve204_fit[[2]][2,2],
                         Curve204_fit[[2]][3,1], Curve204_fit[[2]][3,2],
                         Curve204_fit$RMSE,
                         Curve204_fit$Ci_transition,
                         Curve204_fit$citransition,
                         Curve204_fit$Km,
                         Curve204_fit$GammaStar,
                         Curve204_fit$fitmethod,
                         Curve204_fit$Tcorrect,
                         Curve204_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve204_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve204_output) # add the curve fits to the larger data frame

### Curve205_data
Curve205_data <- subset(aci.df, unique_id == aci.df.unique_id[205]) # find correct curve from full dataframe and make new object
plot(Curve205_data$A~Curve205_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve205_fit <- fitaci(Curve205_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve205_fit) # take a look at fitted values, adjust as needed
plot(Curve205_fit) # plot the fitted curves over the raw data, adjust as needed
Curve205_output <- cbind('Curve205', Curve205_data$id[1], Curve205_data$unique_id[1], Curve205_data$machine[1], Curve205_data$baseline_yn[1],
                         Curve205_data$A[1], Curve205_data$Ci[1], Curve205_data$gsw[1],
                         mean(Curve205_data$VPDleaf, na.rm = T), mean(Curve205_data$Tleaf, na.rm = T), mean(Curve205_data$Qin, na.rm = T),
                         Curve205_fit[[2]][1,1], Curve205_fit[[2]][1,2],
                         Curve205_fit[[2]][2,1], Curve205_fit[[2]][2,2],
                         Curve205_fit[[2]][3,1], Curve205_fit[[2]][3,2],
                         Curve205_fit$RMSE,
                         Curve205_fit$Ci_transition,
                         Curve205_fit$citransition,
                         Curve205_fit$Km,
                         Curve205_fit$GammaStar,
                         Curve205_fit$fitmethod,
                         Curve205_fit$Tcorrect,
                         Curve205_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve205_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve205_output) # add the curve fits to the larger data frame

### Curve206_data
Curve206_data <- subset(aci.df, unique_id == aci.df.unique_id[206]) # find correct curve from full dataframe and make new object
plot(Curve206_data$A~Curve206_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve206_fit <- fitaci(Curve206_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve206_fit) # take a look at fitted values, adjust as needed
plot(Curve206_fit) # plot the fitted curves over the raw data, adjust as needed
Curve206_output <- cbind('Curve206', Curve206_data$id[1], Curve206_data$unique_id[1], Curve206_data$machine[1], Curve206_data$baseline_yn[1],
                         Curve206_data$A[1], Curve206_data$Ci[1], Curve206_data$gsw[1],
                         mean(Curve206_data$VPDleaf, na.rm = T), mean(Curve206_data$Tleaf, na.rm = T), mean(Curve206_data$Qin, na.rm = T),
                         Curve206_fit[[2]][1,1], Curve206_fit[[2]][1,2],
                         Curve206_fit[[2]][2,1], Curve206_fit[[2]][2,2],
                         Curve206_fit[[2]][3,1], Curve206_fit[[2]][3,2],
                         Curve206_fit$RMSE,
                         Curve206_fit$Ci_transition,
                         Curve206_fit$citransition,
                         Curve206_fit$Km,
                         Curve206_fit$GammaStar,
                         Curve206_fit$fitmethod,
                         Curve206_fit$Tcorrect,
                         Curve206_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve206_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve206_output) # add the curve fits to the larger data frame

### Curve207_data
Curve207_data <- subset(aci.df, unique_id == aci.df.unique_id[207]) # find correct curve from full dataframe and make new object
plot(Curve207_data$A~Curve207_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve207_fit <- fitaci(Curve207_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve207_fit) # take a look at fitted values, adjust as needed
plot(Curve207_fit) # plot the fitted curves over the raw data, adjust as needed
Curve207_output <- cbind('Curve207', Curve207_data$id[1], Curve207_data$unique_id[1], Curve207_data$machine[1], Curve207_data$baseline_yn[1],
                         Curve207_data$A[1], Curve207_data$Ci[1], Curve207_data$gsw[1],
                         mean(Curve207_data$VPDleaf, na.rm = T), mean(Curve207_data$Tleaf, na.rm = T), mean(Curve207_data$Qin, na.rm = T),
                         Curve207_fit[[2]][1,1], Curve207_fit[[2]][1,2],
                         Curve207_fit[[2]][2,1], Curve207_fit[[2]][2,2],
                         Curve207_fit[[2]][3,1], Curve207_fit[[2]][3,2],
                         Curve207_fit$RMSE,
                         Curve207_fit$Ci_transition,
                         Curve207_fit$citransition,
                         Curve207_fit$Km,
                         Curve207_fit$GammaStar,
                         Curve207_fit$fitmethod,
                         Curve207_fit$Tcorrect,
                         Curve207_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve207_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve207_output) # add the curve fits to the larger data frame

### Curve208_data
Curve208_data <- subset(aci.df, unique_id == aci.df.unique_id[208]) # find correct curve from full dataframe and make new object
plot(Curve208_data$A~Curve208_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve208_fit <- fitaci(Curve208_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve208_fit) # take a look at fitted values, adjust as needed
plot(Curve208_fit) # plot the fitted curves over the raw data, adjust as needed
Curve208_output <- cbind('Curve208', Curve208_data$id[1], Curve208_data$unique_id[1], Curve208_data$machine[1], Curve208_data$baseline_yn[1],
                         Curve208_data$A[1], Curve208_data$Ci[1], Curve208_data$gsw[1],
                         mean(Curve208_data$VPDleaf, na.rm = T), mean(Curve208_data$Tleaf, na.rm = T), mean(Curve208_data$Qin, na.rm = T),
                         Curve208_fit[[2]][1,1], Curve208_fit[[2]][1,2],
                         Curve208_fit[[2]][2,1], Curve208_fit[[2]][2,2],
                         Curve208_fit[[2]][3,1], Curve208_fit[[2]][3,2],
                         Curve208_fit$RMSE,
                         Curve208_fit$Ci_transition,
                         Curve208_fit$citransition,
                         Curve208_fit$Km,
                         Curve208_fit$GammaStar,
                         Curve208_fit$fitmethod,
                         Curve208_fit$Tcorrect,
                         Curve208_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve208_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve208_output) # add the curve fits to the larger data frame

### Curve209_data
Curve209_data <- subset(aci.df, unique_id == aci.df.unique_id[209]) # find correct curve from full dataframe and make new object
plot(Curve209_data$A~Curve209_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve209_fit <- fitaci(Curve209_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve209_fit) # take a look at fitted values, adjust as needed
plot(Curve209_fit) # plot the fitted curves over the raw data, adjust as needed
Curve209_output <- cbind('Curve209', Curve209_data$id[1], Curve209_data$unique_id[1], Curve209_data$machine[1], Curve209_data$baseline_yn[1],
                         Curve209_data$A[1], Curve209_data$Ci[1], Curve209_data$gsw[1],
                         mean(Curve209_data$VPDleaf, na.rm = T), mean(Curve209_data$Tleaf, na.rm = T), mean(Curve209_data$Qin, na.rm = T),
                         Curve209_fit[[2]][1,1], Curve209_fit[[2]][1,2],
                         Curve209_fit[[2]][2,1], Curve209_fit[[2]][2,2],
                         Curve209_fit[[2]][3,1], Curve209_fit[[2]][3,2],
                         Curve209_fit$RMSE,
                         Curve209_fit$Ci_transition,
                         Curve209_fit$citransition,
                         Curve209_fit$Km,
                         Curve209_fit$GammaStar,
                         Curve209_fit$fitmethod,
                         Curve209_fit$Tcorrect,
                         Curve209_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve209_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve209_output) # add the curve fits to the larger data frame

### Curve210_data
Curve210_data <- subset(aci.df, unique_id == aci.df.unique_id[210]) # find correct curve from full dataframe and make new object
plot(Curve210_data$A~Curve210_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve210_fit <- fitaci(Curve210_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve210_fit) # take a look at fitted values, adjust as needed
plot(Curve210_fit) # plot the fitted curves over the raw data, adjust as needed
Curve210_output <- cbind('Curve210', Curve210_data$id[1], Curve210_data$unique_id[1], Curve210_data$machine[1], Curve210_data$baseline_yn[1],
                         Curve210_data$A[1], Curve210_data$Ci[1], Curve210_data$gsw[1],
                         mean(Curve210_data$VPDleaf, na.rm = T), mean(Curve210_data$Tleaf, na.rm = T), mean(Curve210_data$Qin, na.rm = T),
                         Curve210_fit[[2]][1,1], Curve210_fit[[2]][1,2],
                         Curve210_fit[[2]][2,1], Curve210_fit[[2]][2,2],
                         Curve210_fit[[2]][3,1], Curve210_fit[[2]][3,2],
                         Curve210_fit$RMSE,
                         Curve210_fit$Ci_transition,
                         Curve210_fit$citransition,
                         Curve210_fit$Km,
                         Curve210_fit$GammaStar,
                         Curve210_fit$fitmethod,
                         Curve210_fit$Tcorrect,
                         Curve210_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve210_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve210_output) # add the curve fits to the larger data frame

### Curve211_data
Curve211_data <- subset(aci.df, unique_id == aci.df.unique_id[211] & Ci < 1000) # find correct curve from full dataframe and make new object
plot(Curve211_data$A~Curve211_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve211_fit <- fitaci(Curve211_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve211_fit) # take a look at fitted values, adjust as needed
plot(Curve211_fit) # plot the fitted curves over the raw data, adjust as needed
Curve211_output <- cbind('Curve211', Curve211_data$id[1], Curve211_data$unique_id[1], Curve211_data$machine[1], Curve211_data$baseline_yn[1],
                         Curve211_data$A[1], Curve211_data$Ci[1], Curve211_data$gsw[1],
                         mean(Curve211_data$VPDleaf, na.rm = T), mean(Curve211_data$Tleaf, na.rm = T), mean(Curve211_data$Qin, na.rm = T),
                         Curve211_fit[[2]][1,1], Curve211_fit[[2]][1,2],
                         Curve211_fit[[2]][2,1], Curve211_fit[[2]][2,2],
                         Curve211_fit[[2]][3,1], Curve211_fit[[2]][3,2],
                         Curve211_fit$RMSE,
                         Curve211_fit$Ci_transition,
                         Curve211_fit$citransition,
                         Curve211_fit$Km,
                         Curve211_fit$GammaStar,
                         Curve211_fit$fitmethod,
                         Curve211_fit$Tcorrect,
                         Curve211_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve211_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve211_output) # add the curve fits to the larger data frame

### Curve212_data
Curve212_data <- subset(aci.df, unique_id == aci.df.unique_id[212] & Ci < 1000) # find correct curve from full dataframe and make new object
plot(Curve212_data$A~Curve212_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve212_fit <- fitaci(Curve212_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve212_fit) # take a look at fitted values, adjust as needed
plot(Curve212_fit) # plot the fitted curves over the raw data, adjust as needed
Curve212_output <- cbind('Curve212', Curve212_data$id[1], Curve212_data$unique_id[1], Curve212_data$machine[1], Curve212_data$baseline_yn[1],
                         Curve212_data$A[1], Curve212_data$Ci[1], Curve212_data$gsw[1],
                         mean(Curve212_data$VPDleaf, na.rm = T), mean(Curve212_data$Tleaf, na.rm = T), mean(Curve212_data$Qin, na.rm = T),
                         Curve212_fit[[2]][1,1], Curve212_fit[[2]][1,2],
                         Curve212_fit[[2]][2,1], Curve212_fit[[2]][2,2],
                         Curve212_fit[[2]][3,1], Curve212_fit[[2]][3,2],
                         Curve212_fit$RMSE,
                         Curve212_fit$Ci_transition,
                         Curve212_fit$citransition,
                         Curve212_fit$Km,
                         Curve212_fit$GammaStar,
                         Curve212_fit$fitmethod,
                         Curve212_fit$Tcorrect,
                         Curve212_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve212_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve212_output) # add the curve fits to the larger data frame

### Curve213_data
Curve213_data <- subset(aci.df, unique_id == aci.df.unique_id[213]) # find correct curve from full dataframe and make new object
plot(Curve213_data$A~Curve213_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve213_fit <- fitaci(Curve213_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve213_fit) # take a look at fitted values, adjust as needed
plot(Curve213_fit) # plot the fitted curves over the raw data, adjust as needed
Curve213_output <- cbind('Curve213', Curve213_data$id[1], Curve213_data$unique_id[1], Curve213_data$machine[1], Curve213_data$baseline_yn[1],
                         Curve213_data$A[1], Curve213_data$Ci[1], Curve213_data$gsw[1],
                         mean(Curve213_data$VPDleaf, na.rm = T), mean(Curve213_data$Tleaf, na.rm = T), mean(Curve213_data$Qin, na.rm = T),
                         Curve213_fit[[2]][1,1], Curve213_fit[[2]][1,2],
                         Curve213_fit[[2]][2,1], Curve213_fit[[2]][2,2],
                         Curve213_fit[[2]][3,1], Curve213_fit[[2]][3,2],
                         Curve213_fit$RMSE,
                         Curve213_fit$Ci_transition,
                         Curve213_fit$citransition,
                         Curve213_fit$Km,
                         Curve213_fit$GammaStar,
                         Curve213_fit$fitmethod,
                         Curve213_fit$Tcorrect,
                         Curve213_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve213_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve213_output) # add the curve fits to the larger data frame

### Curve214_data
Curve214_data <- subset(aci.df, unique_id == aci.df.unique_id[214]) # find correct curve from full dataframe and make new object
plot(Curve214_data$A~Curve214_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve214_fit <- fitaci(Curve214_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve214_fit) # take a look at fitted values, adjust as needed
plot(Curve214_fit) # plot the fitted curves over the raw data, adjust as needed
Curve214_output <- cbind('Curve214', Curve214_data$id[1], Curve214_data$unique_id[1], Curve214_data$machine[1], Curve214_data$baseline_yn[1],
                         Curve214_data$A[1], Curve214_data$Ci[1], Curve214_data$gsw[1],
                         mean(Curve214_data$VPDleaf, na.rm = T), mean(Curve214_data$Tleaf, na.rm = T), mean(Curve214_data$Qin, na.rm = T),
                         Curve214_fit[[2]][1,1], Curve214_fit[[2]][1,2],
                         Curve214_fit[[2]][2,1], Curve214_fit[[2]][2,2],
                         Curve214_fit[[2]][3,1], Curve214_fit[[2]][3,2],
                         Curve214_fit$RMSE,
                         Curve214_fit$Ci_transition,
                         Curve214_fit$citransition,
                         Curve214_fit$Km,
                         Curve214_fit$GammaStar,
                         Curve214_fit$fitmethod,
                         Curve214_fit$Tcorrect,
                         Curve214_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve214_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve214_output) # add the curve fits to the larger data frame

### Curve215_data
Curve215_data <- subset(aci.df, unique_id == aci.df.unique_id[215]) # find correct curve from full dataframe and make new object
plot(Curve215_data$A~Curve215_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve215_fit <- fitaci(Curve215_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve215_fit) # take a look at fitted values, adjust as needed
plot(Curve215_fit) # plot the fitted curves over the raw data, adjust as needed
Curve215_output <- cbind('Curve215', Curve215_data$id[1], Curve215_data$unique_id[1], Curve215_data$machine[1], Curve215_data$baseline_yn[1],
                         Curve215_data$A[1], Curve215_data$Ci[1], Curve215_data$gsw[1],
                         mean(Curve215_data$VPDleaf, na.rm = T), mean(Curve215_data$Tleaf, na.rm = T), mean(Curve215_data$Qin, na.rm = T),
                         Curve215_fit[[2]][1,1], Curve215_fit[[2]][1,2],
                         Curve215_fit[[2]][2,1], Curve215_fit[[2]][2,2],
                         Curve215_fit[[2]][3,1], Curve215_fit[[2]][3,2],
                         Curve215_fit$RMSE,
                         Curve215_fit$Ci_transition,
                         Curve215_fit$citransition,
                         Curve215_fit$Km,
                         Curve215_fit$GammaStar,
                         Curve215_fit$fitmethod,
                         Curve215_fit$Tcorrect,
                         Curve215_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve215_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve215_output) # add the curve fits to the larger data frame

### Curve215_data
Curve215_data <- subset(aci.df, unique_id == aci.df.unique_id[215]) # find correct curve from full dataframe and make new object
plot(Curve215_data$A~Curve215_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve215_fit <- fitaci(Curve215_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve215_fit) # take a look at fitted values, adjust as needed
plot(Curve215_fit) # plot the fitted curves over the raw data, adjust as needed
Curve215_output <- cbind('Curve215', Curve215_data$id[1], Curve215_data$unique_id[1], Curve215_data$machine[1], Curve215_data$baseline_yn[1],
                         Curve215_data$A[1], Curve215_data$Ci[1], Curve215_data$gsw[1],
                         mean(Curve215_data$VPDleaf, na.rm = T), mean(Curve215_data$Tleaf, na.rm = T), mean(Curve215_data$Qin, na.rm = T),
                         Curve215_fit[[2]][1,1], Curve215_fit[[2]][1,2],
                         Curve215_fit[[2]][2,1], Curve215_fit[[2]][2,2],
                         Curve215_fit[[2]][3,1], Curve215_fit[[2]][3,2],
                         Curve215_fit$RMSE,
                         Curve215_fit$Ci_transition,
                         Curve215_fit$citransition,
                         Curve215_fit$Km,
                         Curve215_fit$GammaStar,
                         Curve215_fit$fitmethod,
                         Curve215_fit$Tcorrect,
                         Curve215_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve215_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve215_output) # add the curve fits to the larger data frame

### Curve216_data
Curve216_data <- subset(aci.df, unique_id == aci.df.unique_id[216]) # find correct curve from full dataframe and make new object
plot(Curve216_data$A~Curve216_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve216_fit <- fitaci(Curve216_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve216_fit) # take a look at fitted values, adjust as needed
plot(Curve216_fit) # plot the fitted curves over the raw data, adjust as needed
Curve216_output <- cbind('Curve216', Curve216_data$id[1], Curve216_data$unique_id[1], Curve216_data$machine[1], Curve216_data$baseline_yn[1],
                         Curve216_data$A[1], Curve216_data$Ci[1], Curve216_data$gsw[1],
                         mean(Curve216_data$VPDleaf, na.rm = T), mean(Curve216_data$Tleaf, na.rm = T), mean(Curve216_data$Qin, na.rm = T),
                         Curve216_fit[[2]][1,1], Curve216_fit[[2]][1,2],
                         Curve216_fit[[2]][2,1], Curve216_fit[[2]][2,2],
                         Curve216_fit[[2]][3,1], Curve216_fit[[2]][3,2],
                         Curve216_fit$RMSE,
                         Curve216_fit$Ci_transition,
                         Curve216_fit$citransition,
                         Curve216_fit$Km,
                         Curve216_fit$GammaStar,
                         Curve216_fit$fitmethod,
                         Curve216_fit$Tcorrect,
                         Curve216_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve216_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve216_output) # add the curve fits to the larger data frame

### Curve217_data
Curve217_data <- subset(aci.df, unique_id == aci.df.unique_id[217]) # find correct curve from full dataframe and make new object
plot(Curve217_data$A~Curve217_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve217_fit <- fitaci(Curve217_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve217_fit) # take a look at fitted values, adjust as needed
plot(Curve217_fit) # plot the fitted curves over the raw data, adjust as needed
Curve217_output <- cbind('Curve217', Curve217_data$id[1], Curve217_data$unique_id[1], Curve217_data$machine[1], Curve217_data$baseline_yn[1],
                         Curve217_data$A[1], Curve217_data$Ci[1], Curve217_data$gsw[1],
                         mean(Curve217_data$VPDleaf, na.rm = T), mean(Curve217_data$Tleaf, na.rm = T), mean(Curve217_data$Qin, na.rm = T),
                         Curve217_fit[[2]][1,1], Curve217_fit[[2]][1,2],
                         Curve217_fit[[2]][2,1], Curve217_fit[[2]][2,2],
                         Curve217_fit[[2]][3,1], Curve217_fit[[2]][3,2],
                         Curve217_fit$RMSE,
                         Curve217_fit$Ci_transition,
                         Curve217_fit$citransition,
                         Curve217_fit$Km,
                         Curve217_fit$GammaStar,
                         Curve217_fit$fitmethod,
                         Curve217_fit$Tcorrect,
                         Curve217_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve217_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve217_output) # add the curve fits to the larger data frame

### Curve218_data
Curve218_data <- subset(aci.df, unique_id == aci.df.unique_id[218]) # find correct curve from full dataframe and make new object
plot(Curve218_data$A~Curve218_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve218_fit <- fitaci(Curve218_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve218_fit) # take a look at fitted values, adjust as needed
plot(Curve218_fit) # plot the fitted curves over the raw data, adjust as needed
Curve218_output <- cbind('Curve218', Curve218_data$id[1], Curve218_data$unique_id[1], Curve218_data$machine[1], Curve218_data$baseline_yn[1],
                         Curve218_data$A[1], Curve218_data$Ci[1], Curve218_data$gsw[1],
                         mean(Curve218_data$VPDleaf, na.rm = T), mean(Curve218_data$Tleaf, na.rm = T), mean(Curve218_data$Qin, na.rm = T),
                         Curve218_fit[[2]][1,1], Curve218_fit[[2]][1,2],
                         Curve218_fit[[2]][2,1], Curve218_fit[[2]][2,2],
                         Curve218_fit[[2]][3,1], Curve218_fit[[2]][3,2],
                         Curve218_fit$RMSE,
                         Curve218_fit$Ci_transition,
                         Curve218_fit$citransition,
                         Curve218_fit$Km,
                         Curve218_fit$GammaStar,
                         Curve218_fit$fitmethod,
                         Curve218_fit$Tcorrect,
                         Curve218_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve218_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve218_output) # add the curve fits to the larger data frame

### Curve219_data
Curve219_data <- subset(aci.df, unique_id == aci.df.unique_id[219] & Ci < 1000) # find correct curve from full dataframe and make new object
plot(Curve219_data$A~Curve219_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve219_fit <- fitaci(Curve219_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve219_fit) # take a look at fitted values, adjust as needed
plot(Curve219_fit) # plot the fitted curves over the raw data, adjust as needed
Curve219_output <- cbind('Curve219', Curve219_data$id[1], Curve219_data$unique_id[1], Curve219_data$machine[1], Curve219_data$baseline_yn[1],
                         Curve219_data$A[1], Curve219_data$Ci[1], Curve219_data$gsw[1],
                         mean(Curve219_data$VPDleaf, na.rm = T), mean(Curve219_data$Tleaf, na.rm = T), mean(Curve219_data$Qin, na.rm = T),
                         Curve219_fit[[2]][1,1], Curve219_fit[[2]][1,2],
                         Curve219_fit[[2]][2,1], Curve219_fit[[2]][2,2],
                         Curve219_fit[[2]][3,1], Curve219_fit[[2]][3,2],
                         Curve219_fit$RMSE,
                         Curve219_fit$Ci_transition,
                         Curve219_fit$citransition,
                         Curve219_fit$Km,
                         Curve219_fit$GammaStar,
                         Curve219_fit$fitmethod,
                         Curve219_fit$Tcorrect,
                         Curve219_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve219_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve219_output) # add the curve fits to the larger data frame

### Curve220_data
Curve220_data <- subset(aci.df, unique_id == aci.df.unique_id[220]) # find correct curve from full dataframe and make new object
plot(Curve220_data$A~Curve220_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve220_fit <- fitaci(Curve220_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve220_fit) # take a look at fitted values, adjust as needed
plot(Curve220_fit) # plot the fitted curves over the raw data, adjust as needed
Curve220_output <- cbind('Curve220', Curve220_data$id[1], Curve220_data$unique_id[1], Curve220_data$machine[1], Curve220_data$baseline_yn[1],
                         Curve220_data$A[1], Curve220_data$Ci[1], Curve220_data$gsw[1],
                         mean(Curve220_data$VPDleaf, na.rm = T), mean(Curve220_data$Tleaf, na.rm = T), mean(Curve220_data$Qin, na.rm = T),
                         Curve220_fit[[2]][1,1], Curve220_fit[[2]][1,2],
                         Curve220_fit[[2]][2,1], Curve220_fit[[2]][2,2],
                         Curve220_fit[[2]][3,1], Curve220_fit[[2]][3,2],
                         Curve220_fit$RMSE,
                         Curve220_fit$Ci_transition,
                         Curve220_fit$citransition,
                         Curve220_fit$Km,
                         Curve220_fit$GammaStar,
                         Curve220_fit$fitmethod,
                         Curve220_fit$Tcorrect,
                         Curve220_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve220_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve220_output) # add the curve fits to the larger data frame

### Curve221_data
Curve221_data <- subset(aci.df, unique_id == aci.df.unique_id[221] & Ci < 1000) # find correct curve from full dataframe and make new object
plot(Curve221_data$A~Curve221_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve221_fit <- fitaci(Curve221_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve221_fit) # take a look at fitted values, adjust as needed
plot(Curve221_fit) # plot the fitted curves over the raw data, adjust as needed
Curve221_output <- cbind('Curve221', Curve221_data$id[1], Curve221_data$unique_id[1], Curve221_data$machine[1], Curve221_data$baseline_yn[1],
                         Curve221_data$A[1], Curve221_data$Ci[1], Curve221_data$gsw[1],
                         mean(Curve221_data$VPDleaf, na.rm = T), mean(Curve221_data$Tleaf, na.rm = T), mean(Curve221_data$Qin, na.rm = T),
                         Curve221_fit[[2]][1,1], Curve221_fit[[2]][1,2],
                         Curve221_fit[[2]][2,1], Curve221_fit[[2]][2,2],
                         Curve221_fit[[2]][3,1], Curve221_fit[[2]][3,2],
                         Curve221_fit$RMSE,
                         Curve221_fit$Ci_transition,
                         Curve221_fit$citransition,
                         Curve221_fit$Km,
                         Curve221_fit$GammaStar,
                         Curve221_fit$fitmethod,
                         Curve221_fit$Tcorrect,
                         Curve221_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve221_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve221_output) # add the curve fits to the larger data frame

### Curve222_data
Curve222_data <- subset(aci.df, unique_id == aci.df.unique_id[222] & Ci < 1000) # find correct curve from full dataframe and make new object
plot(Curve222_data$A~Curve222_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve222_fit <- fitaci(Curve222_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve222_fit) # take a look at fitted values, adjust as needed
plot(Curve222_fit) # plot the fitted curves over the raw data, adjust as needed
Curve222_output <- cbind('Curve222', Curve222_data$id[1], Curve222_data$unique_id[1], Curve222_data$machine[1], Curve222_data$baseline_yn[1],
                         Curve222_data$A[1], Curve222_data$Ci[1], Curve222_data$gsw[1],
                         mean(Curve222_data$VPDleaf, na.rm = T), mean(Curve222_data$Tleaf, na.rm = T), mean(Curve222_data$Qin, na.rm = T),
                         Curve222_fit[[2]][1,1], Curve222_fit[[2]][1,2],
                         Curve222_fit[[2]][2,1], Curve222_fit[[2]][2,2],
                         Curve222_fit[[2]][3,1], Curve222_fit[[2]][3,2],
                         Curve222_fit$RMSE,
                         Curve222_fit$Ci_transition,
                         Curve222_fit$citransition,
                         Curve222_fit$Km,
                         Curve222_fit$GammaStar,
                         Curve222_fit$fitmethod,
                         Curve222_fit$Tcorrect,
                         Curve222_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve222_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve222_output) # add the curve fits to the larger data frame

Curve223_data <- subset(aci.df, unique_id == aci.df.unique_id[223]) # find correct curve from full dataframe and make new object
plot(Curve223_data$A~Curve223_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve223_fit <- fitaci(Curve223_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve223_fit) # take a look at fitted values, adjust as needed
plot(Curve223_fit) # plot the fitted curves over the raw data, adjust as needed
Curve223_output <- cbind('Curve223', Curve223_data$id[1], Curve223_data$unique_id[1], Curve223_data$machine[1], Curve223_data$baseline_yn[1],
                         Curve223_data$A[1], Curve223_data$Ci[1], Curve223_data$gsw[1],
                         mean(Curve223_data$VPDleaf, na.rm = T), mean(Curve223_data$Tleaf, na.rm = T), mean(Curve223_data$Qin, na.rm = T),
                         Curve223_fit[[2]][1,1], Curve223_fit[[2]][1,2],
                         Curve223_fit[[2]][2,1], Curve223_fit[[2]][2,2],
                         Curve223_fit[[2]][3,1], Curve223_fit[[2]][3,2],
                         Curve223_fit$RMSE,
                         Curve223_fit$Ci_transition,
                         Curve223_fit$citransition,
                         Curve223_fit$Km,
                         Curve223_fit$GammaStar,
                         Curve223_fit$fitmethod,
                         Curve223_fit$Tcorrect,
                         Curve223_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve223_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve223_output) # add the curve fits to the larger data frame

### Curve224_data
Curve224_data <- subset(aci.df, unique_id == aci.df.unique_id[224] & Ci < 1000) # find correct curve from full dataframe and make new object
plot(Curve224_data$A~Curve224_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve224_fit <- fitaci(Curve224_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve224_fit) # take a look at fitted values, adjust as needed
plot(Curve224_fit) # plot the fitted curves over the raw data, adjust as needed
Curve224_output <- cbind('Curve224', Curve224_data$id[1], Curve224_data$unique_id[1], Curve224_data$machine[1], Curve224_data$baseline_yn[1],
                         Curve224_data$A[1], Curve224_data$Ci[1], Curve224_data$gsw[1],
                         mean(Curve224_data$VPDleaf, na.rm = T), mean(Curve224_data$Tleaf, na.rm = T), mean(Curve224_data$Qin, na.rm = T),
                         Curve224_fit[[2]][1,1], Curve224_fit[[2]][1,2],
                         Curve224_fit[[2]][2,1], Curve224_fit[[2]][2,2],
                         Curve224_fit[[2]][3,1], Curve224_fit[[2]][3,2],
                         Curve224_fit$RMSE,
                         Curve224_fit$Ci_transition,
                         Curve224_fit$citransition,
                         Curve224_fit$Km,
                         Curve224_fit$GammaStar,
                         Curve224_fit$fitmethod,
                         Curve224_fit$Tcorrect,
                         Curve224_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve224_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve224_output) # add the curve fits to the larger data frame

### Curve225_data
Curve225_data <- subset(aci.df, unique_id == aci.df.unique_id[225] & Ci < 1000) # find correct curve from full dataframe and make new object
plot(Curve225_data$A~Curve225_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve225_fit <- fitaci(Curve225_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve225_fit) # take a look at fitted values, adjust as needed
plot(Curve225_fit) # plot the fitted curves over the raw data, adjust as needed
Curve225_output <- cbind('Curve225', Curve225_data$id[1], Curve225_data$unique_id[1], Curve225_data$machine[1], Curve225_data$baseline_yn[1],
                         Curve225_data$A[1], Curve225_data$Ci[1], Curve225_data$gsw[1],
                         mean(Curve225_data$VPDleaf, na.rm = T), mean(Curve225_data$Tleaf, na.rm = T), mean(Curve225_data$Qin, na.rm = T),
                         Curve225_fit[[2]][1,1], Curve225_fit[[2]][1,2],
                         Curve225_fit[[2]][2,1], Curve225_fit[[2]][2,2],
                         Curve225_fit[[2]][3,1], Curve225_fit[[2]][3,2],
                         Curve225_fit$RMSE,
                         Curve225_fit$Ci_transition,
                         Curve225_fit$citransition,
                         Curve225_fit$Km,
                         Curve225_fit$GammaStar,
                         Curve225_fit$fitmethod,
                         Curve225_fit$Tcorrect,
                         Curve225_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve225_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve225_output) # add the curve fits to the larger data frame

### Curve226_data
Curve226_data <- subset(aci.df, unique_id == aci.df.unique_id[226] & Ci < 1000) # find correct curve from full dataframe and make new object
plot(Curve226_data$A~Curve226_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve226_fit <- fitaci(Curve226_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve226_fit) # take a look at fitted values, adjust as needed
plot(Curve226_fit) # plot the fitted curves over the raw data, adjust as needed
Curve226_output <- cbind('Curve226', Curve226_data$id[1], Curve226_data$unique_id[1], Curve226_data$machine[1], Curve226_data$baseline_yn[1],
                         Curve226_data$A[1], Curve226_data$Ci[1], Curve226_data$gsw[1],
                         mean(Curve226_data$VPDleaf, na.rm = T), mean(Curve226_data$Tleaf, na.rm = T), mean(Curve226_data$Qin, na.rm = T),
                         Curve226_fit[[2]][1,1], Curve226_fit[[2]][1,2],
                         Curve226_fit[[2]][2,1], Curve226_fit[[2]][2,2],
                         Curve226_fit[[2]][3,1], Curve226_fit[[2]][3,2],
                         Curve226_fit$RMSE,
                         Curve226_fit$Ci_transition,
                         Curve226_fit$citransition,
                         Curve226_fit$Km,
                         Curve226_fit$GammaStar,
                         Curve226_fit$fitmethod,
                         Curve226_fit$Tcorrect,
                         Curve226_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve226_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve226_output) # add the curve fits to the larger data frame

### Curve227_data
Curve227_data <- subset(aci.df, unique_id == aci.df.unique_id[227] & Ci < 1000) # find correct curve from full dataframe and make new object
plot(Curve227_data$A~Curve227_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve227_fit <- fitaci(Curve227_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve227_fit) # take a look at fitted values, adjust as needed
plot(Curve227_fit) # plot the fitted curves over the raw data, adjust as needed
Curve227_output <- cbind('Curve227', Curve227_data$id[1], Curve227_data$unique_id[1], Curve227_data$machine[1], Curve227_data$baseline_yn[1],
                         Curve227_data$A[1], Curve227_data$Ci[1], Curve227_data$gsw[1],
                         mean(Curve227_data$VPDleaf, na.rm = T), mean(Curve227_data$Tleaf, na.rm = T), mean(Curve227_data$Qin, na.rm = T),
                         Curve227_fit[[2]][1,1], Curve227_fit[[2]][1,2],
                         Curve227_fit[[2]][2,1], Curve227_fit[[2]][2,2],
                         Curve227_fit[[2]][3,1], Curve227_fit[[2]][3,2],
                         Curve227_fit$RMSE,
                         Curve227_fit$Ci_transition,
                         Curve227_fit$citransition,
                         Curve227_fit$Km,
                         Curve227_fit$GammaStar,
                         Curve227_fit$fitmethod,
                         Curve227_fit$Tcorrect,
                         Curve227_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve227_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve227_output) # add the curve fits to the larger data frame

### Curve228_data
Curve228_data <- subset(aci.df, unique_id == aci.df.unique_id[228]) # find correct curve from full dataframe and make new object
plot(Curve228_data$A~Curve228_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve228_fit <- fitaci(Curve228_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve228_fit) # take a look at fitted values, adjust as needed
plot(Curve228_fit) # plot the fitted curves over the raw data, adjust as needed
Curve228_output <- cbind('Curve228', Curve228_data$id[1], Curve228_data$unique_id[1], Curve228_data$machine[1], Curve228_data$baseline_yn[1],
                         Curve228_data$A[1], Curve228_data$Ci[1], Curve228_data$gsw[1],
                         mean(Curve228_data$VPDleaf, na.rm = T), mean(Curve228_data$Tleaf, na.rm = T), mean(Curve228_data$Qin, na.rm = T),
                         Curve228_fit[[2]][1,1], Curve228_fit[[2]][1,2],
                         Curve228_fit[[2]][2,1], Curve228_fit[[2]][2,2],
                         Curve228_fit[[2]][3,1], Curve228_fit[[2]][3,2],
                         Curve228_fit$RMSE,
                         Curve228_fit$Ci_transition,
                         Curve228_fit$citransition,
                         Curve228_fit$Km,
                         Curve228_fit$GammaStar,
                         Curve228_fit$fitmethod,
                         Curve228_fit$Tcorrect,
                         Curve228_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve228_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve228_output) # add the curve fits to the larger data frame

### Curve229_data
Curve229_data <- subset(aci.df, unique_id == aci.df.unique_id[229]) # find correct curve from full dataframe and make new object
plot(Curve229_data$A~Curve229_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve229_fit <- fitaci(Curve229_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve229_fit) # take a look at fitted values, adjust as needed
plot(Curve229_fit) # plot the fitted curves over the raw data, adjust as needed
Curve229_output <- cbind('Curve229', Curve229_data$id[1], Curve229_data$unique_id[1], Curve229_data$machine[1], Curve229_data$baseline_yn[1],
                         Curve229_data$A[1], Curve229_data$Ci[1], Curve229_data$gsw[1],
                         mean(Curve229_data$VPDleaf, na.rm = T), mean(Curve229_data$Tleaf, na.rm = T), mean(Curve229_data$Qin, na.rm = T),
                         Curve229_fit[[2]][1,1], Curve229_fit[[2]][1,2],
                         Curve229_fit[[2]][2,1], Curve229_fit[[2]][2,2],
                         Curve229_fit[[2]][3,1], Curve229_fit[[2]][3,2],
                         Curve229_fit$RMSE,
                         Curve229_fit$Ci_transition,
                         Curve229_fit$citransition,
                         Curve229_fit$Km,
                         Curve229_fit$GammaStar,
                         Curve229_fit$fitmethod,
                         Curve229_fit$Tcorrect,
                         Curve229_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve229_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve229_output) # add the curve fits to the larger data frame

### Curve230_data
Curve230_data <- subset(aci.df, unique_id == aci.df.unique_id[230]) # find correct curve from full dataframe and make new object
plot(Curve230_data$A~Curve230_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve230_fit <- fitaci(Curve230_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve230_fit) # take a look at fitted values, adjust as needed
plot(Curve230_fit) # plot the fitted curves over the raw data, adjust as needed
Curve230_output <- cbind('Curve230', Curve230_data$id[1], Curve230_data$unique_id[1], Curve230_data$machine[1], Curve230_data$baseline_yn[1],
                         Curve230_data$A[1], Curve230_data$Ci[1], Curve230_data$gsw[1],
                         mean(Curve230_data$VPDleaf, na.rm = T), mean(Curve230_data$Tleaf, na.rm = T), mean(Curve230_data$Qin, na.rm = T),
                         Curve230_fit[[2]][1,1], Curve230_fit[[2]][1,2],
                         Curve230_fit[[2]][2,1], Curve230_fit[[2]][2,2],
                         Curve230_fit[[2]][3,1], Curve230_fit[[2]][3,2],
                         Curve230_fit$RMSE,
                         Curve230_fit$Ci_transition,
                         Curve230_fit$citransition,
                         Curve230_fit$Km,
                         Curve230_fit$GammaStar,
                         Curve230_fit$fitmethod,
                         Curve230_fit$Tcorrect,
                         Curve230_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve230_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve230_output) # add the curve fits to the larger data frame

### Curve231_data
Curve231_data <- subset(aci.df, unique_id == aci.df.unique_id[231] & Ci < 1000) # find correct curve from full dataframe and make new object
plot(Curve231_data$A~Curve231_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve231_fit <- fitaci(Curve231_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve231_fit) # take a look at fitted values, adjust as needed
plot(Curve231_fit) # plot the fitted curves over the raw data, adjust as needed
Curve231_output <- cbind('Curve231', Curve231_data$id[1], Curve231_data$unique_id[1], Curve231_data$machine[1], Curve231_data$baseline_yn[1],
                         Curve231_data$A[1], Curve231_data$Ci[1], Curve231_data$gsw[1],
                         mean(Curve231_data$VPDleaf, na.rm = T), mean(Curve231_data$Tleaf, na.rm = T), mean(Curve231_data$Qin, na.rm = T),
                         Curve231_fit[[2]][1,1], Curve231_fit[[2]][1,2],
                         Curve231_fit[[2]][2,1], Curve231_fit[[2]][2,2],
                         Curve231_fit[[2]][3,1], Curve231_fit[[2]][3,2],
                         Curve231_fit$RMSE,
                         Curve231_fit$Ci_transition,
                         Curve231_fit$citransition,
                         Curve231_fit$Km,
                         Curve231_fit$GammaStar,
                         Curve231_fit$fitmethod,
                         Curve231_fit$Tcorrect,
                         Curve231_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve231_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve231_output) # add the curve fits to the larger data frame

### Curve232_data
Curve232_data <- subset(aci.df, unique_id == aci.df.unique_id[232] & Ci < 1000) # find correct curve from full dataframe and make new object
plot(Curve232_data$A~Curve232_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve232_fit <- fitaci(Curve232_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve232_fit) # take a look at fitted values, adjust as needed
plot(Curve232_fit) # plot the fitted curves over the raw data, adjust as needed
Curve232_output <- cbind('Curve232', Curve232_data$id[1], Curve232_data$unique_id[1], Curve232_data$machine[1], Curve232_data$baseline_yn[1],
                         Curve232_data$A[1], Curve232_data$Ci[1], Curve232_data$gsw[1],
                         mean(Curve232_data$VPDleaf, na.rm = T), mean(Curve232_data$Tleaf, na.rm = T), mean(Curve232_data$Qin, na.rm = T),
                         Curve232_fit[[2]][1,1], Curve232_fit[[2]][1,2],
                         Curve232_fit[[2]][2,1], Curve232_fit[[2]][2,2],
                         Curve232_fit[[2]][3,1], Curve232_fit[[2]][3,2],
                         Curve232_fit$RMSE,
                         Curve232_fit$Ci_transition,
                         Curve232_fit$citransition,
                         Curve232_fit$Km,
                         Curve232_fit$GammaStar,
                         Curve232_fit$fitmethod,
                         Curve232_fit$Tcorrect,
                         Curve232_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve232_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve232_output) # add the curve fits to the larger data frame

### Curve233_data
Curve233_data <- subset(aci.df, unique_id == aci.df.unique_id[233]) # find correct curve from full dataframe and make new object
plot(Curve233_data$A~Curve233_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve233_fit <- fitaci(Curve233_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve233_fit) # take a look at fitted values, adjust as needed
plot(Curve233_fit) # plot the fitted curves over the raw data, adjust as needed
Curve233_output <- cbind('Curve233', Curve233_data$id[1], Curve233_data$unique_id[1], Curve233_data$machine[1], Curve233_data$baseline_yn[1],
                         Curve233_data$A[1], Curve233_data$Ci[1], Curve233_data$gsw[1],
                         mean(Curve233_data$VPDleaf, na.rm = T), mean(Curve233_data$Tleaf, na.rm = T), mean(Curve233_data$Qin, na.rm = T),
                         Curve233_fit[[2]][1,1], Curve233_fit[[2]][1,2],
                         Curve233_fit[[2]][2,1], Curve233_fit[[2]][2,2],
                         Curve233_fit[[2]][3,1], Curve233_fit[[2]][3,2],
                         Curve233_fit$RMSE,
                         Curve233_fit$Ci_transition,
                         Curve233_fit$citransition,
                         Curve233_fit$Km,
                         Curve233_fit$GammaStar,
                         Curve233_fit$fitmethod,
                         Curve233_fit$Tcorrect,
                         Curve233_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve233_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve233_output) # add the curve fits to the larger data frame

### Curve234_data
Curve234_data <- subset(aci.df, unique_id == aci.df.unique_id[234] & Ci < 1000) # find correct curve from full dataframe and make new object
plot(Curve234_data$A~Curve234_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve234_fit <- fitaci(Curve234_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve234_fit) # take a look at fitted values, adjust as needed
plot(Curve234_fit) # plot the fitted curves over the raw data, adjust as needed
Curve234_output <- cbind('Curve234', Curve234_data$id[1], Curve234_data$unique_id[1], Curve234_data$machine[1], Curve234_data$baseline_yn[1],
                         Curve234_data$A[1], Curve234_data$Ci[1], Curve234_data$gsw[1],
                         mean(Curve234_data$VPDleaf, na.rm = T), mean(Curve234_data$Tleaf, na.rm = T), mean(Curve234_data$Qin, na.rm = T),
                         Curve234_fit[[2]][1,1], Curve234_fit[[2]][1,2],
                         Curve234_fit[[2]][2,1], Curve234_fit[[2]][2,2],
                         Curve234_fit[[2]][3,1], Curve234_fit[[2]][3,2],
                         Curve234_fit$RMSE,
                         Curve234_fit$Ci_transition,
                         Curve234_fit$citransition,
                         Curve234_fit$Km,
                         Curve234_fit$GammaStar,
                         Curve234_fit$fitmethod,
                         Curve234_fit$Tcorrect,
                         Curve234_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve234_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve234_output) # add the curve fits to the larger data frame

### Curve235_data
Curve235_data <- subset(aci.df, unique_id == aci.df.unique_id[235] & Ci < 1000) # find correct curve from full dataframe and make new object
plot(Curve235_data$A~Curve235_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve235_fit <- fitaci(Curve235_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve235_fit) # take a look at fitted values, adjust as needed
plot(Curve235_fit) # plot the fitted curves over the raw data, adjust as needed
Curve235_output <- cbind('Curve235', Curve235_data$id[1], Curve235_data$unique_id[1], Curve235_data$machine[1], Curve235_data$baseline_yn[1],
                         Curve235_data$A[1], Curve235_data$Ci[1], Curve235_data$gsw[1],
                         mean(Curve235_data$VPDleaf, na.rm = T), mean(Curve235_data$Tleaf, na.rm = T), mean(Curve235_data$Qin, na.rm = T),
                         Curve235_fit[[2]][1,1], Curve235_fit[[2]][1,2],
                         Curve235_fit[[2]][2,1], Curve235_fit[[2]][2,2],
                         Curve235_fit[[2]][3,1], Curve235_fit[[2]][3,2],
                         Curve235_fit$RMSE,
                         Curve235_fit$Ci_transition,
                         Curve235_fit$citransition,
                         Curve235_fit$Km,
                         Curve235_fit$GammaStar,
                         Curve235_fit$fitmethod,
                         Curve235_fit$Tcorrect,
                         Curve235_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve235_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve235_output) # add the curve fits to the larger data frame

### Curve236_data
Curve236_data <- subset(aci.df, unique_id == aci.df.unique_id[236]) # find correct curve from full dataframe and make new object
plot(Curve236_data$A~Curve236_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve236_fit <- fitaci(Curve236_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve236_fit) # take a look at fitted values, adjust as needed
plot(Curve236_fit) # plot the fitted curves over the raw data, adjust as needed
Curve236_output <- cbind('Curve236', Curve236_data$id[1], Curve236_data$unique_id[1], Curve236_data$machine[1], Curve236_data$baseline_yn[1],
                         Curve236_data$A[1], Curve236_data$Ci[1], Curve236_data$gsw[1],
                         mean(Curve236_data$VPDleaf, na.rm = T), mean(Curve236_data$Tleaf, na.rm = T), mean(Curve236_data$Qin, na.rm = T),
                         Curve236_fit[[2]][1,1], Curve236_fit[[2]][1,2],
                         Curve236_fit[[2]][2,1], Curve236_fit[[2]][2,2],
                         Curve236_fit[[2]][3,1], Curve236_fit[[2]][3,2],
                         Curve236_fit$RMSE,
                         Curve236_fit$Ci_transition,
                         Curve236_fit$citransition,
                         Curve236_fit$Km,
                         Curve236_fit$GammaStar,
                         Curve236_fit$fitmethod,
                         Curve236_fit$Tcorrect,
                         Curve236_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve236_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve236_output) # add the curve fits to the larger data frame

### Curve237_data
Curve237_data <- subset(aci.df, unique_id == aci.df.unique_id[237]) # find correct curve from full dataframe and make new object
plot(Curve237_data$A~Curve237_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve237_fit <- fitaci(Curve237_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve237_fit) # take a look at fitted values, adjust as needed
plot(Curve237_fit) # plot the fitted curves over the raw data, adjust as needed
Curve237_output <- cbind('Curve237', Curve237_data$id[1], Curve237_data$unique_id[1], Curve237_data$machine[1], Curve237_data$baseline_yn[1],
                         Curve237_data$A[1], Curve237_data$Ci[1], Curve237_data$gsw[1],
                         mean(Curve237_data$VPDleaf, na.rm = T), mean(Curve237_data$Tleaf, na.rm = T), mean(Curve237_data$Qin, na.rm = T),
                         Curve237_fit[[2]][1,1], Curve237_fit[[2]][1,2],
                         Curve237_fit[[2]][2,1], Curve237_fit[[2]][2,2],
                         Curve237_fit[[2]][3,1], Curve237_fit[[2]][3,2],
                         Curve237_fit$RMSE,
                         Curve237_fit$Ci_transition,
                         Curve237_fit$citransition,
                         Curve237_fit$Km,
                         Curve237_fit$GammaStar,
                         Curve237_fit$fitmethod,
                         Curve237_fit$Tcorrect,
                         Curve237_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve237_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve237_output) # add the curve fits to the larger data frame

### Curve238_data
Curve238_data <- subset(aci.df, unique_id == aci.df.unique_id[238]) # find correct curve from full dataframe and make new object
plot(Curve238_data$A~Curve238_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve238_fit <- fitaci(Curve238_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve238_fit) # take a look at fitted values, adjust as needed
plot(Curve238_fit) # plot the fitted curves over the raw data, adjust as needed
Curve238_output <- cbind('Curve238', Curve238_data$id[1], Curve238_data$unique_id[1], Curve238_data$machine[1], Curve238_data$baseline_yn[1],
                         Curve238_data$A[1], Curve238_data$Ci[1], Curve238_data$gsw[1],
                         mean(Curve238_data$VPDleaf, na.rm = T), mean(Curve238_data$Tleaf, na.rm = T), mean(Curve238_data$Qin, na.rm = T),
                         Curve238_fit[[2]][1,1], Curve238_fit[[2]][1,2],
                         Curve238_fit[[2]][2,1], Curve238_fit[[2]][2,2],
                         Curve238_fit[[2]][3,1], Curve238_fit[[2]][3,2],
                         Curve238_fit$RMSE,
                         Curve238_fit$Ci_transition,
                         Curve238_fit$citransition,
                         Curve238_fit$Km,
                         Curve238_fit$GammaStar,
                         Curve238_fit$fitmethod,
                         Curve238_fit$Tcorrect,
                         Curve238_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve238_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve238_output) # add the curve fits to the larger data frame

### Curve239_data
Curve239_data <- subset(aci.df, unique_id == aci.df.unique_id[239] & Ci < 1000) # find correct curve from full dataframe and make new object
plot(Curve239_data$A~Curve239_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve239_fit <- fitaci(Curve239_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve239_fit) # take a look at fitted values, adjust as needed
plot(Curve239_fit) # plot the fitted curves over the raw data, adjust as needed
Curve239_output <- cbind('Curve239', Curve239_data$id[1], Curve239_data$unique_id[1], Curve239_data$machine[1], Curve239_data$baseline_yn[1],
                         Curve239_data$A[1], Curve239_data$Ci[1], Curve239_data$gsw[1],
                         mean(Curve239_data$VPDleaf, na.rm = T), mean(Curve239_data$Tleaf, na.rm = T), mean(Curve239_data$Qin, na.rm = T),
                         Curve239_fit[[2]][1,1], Curve239_fit[[2]][1,2],
                         Curve239_fit[[2]][2,1], Curve239_fit[[2]][2,2],
                         Curve239_fit[[2]][3,1], Curve239_fit[[2]][3,2],
                         Curve239_fit$RMSE,
                         Curve239_fit$Ci_transition,
                         Curve239_fit$citransition,
                         Curve239_fit$Km,
                         Curve239_fit$GammaStar,
                         Curve239_fit$fitmethod,
                         Curve239_fit$Tcorrect,
                         Curve239_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve239_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve239_output) # add the curve fits to the larger data frame

### Curve240_data
Curve240_data <- subset(aci.df, unique_id == aci.df.unique_id[240]) # find correct curve from full dataframe and make new object
plot(Curve240_data$A~Curve240_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve240_fit <- fitaci(Curve240_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve240_fit) # take a look at fitted values, adjust as needed
plot(Curve240_fit) # plot the fitted curves over the raw data, adjust as needed
Curve240_output <- cbind('Curve240', Curve240_data$id[1], Curve240_data$unique_id[1], Curve240_data$machine[1], Curve240_data$baseline_yn[1],
                         Curve240_data$A[1], Curve240_data$Ci[1], Curve240_data$gsw[1],
                         mean(Curve240_data$VPDleaf, na.rm = T), mean(Curve240_data$Tleaf, na.rm = T), mean(Curve240_data$Qin, na.rm = T),
                         Curve240_fit[[2]][1,1], Curve240_fit[[2]][1,2],
                         Curve240_fit[[2]][2,1], Curve240_fit[[2]][2,2],
                         Curve240_fit[[2]][3,1], Curve240_fit[[2]][3,2],
                         Curve240_fit$RMSE,
                         Curve240_fit$Ci_transition,
                         Curve240_fit$citransition,
                         Curve240_fit$Km,
                         Curve240_fit$GammaStar,
                         Curve240_fit$fitmethod,
                         Curve240_fit$Tcorrect,
                         Curve240_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve240_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve240_output) # add the curve fits to the larger data frame

### Curve241_data
Curve241_data <- subset(aci.df, unique_id == aci.df.unique_id[241]) # find correct curve from full dataframe and make new object
plot(Curve241_data$A~Curve241_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve241_fit <- fitaci(Curve241_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve241_fit) # take a look at fitted values, adjust as needed
plot(Curve241_fit) # plot the fitted curves over the raw data, adjust as needed
Curve241_output <- cbind('Curve241', Curve241_data$id[1], Curve241_data$unique_id[1], Curve241_data$machine[1], Curve241_data$baseline_yn[1],
                         Curve241_data$A[1], Curve241_data$Ci[1], Curve241_data$gsw[1],
                         mean(Curve241_data$VPDleaf, na.rm = T), mean(Curve241_data$Tleaf, na.rm = T), mean(Curve241_data$Qin, na.rm = T),
                         Curve241_fit[[2]][1,1], Curve241_fit[[2]][1,2],
                         Curve241_fit[[2]][2,1], Curve241_fit[[2]][2,2],
                         Curve241_fit[[2]][3,1], Curve241_fit[[2]][3,2],
                         Curve241_fit$RMSE,
                         Curve241_fit$Ci_transition,
                         Curve241_fit$citransition,
                         Curve241_fit$Km,
                         Curve241_fit$GammaStar,
                         Curve241_fit$fitmethod,
                         Curve241_fit$Tcorrect,
                         Curve241_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve241_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve241_output) # add the curve fits to the larger data frame

### Curve242_data
Curve242_data <- subset(aci.df, unique_id == aci.df.unique_id[242]) # find correct curve from full dataframe and make new object
plot(Curve242_data$A~Curve242_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve242_fit <- fitaci(Curve242_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve242_fit) # take a look at fitted values, adjust as needed
plot(Curve242_fit) # plot the fitted curves over the raw data, adjust as needed
Curve242_output <- cbind('Curve242', Curve242_data$id[1], Curve242_data$unique_id[1], Curve242_data$machine[1], Curve242_data$baseline_yn[1],
                         Curve242_data$A[1], Curve242_data$Ci[1], Curve242_data$gsw[1],
                         mean(Curve242_data$VPDleaf, na.rm = T), mean(Curve242_data$Tleaf, na.rm = T), mean(Curve242_data$Qin, na.rm = T),
                         Curve242_fit[[2]][1,1], Curve242_fit[[2]][1,2],
                         Curve242_fit[[2]][2,1], Curve242_fit[[2]][2,2],
                         Curve242_fit[[2]][3,1], Curve242_fit[[2]][3,2],
                         Curve242_fit$RMSE,
                         Curve242_fit$Ci_transition,
                         Curve242_fit$citransition,
                         Curve242_fit$Km,
                         Curve242_fit$GammaStar,
                         Curve242_fit$fitmethod,
                         Curve242_fit$Tcorrect,
                         Curve242_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve242_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve242_output) # add the curve fits to the larger data frame

### Curve243_data
Curve243_data <- subset(aci.df, unique_id == aci.df.unique_id[243]) # find correct curve from full dataframe and make new object
plot(Curve243_data$A~Curve243_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve243_fit <- fitaci(Curve243_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve243_fit) # take a look at fitted values, adjust as needed
plot(Curve243_fit) # plot the fitted curves over the raw data, adjust as needed
Curve243_output <- cbind('Curve243', Curve243_data$id[1], Curve243_data$unique_id[1], Curve243_data$machine[1], Curve243_data$baseline_yn[1],
                         Curve243_data$A[1], Curve243_data$Ci[1], Curve243_data$gsw[1],
                         mean(Curve243_data$VPDleaf, na.rm = T), mean(Curve243_data$Tleaf, na.rm = T), mean(Curve243_data$Qin, na.rm = T),
                         Curve243_fit[[2]][1,1], Curve243_fit[[2]][1,2],
                         Curve243_fit[[2]][2,1], Curve243_fit[[2]][2,2],
                         Curve243_fit[[2]][3,1], Curve243_fit[[2]][3,2],
                         Curve243_fit$RMSE,
                         Curve243_fit$Ci_transition,
                         Curve243_fit$citransition,
                         Curve243_fit$Km,
                         Curve243_fit$GammaStar,
                         Curve243_fit$fitmethod,
                         Curve243_fit$Tcorrect,
                         Curve243_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve243_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve243_output) # add the curve fits to the larger data frame

### Curve244_data
Curve244_data <- subset(aci.df, unique_id == aci.df.unique_id[244] & Ci < 1000) # find correct curve from full dataframe and make new object
plot(Curve244_data$A~Curve244_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve244_fit <- fitaci(Curve244_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve244_fit) # take a look at fitted values, adjust as needed
plot(Curve244_fit) # plot the fitted curves over the raw data, adjust as needed
Curve244_output <- cbind('Curve244', Curve244_data$id[1], Curve244_data$unique_id[1], Curve244_data$machine[1], Curve244_data$baseline_yn[1],
                         Curve244_data$A[1], Curve244_data$Ci[1], Curve244_data$gsw[1],
                         mean(Curve244_data$VPDleaf, na.rm = T), mean(Curve244_data$Tleaf, na.rm = T), mean(Curve244_data$Qin, na.rm = T),
                         Curve244_fit[[2]][1,1], Curve244_fit[[2]][1,2],
                         Curve244_fit[[2]][2,1], Curve244_fit[[2]][2,2],
                         Curve244_fit[[2]][3,1], Curve244_fit[[2]][3,2],
                         Curve244_fit$RMSE,
                         Curve244_fit$Ci_transition,
                         Curve244_fit$citransition,
                         Curve244_fit$Km,
                         Curve244_fit$GammaStar,
                         Curve244_fit$fitmethod,
                         Curve244_fit$Tcorrect,
                         Curve244_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve244_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve244_output) # add the curve fits to the larger data frame

### Curve245_data
Curve245_data <- subset(aci.df, unique_id == aci.df.unique_id[245]) # find correct curve from full dataframe and make new object
plot(Curve245_data$A~Curve245_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve245_fit <- fitaci(Curve245_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve245_fit) # take a look at fitted values, adjust as needed
plot(Curve245_fit) # plot the fitted curves over the raw data, adjust as needed
Curve245_output <- cbind('Curve245', Curve245_data$id[1], Curve245_data$unique_id[1], Curve245_data$machine[1], Curve245_data$baseline_yn[1],
                         Curve245_data$A[1], Curve245_data$Ci[1], Curve245_data$gsw[1],
                         mean(Curve245_data$VPDleaf, na.rm = T), mean(Curve245_data$Tleaf, na.rm = T), mean(Curve245_data$Qin, na.rm = T),
                         Curve245_fit[[2]][1,1], Curve245_fit[[2]][1,2],
                         Curve245_fit[[2]][2,1], Curve245_fit[[2]][2,2],
                         Curve245_fit[[2]][3,1], Curve245_fit[[2]][3,2],
                         Curve245_fit$RMSE,
                         Curve245_fit$Ci_transition,
                         Curve245_fit$citransition,
                         Curve245_fit$Km,
                         Curve245_fit$GammaStar,
                         Curve245_fit$fitmethod,
                         Curve245_fit$Tcorrect,
                         Curve245_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve245_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve245_output) # add the curve fits to the larger data frame

### Curve246_data
Curve246_data <- subset(aci.df, unique_id == aci.df.unique_id[246] & Ci < 1000) # find correct curve from full dataframe and make new object
plot(Curve246_data$A~Curve246_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve246_fit <- fitaci(Curve246_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve246_fit) # take a look at fitted values, adjust as needed
plot(Curve246_fit) # plot the fitted curves over the raw data, adjust as needed
Curve246_output <- cbind('Curve246', Curve246_data$id[1], Curve246_data$unique_id[1], Curve246_data$machine[1], Curve246_data$baseline_yn[1],
                         Curve246_data$A[1], Curve246_data$Ci[1], Curve246_data$gsw[1],
                         mean(Curve246_data$VPDleaf, na.rm = T), mean(Curve246_data$Tleaf, na.rm = T), mean(Curve246_data$Qin, na.rm = T),
                         Curve246_fit[[2]][1,1], Curve246_fit[[2]][1,2],
                         Curve246_fit[[2]][2,1], Curve246_fit[[2]][2,2],
                         Curve246_fit[[2]][3,1], Curve246_fit[[2]][3,2],
                         Curve246_fit$RMSE,
                         Curve246_fit$Ci_transition,
                         Curve246_fit$citransition,
                         Curve246_fit$Km,
                         Curve246_fit$GammaStar,
                         Curve246_fit$fitmethod,
                         Curve246_fit$Tcorrect,
                         Curve246_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve246_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve246_output) # add the curve fits to the larger data frame

### Curve247_data
Curve247_data <- subset(aci.df, unique_id == aci.df.unique_id[247]) # find correct curve from full dataframe and make new object
plot(Curve247_data$A~Curve247_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve247_fit <- fitaci(Curve247_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve247_fit) # take a look at fitted values, adjust as needed
plot(Curve247_fit) # plot the fitted curves over the raw data, adjust as needed
Curve247_output <- cbind('Curve247', Curve247_data$id[1], Curve247_data$unique_id[1], Curve247_data$machine[1], Curve247_data$baseline_yn[1],
                         Curve247_data$A[1], Curve247_data$Ci[1], Curve247_data$gsw[1],
                         mean(Curve247_data$VPDleaf, na.rm = T), mean(Curve247_data$Tleaf, na.rm = T), mean(Curve247_data$Qin, na.rm = T),
                         Curve247_fit[[2]][1,1], Curve247_fit[[2]][1,2],
                         Curve247_fit[[2]][2,1], Curve247_fit[[2]][2,2],
                         Curve247_fit[[2]][3,1], Curve247_fit[[2]][3,2],
                         Curve247_fit$RMSE,
                         Curve247_fit$Ci_transition,
                         Curve247_fit$citransition,
                         Curve247_fit$Km,
                         Curve247_fit$GammaStar,
                         Curve247_fit$fitmethod,
                         Curve247_fit$Tcorrect,
                         Curve247_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve247_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve247_output) # add the curve fits to the larger data frame

### Curve248_data
Curve248_data <- subset(aci.df, unique_id == aci.df.unique_id[248]) # find correct curve from full dataframe and make new object
plot(Curve248_data$A~Curve248_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve248_fit <- fitaci(Curve248_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve248_fit) # take a look at fitted values, adjust as needed
plot(Curve248_fit) # plot the fitted curves over the raw data, adjust as needed
Curve248_output <- cbind('Curve248', Curve248_data$id[1], Curve248_data$unique_id[1], Curve248_data$machine[1], Curve248_data$baseline_yn[1],
                         Curve248_data$A[1], Curve248_data$Ci[1], Curve248_data$gsw[1],
                         mean(Curve248_data$VPDleaf, na.rm = T), mean(Curve248_data$Tleaf, na.rm = T), mean(Curve248_data$Qin, na.rm = T),
                         Curve248_fit[[2]][1,1], Curve248_fit[[2]][1,2],
                         Curve248_fit[[2]][2,1], Curve248_fit[[2]][2,2],
                         Curve248_fit[[2]][3,1], Curve248_fit[[2]][3,2],
                         Curve248_fit$RMSE,
                         Curve248_fit$Ci_transition,
                         Curve248_fit$citransition,
                         Curve248_fit$Km,
                         Curve248_fit$GammaStar,
                         Curve248_fit$fitmethod,
                         Curve248_fit$Tcorrect,
                         Curve248_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve248_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve248_output) # add the curve fits to the larger data frame

### Curve249_data
Curve249_data <- subset(aci.df, unique_id == aci.df.unique_id[249] & Ci < 1000) # find correct curve from full dataframe and make new object
plot(Curve249_data$A~Curve249_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve249_fit <- fitaci(Curve249_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve249_fit) # take a look at fitted values, adjust as needed
plot(Curve249_fit) # plot the fitted curves over the raw data, adjust as needed
Curve249_output <- cbind('Curve249', Curve249_data$id[1], Curve249_data$unique_id[1], Curve249_data$machine[1], Curve249_data$baseline_yn[1],
                         Curve249_data$A[1], Curve249_data$Ci[1], Curve249_data$gsw[1],
                         mean(Curve249_data$VPDleaf, na.rm = T), mean(Curve249_data$Tleaf, na.rm = T), mean(Curve249_data$Qin, na.rm = T),
                         Curve249_fit[[2]][1,1], Curve249_fit[[2]][1,2],
                         Curve249_fit[[2]][2,1], Curve249_fit[[2]][2,2],
                         Curve249_fit[[2]][3,1], Curve249_fit[[2]][3,2],
                         Curve249_fit$RMSE,
                         Curve249_fit$Ci_transition,
                         Curve249_fit$citransition,
                         Curve249_fit$Km,
                         Curve249_fit$GammaStar,
                         Curve249_fit$fitmethod,
                         Curve249_fit$Tcorrect,
                         Curve249_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve249_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve249_output) # add the curve fits to the larger data frame

### Curve250_data
Curve250_data <- subset(aci.df, unique_id == aci.df.unique_id[250] & Ci < 1000) # find correct curve from full dataframe and make new object
plot(Curve250_data$A~Curve250_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve250_fit <- fitaci(Curve250_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve250_fit) # take a look at fitted values, adjust as needed
plot(Curve250_fit) # plot the fitted curves over the raw data, adjust as needed
Curve250_output <- cbind('Curve250', Curve250_data$id[1], Curve250_data$unique_id[1], Curve250_data$machine[1], Curve250_data$baseline_yn[1],
                         Curve250_data$A[1], Curve250_data$Ci[1], Curve250_data$gsw[1],
                         mean(Curve250_data$VPDleaf, na.rm = T), mean(Curve250_data$Tleaf, na.rm = T), mean(Curve250_data$Qin, na.rm = T),
                         Curve250_fit[[2]][1,1], Curve250_fit[[2]][1,2],
                         Curve250_fit[[2]][2,1], Curve250_fit[[2]][2,2],
                         Curve250_fit[[2]][3,1], Curve250_fit[[2]][3,2],
                         Curve250_fit$RMSE,
                         Curve250_fit$Ci_transition,
                         Curve250_fit$citransition,
                         Curve250_fit$Km,
                         Curve250_fit$GammaStar,
                         Curve250_fit$fitmethod,
                         Curve250_fit$Tcorrect,
                         Curve250_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve250_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve250_output) # add the curve fits to the larger data frame

### Curve251_data
Curve251_data <- subset(aci.df, unique_id == aci.df.unique_id[251] & Ci < 1000) # find correct curve from full dataframe and make new object
plot(Curve251_data$A~Curve251_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve251_fit <- fitaci(Curve251_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve251_fit) # take a look at fitted values, adjust as needed
plot(Curve251_fit) # plot the fitted curves over the raw data, adjust as needed
Curve251_output <- cbind('Curve251', Curve251_data$id[1], Curve251_data$unique_id[1], Curve251_data$machine[1], Curve251_data$baseline_yn[1],
                         Curve251_data$A[1], Curve251_data$Ci[1], Curve251_data$gsw[1],
                         mean(Curve251_data$VPDleaf, na.rm = T), mean(Curve251_data$Tleaf, na.rm = T), mean(Curve251_data$Qin, na.rm = T),
                         Curve251_fit[[2]][1,1], Curve251_fit[[2]][1,2],
                         Curve251_fit[[2]][2,1], Curve251_fit[[2]][2,2],
                         Curve251_fit[[2]][3,1], Curve251_fit[[2]][3,2],
                         Curve251_fit$RMSE,
                         Curve251_fit$Ci_transition,
                         Curve251_fit$citransition,
                         Curve251_fit$Km,
                         Curve251_fit$GammaStar,
                         Curve251_fit$fitmethod,
                         Curve251_fit$Tcorrect,
                         Curve251_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve251_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve251_output) # add the curve fits to the larger data frame

### Curve252_data
Curve252_data <- subset(aci.df, unique_id == aci.df.unique_id[252]) # find correct curve from full dataframe and make new object
plot(Curve252_data$A~Curve252_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve252_fit <- fitaci(Curve252_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve252_fit) # take a look at fitted values, adjust as needed
plot(Curve252_fit) # plot the fitted curves over the raw data, adjust as needed
Curve252_output <- cbind('Curve252', Curve252_data$id[1], Curve252_data$unique_id[1], Curve252_data$machine[1], Curve252_data$baseline_yn[1],
                         Curve252_data$A[1], Curve252_data$Ci[1], Curve252_data$gsw[1],
                         mean(Curve252_data$VPDleaf, na.rm = T), mean(Curve252_data$Tleaf, na.rm = T), mean(Curve252_data$Qin, na.rm = T),
                         Curve252_fit[[2]][1,1], Curve252_fit[[2]][1,2],
                         Curve252_fit[[2]][2,1], Curve252_fit[[2]][2,2],
                         Curve252_fit[[2]][3,1], Curve252_fit[[2]][3,2],
                         Curve252_fit$RMSE,
                         Curve252_fit$Ci_transition,
                         Curve252_fit$citransition,
                         Curve252_fit$Km,
                         Curve252_fit$GammaStar,
                         Curve252_fit$fitmethod,
                         Curve252_fit$Tcorrect,
                         Curve252_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve252_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve252_output) # add the curve fits to the larger data frame

### Curve253_data
Curve253_data <- subset(aci.df, unique_id == aci.df.unique_id[253]) # find correct curve from full dataframe and make new object
plot(Curve253_data$A~Curve253_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve253_fit <- fitaci(Curve253_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve253_fit) # take a look at fitted values, adjust as needed
plot(Curve253_fit) # plot the fitted curves over the raw data, adjust as needed
Curve253_output <- cbind('Curve253', Curve253_data$id[1], Curve253_data$unique_id[1], Curve253_data$machine[1], Curve253_data$baseline_yn[1],
                         Curve253_data$A[1], Curve253_data$Ci[1], Curve253_data$gsw[1],
                         mean(Curve253_data$VPDleaf, na.rm = T), mean(Curve253_data$Tleaf, na.rm = T), mean(Curve253_data$Qin, na.rm = T),
                         Curve253_fit[[2]][1,1], Curve253_fit[[2]][1,2],
                         Curve253_fit[[2]][2,1], Curve253_fit[[2]][2,2],
                         Curve253_fit[[2]][3,1], Curve253_fit[[2]][3,2],
                         Curve253_fit$RMSE,
                         Curve253_fit$Ci_transition,
                         Curve253_fit$citransition,
                         Curve253_fit$Km,
                         Curve253_fit$GammaStar,
                         Curve253_fit$fitmethod,
                         Curve253_fit$Tcorrect,
                         Curve253_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve253_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve253_output) # add the curve fits to the larger data frame

### Curve254_data
Curve254_data <- subset(aci.df, unique_id == aci.df.unique_id[254]) # find correct curve from full dataframe and make new object
plot(Curve254_data$A~Curve254_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve254_fit <- fitaci(Curve254_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve254_fit) # take a look at fitted values, adjust as needed
plot(Curve254_fit) # plot the fitted curves over the raw data, adjust as needed
Curve254_output <- cbind('Curve254', Curve254_data$id[1], Curve254_data$unique_id[1], Curve254_data$machine[1], Curve254_data$baseline_yn[1],
                         Curve254_data$A[1], Curve254_data$Ci[1], Curve254_data$gsw[1],
                         mean(Curve254_data$VPDleaf, na.rm = T), mean(Curve254_data$Tleaf, na.rm = T), mean(Curve254_data$Qin, na.rm = T),
                         Curve254_fit[[2]][1,1], Curve254_fit[[2]][1,2],
                         Curve254_fit[[2]][2,1], Curve254_fit[[2]][2,2],
                         Curve254_fit[[2]][3,1], Curve254_fit[[2]][3,2],
                         Curve254_fit$RMSE,
                         Curve254_fit$Ci_transition,
                         Curve254_fit$citransition,
                         Curve254_fit$Km,
                         Curve254_fit$GammaStar,
                         Curve254_fit$fitmethod,
                         Curve254_fit$Tcorrect,
                         Curve254_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve254_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve254_output) # add the curve fits to the larger data frame

### Curve255_data
Curve255_data <- subset(aci.df, unique_id == aci.df.unique_id[255] & Ci < 1000) # find correct curve from full dataframe and make new object
plot(Curve255_data$A~Curve255_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve255_fit <- fitaci(Curve255_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve255_fit) # take a look at fitted values, adjust as needed
plot(Curve255_fit) # plot the fitted curves over the raw data, adjust as needed
Curve255_output <- cbind('Curve255', Curve255_data$id[1], Curve255_data$unique_id[1], Curve255_data$machine[1], Curve255_data$baseline_yn[1],
                         Curve255_data$A[1], Curve255_data$Ci[1], Curve255_data$gsw[1],
                         mean(Curve255_data$VPDleaf, na.rm = T), mean(Curve255_data$Tleaf, na.rm = T), mean(Curve255_data$Qin, na.rm = T),
                         Curve255_fit[[2]][1,1], Curve255_fit[[2]][1,2],
                         Curve255_fit[[2]][2,1], Curve255_fit[[2]][2,2],
                         Curve255_fit[[2]][3,1], Curve255_fit[[2]][3,2],
                         Curve255_fit$RMSE,
                         Curve255_fit$Ci_transition,
                         Curve255_fit$citransition,
                         Curve255_fit$Km,
                         Curve255_fit$GammaStar,
                         Curve255_fit$fitmethod,
                         Curve255_fit$Tcorrect,
                         Curve255_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve255_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve255_output) # add the curve fits to the larger data frame

### Curve256_data
Curve256_data <- subset(aci.df, unique_id == aci.df.unique_id[256]) # find correct curve from full dataframe and make new object
plot(Curve256_data$A~Curve256_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve256_fit <- fitaci(Curve256_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve256_fit) # take a look at fitted values, adjust as needed
plot(Curve256_fit) # plot the fitted curves over the raw data, adjust as needed
Curve256_output <- cbind('Curve256', Curve256_data$id[1], Curve256_data$unique_id[1], Curve256_data$machine[1], Curve256_data$baseline_yn[1],
                         Curve256_data$A[1], Curve256_data$Ci[1], Curve256_data$gsw[1],
                         mean(Curve256_data$VPDleaf, na.rm = T), mean(Curve256_data$Tleaf, na.rm = T), mean(Curve256_data$Qin, na.rm = T),
                         Curve256_fit[[2]][1,1], Curve256_fit[[2]][1,2],
                         Curve256_fit[[2]][2,1], Curve256_fit[[2]][2,2],
                         Curve256_fit[[2]][3,1], Curve256_fit[[2]][3,2],
                         Curve256_fit$RMSE,
                         Curve256_fit$Ci_transition,
                         Curve256_fit$citransition,
                         Curve256_fit$Km,
                         Curve256_fit$GammaStar,
                         Curve256_fit$fitmethod,
                         Curve256_fit$Tcorrect,
                         Curve256_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve256_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve256_output) # add the curve fits to the larger data frame

### Curve257_data
Curve257_data <- subset(aci.df, unique_id == aci.df.unique_id[257] & Ci < 1000) # find correct curve from full dataframe and make new object
plot(Curve257_data$A~Curve257_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve257_fit <- fitaci(Curve257_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve257_fit) # take a look at fitted values, adjust as needed
plot(Curve257_fit) # plot the fitted curves over the raw data, adjust as needed
Curve257_output <- cbind('Curve257', Curve257_data$id[1], Curve257_data$unique_id[1], Curve257_data$machine[1], Curve257_data$baseline_yn[1],
                         Curve257_data$A[1], Curve257_data$Ci[1], Curve257_data$gsw[1],
                         mean(Curve257_data$VPDleaf, na.rm = T), mean(Curve257_data$Tleaf, na.rm = T), mean(Curve257_data$Qin, na.rm = T),
                         Curve257_fit[[2]][1,1], Curve257_fit[[2]][1,2],
                         Curve257_fit[[2]][2,1], Curve257_fit[[2]][2,2],
                         Curve257_fit[[2]][3,1], Curve257_fit[[2]][3,2],
                         Curve257_fit$RMSE,
                         Curve257_fit$Ci_transition,
                         Curve257_fit$citransition,
                         Curve257_fit$Km,
                         Curve257_fit$GammaStar,
                         Curve257_fit$fitmethod,
                         Curve257_fit$Tcorrect,
                         Curve257_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve257_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve257_output) # add the curve fits to the larger data frame

### Curve258_data
Curve258_data <- subset(aci.df, unique_id == aci.df.unique_id[258]) # find correct curve from full dataframe and make new object
plot(Curve258_data$A~Curve258_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve258_fit <- fitaci(Curve258_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve258_fit) # take a look at fitted values, adjust as needed
plot(Curve258_fit) # plot the fitted curves over the raw data, adjust as needed
Curve258_output <- cbind('Curve258', Curve258_data$id[1], Curve258_data$unique_id[1], Curve258_data$machine[1], Curve258_data$baseline_yn[1],
                         Curve258_data$A[1], Curve258_data$Ci[1], Curve258_data$gsw[1],
                         mean(Curve258_data$VPDleaf, na.rm = T), mean(Curve258_data$Tleaf, na.rm = T), mean(Curve258_data$Qin, na.rm = T),
                         Curve258_fit[[2]][1,1], Curve258_fit[[2]][1,2],
                         Curve258_fit[[2]][2,1], Curve258_fit[[2]][2,2],
                         Curve258_fit[[2]][3,1], Curve258_fit[[2]][3,2],
                         Curve258_fit$RMSE,
                         Curve258_fit$Ci_transition,
                         Curve258_fit$citransition,
                         Curve258_fit$Km,
                         Curve258_fit$GammaStar,
                         Curve258_fit$fitmethod,
                         Curve258_fit$Tcorrect,
                         Curve258_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve258_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve258_output) # add the curve fits to the larger data frame

### Curve260_data
Curve260_data <- subset(aci.df, unique_id == aci.df.unique_id[260]) # find correct curve from full dataframe and make new object
plot(Curve260_data$A~Curve260_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve260_fit <- fitaci(Curve260_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve260_fit) # take a look at fitted values, adjust as needed
plot(Curve260_fit) # plot the fitted curves over the raw data, adjust as needed
Curve260_output <- cbind('Curve260', Curve260_data$id[1], Curve260_data$unique_id[1], Curve260_data$machine[1], Curve260_data$baseline_yn[1],
                         Curve260_data$A[1], Curve260_data$Ci[1], Curve260_data$gsw[1],
                         mean(Curve260_data$VPDleaf, na.rm = T), mean(Curve260_data$Tleaf, na.rm = T), mean(Curve260_data$Qin, na.rm = T),
                         Curve260_fit[[2]][1,1], Curve260_fit[[2]][1,2],
                         Curve260_fit[[2]][2,1], Curve260_fit[[2]][2,2],
                         Curve260_fit[[2]][3,1], Curve260_fit[[2]][3,2],
                         Curve260_fit$RMSE,
                         Curve260_fit$Ci_transition,
                         Curve260_fit$citransition,
                         Curve260_fit$Km,
                         Curve260_fit$GammaStar,
                         Curve260_fit$fitmethod,
                         Curve260_fit$Tcorrect,
                         Curve260_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve260_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve260_output) # add the curve fits to the larger data frame

### Curve261_data
Curve261_data <- subset(aci.df, unique_id == aci.df.unique_id[261]) # find correct curve from full dataframe and make new object
plot(Curve261_data$A~Curve261_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve261_fit <- fitaci(Curve261_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve261_fit) # take a look at fitted values, adjust as needed
plot(Curve261_fit) # plot the fitted curves over the raw data, adjust as needed
Curve261_output <- cbind('Curve261', Curve261_data$id[1], Curve261_data$unique_id[1], Curve261_data$machine[1], Curve261_data$baseline_yn[1],
                         Curve261_data$A[1], Curve261_data$Ci[1], Curve261_data$gsw[1],
                         mean(Curve261_data$VPDleaf, na.rm = T), mean(Curve261_data$Tleaf, na.rm = T), mean(Curve261_data$Qin, na.rm = T),
                         Curve261_fit[[2]][1,1], Curve261_fit[[2]][1,2],
                         Curve261_fit[[2]][2,1], Curve261_fit[[2]][2,2],
                         Curve261_fit[[2]][3,1], Curve261_fit[[2]][3,2],
                         Curve261_fit$RMSE,
                         Curve261_fit$Ci_transition,
                         Curve261_fit$citransition,
                         Curve261_fit$Km,
                         Curve261_fit$GammaStar,
                         Curve261_fit$fitmethod,
                         Curve261_fit$Tcorrect,
                         Curve261_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve261_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve261_output) # add the curve fits to the larger data frame

### Curve262_data
Curve262_data <- subset(aci.df, unique_id == aci.df.unique_id[262]) # find correct curve from full dataframe and make new object
plot(Curve262_data$A~Curve262_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve262_fit <- fitaci(Curve262_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve262_fit) # take a look at fitted values, adjust as needed
plot(Curve262_fit) # plot the fitted curves over the raw data, adjust as needed
Curve262_output <- cbind('Curve262', Curve262_data$id[1], Curve262_data$unique_id[1], Curve262_data$machine[1], Curve262_data$baseline_yn[1],
                         Curve262_data$A[1], Curve262_data$Ci[1], Curve262_data$gsw[1],
                         mean(Curve262_data$VPDleaf, na.rm = T), mean(Curve262_data$Tleaf, na.rm = T), mean(Curve262_data$Qin, na.rm = T),
                         Curve262_fit[[2]][1,1], Curve262_fit[[2]][1,2],
                         Curve262_fit[[2]][2,1], Curve262_fit[[2]][2,2],
                         Curve262_fit[[2]][3,1], Curve262_fit[[2]][3,2],
                         Curve262_fit$RMSE,
                         Curve262_fit$Ci_transition,
                         Curve262_fit$citransition,
                         Curve262_fit$Km,
                         Curve262_fit$GammaStar,
                         Curve262_fit$fitmethod,
                         Curve262_fit$Tcorrect,
                         Curve262_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve262_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve262_output) # add the curve fits to the larger data frame

### Curve263_data
Curve263_data <- subset(aci.df, unique_id == aci.df.unique_id[263]) # find correct curve from full dataframe and make new object
plot(Curve263_data$A~Curve263_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve263_fit <- fitaci(Curve263_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve263_fit) # take a look at fitted values, adjust as needed
plot(Curve263_fit) # plot the fitted curves over the raw data, adjust as needed
Curve263_output <- cbind('Curve263', Curve263_data$id[1], Curve263_data$unique_id[1], Curve263_data$machine[1], Curve263_data$baseline_yn[1],
                         Curve263_data$A[1], Curve263_data$Ci[1], Curve263_data$gsw[1],
                         mean(Curve263_data$VPDleaf, na.rm = T), mean(Curve263_data$Tleaf, na.rm = T), mean(Curve263_data$Qin, na.rm = T),
                         Curve263_fit[[2]][1,1], Curve263_fit[[2]][1,2],
                         Curve263_fit[[2]][2,1], Curve263_fit[[2]][2,2],
                         Curve263_fit[[2]][3,1], Curve263_fit[[2]][3,2],
                         Curve263_fit$RMSE,
                         Curve263_fit$Ci_transition,
                         Curve263_fit$citransition,
                         Curve263_fit$Km,
                         Curve263_fit$GammaStar,
                         Curve263_fit$fitmethod,
                         Curve263_fit$Tcorrect,
                         Curve263_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve263_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve263_output) # add the curve fits to the larger data frame

### Curve264_data
Curve264_data <- subset(aci.df, unique_id == aci.df.unique_id[264]) # find correct curve from full dataframe and make new object
plot(Curve264_data$A~Curve264_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve264_fit <- fitaci(Curve264_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve264_fit) # take a look at fitted values, adjust as needed
plot(Curve264_fit) # plot the fitted curves over the raw data, adjust as needed
Curve264_output <- cbind('Curve264', Curve264_data$id[1], Curve264_data$unique_id[1], Curve264_data$machine[1], Curve264_data$baseline_yn[1],
                         Curve264_data$A[1], Curve264_data$Ci[1], Curve264_data$gsw[1],
                         mean(Curve264_data$VPDleaf, na.rm = T), mean(Curve264_data$Tleaf, na.rm = T), mean(Curve264_data$Qin, na.rm = T),
                         Curve264_fit[[2]][1,1], Curve264_fit[[2]][1,2],
                         Curve264_fit[[2]][2,1], Curve264_fit[[2]][2,2],
                         Curve264_fit[[2]][3,1], Curve264_fit[[2]][3,2],
                         Curve264_fit$RMSE,
                         Curve264_fit$Ci_transition,
                         Curve264_fit$citransition,
                         Curve264_fit$Km,
                         Curve264_fit$GammaStar,
                         Curve264_fit$fitmethod,
                         Curve264_fit$Tcorrect,
                         Curve264_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve264_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve264_output) # add the curve fits to the larger data frame

### Curve265_data
Curve265_data <- subset(aci.df, unique_id == aci.df.unique_id[265] & Ci < 1000) # find correct curve from full dataframe and make new object
plot(Curve265_data$A~Curve265_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve265_fit <- fitaci(Curve265_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve265_fit) # take a look at fitted values, adjust as needed
plot(Curve265_fit) # plot the fitted curves over the raw data, adjust as needed
Curve265_output <- cbind('Curve265', Curve265_data$id[1], Curve265_data$unique_id[1], Curve265_data$machine[1], Curve265_data$baseline_yn[1],
                         Curve265_data$A[1], Curve265_data$Ci[1], Curve265_data$gsw[1],
                         mean(Curve265_data$VPDleaf, na.rm = T), mean(Curve265_data$Tleaf, na.rm = T), mean(Curve265_data$Qin, na.rm = T),
                         Curve265_fit[[2]][1,1], Curve265_fit[[2]][1,2],
                         Curve265_fit[[2]][2,1], Curve265_fit[[2]][2,2],
                         Curve265_fit[[2]][3,1], Curve265_fit[[2]][3,2],
                         Curve265_fit$RMSE,
                         Curve265_fit$Ci_transition,
                         Curve265_fit$citransition,
                         Curve265_fit$Km,
                         Curve265_fit$GammaStar,
                         Curve265_fit$fitmethod,
                         Curve265_fit$Tcorrect,
                         Curve265_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve265_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve265_output) # add the curve fits to the larger data frame

### Curve266_data
Curve266_data <- subset(aci.df, unique_id == aci.df.unique_id[266]) # find correct curve from full dataframe and make new object
plot(Curve266_data$A~Curve266_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve266_fit <- fitaci(Curve266_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve266_fit) # take a look at fitted values, adjust as needed
plot(Curve266_fit) # plot the fitted curves over the raw data, adjust as needed
Curve266_output <- cbind('Curve266', Curve266_data$id[1], Curve266_data$unique_id[1], Curve266_data$machine[1], Curve266_data$baseline_yn[1],
                         Curve266_data$A[1], Curve266_data$Ci[1], Curve266_data$gsw[1],
                         mean(Curve266_data$VPDleaf, na.rm = T), mean(Curve266_data$Tleaf, na.rm = T), mean(Curve266_data$Qin, na.rm = T),
                         Curve266_fit[[2]][1,1], Curve266_fit[[2]][1,2],
                         Curve266_fit[[2]][2,1], Curve266_fit[[2]][2,2],
                         Curve266_fit[[2]][3,1], Curve266_fit[[2]][3,2],
                         Curve266_fit$RMSE,
                         Curve266_fit$Ci_transition,
                         Curve266_fit$citransition,
                         Curve266_fit$Km,
                         Curve266_fit$GammaStar,
                         Curve266_fit$fitmethod,
                         Curve266_fit$Tcorrect,
                         Curve266_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve266_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve266_output) # add the curve fits to the larger data frame

### Curve267_data
Curve267_data <- subset(aci.df, unique_id == aci.df.unique_id[267]) # find correct curve from full dataframe and make new object
plot(Curve267_data$A~Curve267_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve267_fit <- fitaci(Curve267_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve267_fit) # take a look at fitted values, adjust as needed
plot(Curve267_fit) # plot the fitted curves over the raw data, adjust as needed
Curve267_output <- cbind('Curve267', Curve267_data$id[1], Curve267_data$unique_id[1], Curve267_data$machine[1], Curve267_data$baseline_yn[1],
                         Curve267_data$A[1], Curve267_data$Ci[1], Curve267_data$gsw[1],
                         mean(Curve267_data$VPDleaf, na.rm = T), mean(Curve267_data$Tleaf, na.rm = T), mean(Curve267_data$Qin, na.rm = T),
                         Curve267_fit[[2]][1,1], Curve267_fit[[2]][1,2],
                         Curve267_fit[[2]][2,1], Curve267_fit[[2]][2,2],
                         Curve267_fit[[2]][3,1], Curve267_fit[[2]][3,2],
                         Curve267_fit$RMSE,
                         Curve267_fit$Ci_transition,
                         Curve267_fit$citransition,
                         Curve267_fit$Km,
                         Curve267_fit$GammaStar,
                         Curve267_fit$fitmethod,
                         Curve267_fit$Tcorrect,
                         Curve267_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve267_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve267_output) # add the curve fits to the larger data frame

### Curve268_data
Curve268_data <- subset(aci.df, unique_id == aci.df.unique_id[268] & Ci < 1000) # find correct curve from full dataframe and make new object
plot(Curve268_data$A~Curve268_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve268_fit <- fitaci(Curve268_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve268_fit) # take a look at fitted values, adjust as needed
plot(Curve268_fit) # plot the fitted curves over the raw data, adjust as needed
Curve268_output <- cbind('Curve268', Curve268_data$id[1], Curve268_data$unique_id[1], Curve268_data$machine[1], Curve268_data$baseline_yn[1],
                         Curve268_data$A[1], Curve268_data$Ci[1], Curve268_data$gsw[1],
                         mean(Curve268_data$VPDleaf, na.rm = T), mean(Curve268_data$Tleaf, na.rm = T), mean(Curve268_data$Qin, na.rm = T),
                         Curve268_fit[[2]][1,1], Curve268_fit[[2]][1,2],
                         Curve268_fit[[2]][2,1], Curve268_fit[[2]][2,2],
                         Curve268_fit[[2]][3,1], Curve268_fit[[2]][3,2],
                         Curve268_fit$RMSE,
                         Curve268_fit$Ci_transition,
                         Curve268_fit$citransition,
                         Curve268_fit$Km,
                         Curve268_fit$GammaStar,
                         Curve268_fit$fitmethod,
                         Curve268_fit$Tcorrect,
                         Curve268_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve268_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve268_output) # add the curve fits to the larger data frame

### Curve269_data
Curve269_data <- subset(aci.df, unique_id == aci.df.unique_id[269]) # find correct curve from full dataframe and make new object
plot(Curve269_data$A~Curve269_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve269_fit <- fitaci(Curve269_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve269_fit) # take a look at fitted values, adjust as needed
plot(Curve269_fit) # plot the fitted curves over the raw data, adjust as needed
Curve269_output <- cbind('Curve269', Curve269_data$id[1], Curve269_data$unique_id[1], Curve269_data$machine[1], Curve269_data$baseline_yn[1],
                         Curve269_data$A[1], Curve269_data$Ci[1], Curve269_data$gsw[1],
                         mean(Curve269_data$VPDleaf, na.rm = T), mean(Curve269_data$Tleaf, na.rm = T), mean(Curve269_data$Qin, na.rm = T),
                         Curve269_fit[[2]][1,1], Curve269_fit[[2]][1,2],
                         Curve269_fit[[2]][2,1], Curve269_fit[[2]][2,2],
                         Curve269_fit[[2]][3,1], Curve269_fit[[2]][3,2],
                         Curve269_fit$RMSE,
                         Curve269_fit$Ci_transition,
                         Curve269_fit$citransition,
                         Curve269_fit$Km,
                         Curve269_fit$GammaStar,
                         Curve269_fit$fitmethod,
                         Curve269_fit$Tcorrect,
                         Curve269_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve269_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve269_output) # add the curve fits to the larger data frame

### Curve270_data
Curve270_data <- subset(aci.df, unique_id == aci.df.unique_id[270]) # find correct curve from full dataframe and make new object
plot(Curve270_data$A~Curve270_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve270_fit <- fitaci(Curve270_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve270_fit) # take a look at fitted values, adjust as needed
plot(Curve270_fit) # plot the fitted curves over the raw data, adjust as needed
Curve270_output <- cbind('Curve270', Curve270_data$id[1], Curve270_data$unique_id[1], Curve270_data$machine[1], Curve270_data$baseline_yn[1],
                         Curve270_data$A[1], Curve270_data$Ci[1], Curve270_data$gsw[1],
                         mean(Curve270_data$VPDleaf, na.rm = T), mean(Curve270_data$Tleaf, na.rm = T), mean(Curve270_data$Qin, na.rm = T),
                         Curve270_fit[[2]][1,1], Curve270_fit[[2]][1,2],
                         Curve270_fit[[2]][2,1], Curve270_fit[[2]][2,2],
                         Curve270_fit[[2]][3,1], Curve270_fit[[2]][3,2],
                         Curve270_fit$RMSE,
                         Curve270_fit$Ci_transition,
                         Curve270_fit$citransition,
                         Curve270_fit$Km,
                         Curve270_fit$GammaStar,
                         Curve270_fit$fitmethod,
                         Curve270_fit$Tcorrect,
                         Curve270_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve270_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve270_output) # add the curve fits to the larger data frame

### Curve271_data
Curve271_data <- subset(aci.df, unique_id == aci.df.unique_id[271] & Ci < 1000) # find correct curve from full dataframe and make new object
plot(Curve271_data$A~Curve271_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve271_fit <- fitaci(Curve271_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve271_fit) # take a look at fitted values, adjust as needed
plot(Curve271_fit) # plot the fitted curves over the raw data, adjust as needed
Curve271_output <- cbind('Curve271', Curve271_data$id[1], Curve271_data$unique_id[1], Curve271_data$machine[1], Curve271_data$baseline_yn[1],
                         Curve271_data$A[1], Curve271_data$Ci[1], Curve271_data$gsw[1],
                         mean(Curve271_data$VPDleaf, na.rm = T), mean(Curve271_data$Tleaf, na.rm = T), mean(Curve271_data$Qin, na.rm = T),
                         Curve271_fit[[2]][1,1], Curve271_fit[[2]][1,2],
                         Curve271_fit[[2]][2,1], Curve271_fit[[2]][2,2],
                         Curve271_fit[[2]][3,1], Curve271_fit[[2]][3,2],
                         Curve271_fit$RMSE,
                         Curve271_fit$Ci_transition,
                         Curve271_fit$citransition,
                         Curve271_fit$Km,
                         Curve271_fit$GammaStar,
                         Curve271_fit$fitmethod,
                         Curve271_fit$Tcorrect,
                         Curve271_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve271_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve271_output) # add the curve fits to the larger data frame

### Curve272_data
Curve272_data <- subset(aci.df, unique_id == aci.df.unique_id[272] & Ci < 1000) # find correct curve from full dataframe and make new object
plot(Curve272_data$A~Curve272_data$Ci) # plot the A/Ci data and look for any weirdness, adjust as needed
Curve272_fit <- fitaci(Curve272_data, varnames = list(ALEAF = "A", # fit the curves
                                                      Tleaf = "Tleaf",
                                                      Ci = "Ci",
                                                      PPFD = "Qin"),
                       fitTPU = FALSE, Tcorrect = FALSE, useRd = FALSE)
summary(Curve272_fit) # take a look at fitted values, adjust as needed
plot(Curve272_fit) # plot the fitted curves over the raw data, adjust as needed
Curve272_output <- cbind('Curve272', Curve272_data$id[1], Curve272_data$unique_id[1], Curve272_data$machine[1], Curve272_data$baseline_yn[1],
                         Curve272_data$A[1], Curve272_data$Ci[1], Curve272_data$gsw[1],
                         mean(Curve272_data$VPDleaf, na.rm = T), mean(Curve272_data$Tleaf, na.rm = T), mean(Curve272_data$Qin, na.rm = T),
                         Curve272_fit[[2]][1,1], Curve272_fit[[2]][1,2],
                         Curve272_fit[[2]][2,1], Curve272_fit[[2]][2,2],
                         Curve272_fit[[2]][3,1], Curve272_fit[[2]][3,2],
                         Curve272_fit$RMSE,
                         Curve272_fit$Ci_transition,
                         Curve272_fit$citransition,
                         Curve272_fit$Km,
                         Curve272_fit$GammaStar,
                         Curve272_fit$fitmethod,
                         Curve272_fit$Tcorrect,
                         Curve272_fit$fitTPU) # put relevant data together in a vector of values
colnames(Curve272_output) <- c('curve_fit_number', 'id', 'unique_id', 'machine', 'baseline_yn',
                               'anet_420', 'ci_420', 'gs_420',
                               'vpd_leaf', 'temperature_leaf', 'par_leaf',
                               'vcmax_tleaf', 'vcmax_tleaf_se',
                               'jmax_tleaf', 'jmax_tleaf_se', 
                               'rdfit_tleaf', 'rdfit_tleaf_se',
                               'aci_RMSE', 'aci_ci_transistion', 'aci_ci_transition_set',
                               'aci_km', 'aci_gammastar', 'aci_fitmethod',
                               'aci_tcorrect', 'aci_fittpu') # add column names to the vector of values
all_curve_fits <- rbind(all_curve_fits, Curve272_output) # add the curve fits to the larger data frame



##########################################################
#### 
#### next step: fit all of the other 270ish curves, checking for any issues along the way
#### 
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
