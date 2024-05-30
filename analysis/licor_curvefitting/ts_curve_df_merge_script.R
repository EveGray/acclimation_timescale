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
Curve75_data <- subset(aci.df, unique_id == aci.df.unique_id[75]) # find correct curve from full dataframe and make new object
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
