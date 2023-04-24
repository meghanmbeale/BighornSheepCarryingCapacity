##############################################################################################
##            R Code for BHS CC manuscript - CSP                                            ##
##            Calculate carrying capacity                                                   ##
##            Script by Meghan Beale, WSP Canada Inc.                                       ##
##				    to use with R version 4.2.2 (2022-10-31) -- "Innocent and Trusting"           ##
##						Last updated: 23 April 2023                                                   ##
##############################################################################################

# clear environments
rm(list = ls())

# load libraries
library(tidyverse)

###### Equation for BHS CC ######

# BHS CC = (sum of (F*U*A))*SUF / FR

# F = forage quality; measured in Mcal
# U = relative selection; proportion between 0-1, which weights the forage quality by how likely sheep are to use it
# A = area; based on winter range; proportion between 0-1, which weights F*U by the proportion of the area in winter range
# SUF = safe use factor; we can try multiple values here
# FR = forage requirements; we can try multiple values here; based on composition of BHS in Elk Valley East

###### Read raw data for F, U, and A ######

FUA <- read.csv("./input_data/bhs_energy_in_winter_ranges.csv") # takes a few seconds; there are 5.3 million rows

# clean up df
fua <- FUA %>%
  dplyr::select(-X) %>%
  dplyr::mutate(SubPop = as.factor(SubPop)) %>%
  dplyr::mutate(SubPop = fct_recode(SubPop,
                             "NA" = ""))

###### Calculate area of winter range within each BHS subpopulation ######

areas <- fua %>%
  dplyr::mutate(area = 625) %>% # area of each 25 x 25-m raster cell (i.e., 625 m sq)
  dplyr::group_by(SubPop) %>%
  dplyr::summarise(total.area.m2 = sum(area*PropWinter)) %>%
  dplyr::mutate(total.area.km2 = total.area.m2*(1/1000000)) # 1 km sq = 10^6 m sq

###### Calculate carrying capacity per bighorn sheep subpopulation ######

cc <- fua %>%
  dplyr::mutate(FUA = ForageQual*PropWinter*Rel_BHSUse) %>% # calculate product of F, U, and A
  dplyr::group_by(SubPop) %>%
  dplyr::summarise(TotalMcal = sum(FUA)) %>% # sum total energy in each bhs subpopulation; there are some winter range pixels falling outside pre-determined subpopulation polygons; these are represented in blank subpopulation
  dplyr::ungroup() %>%
  pivot_wider(names_from = "SubPop", values_from = "TotalMcal") %>%
  dplyr::mutate(AllWinterRange = `NA` + `Crossing Ck` + `Crowsnest North` + `Erickson Sheep M` +
                  `EV West Hornaday` + `Ewin Ck` + `Fording` + `Upper Elk East` + `Upper Elk West`) %>% 
  pivot_longer(names_to = "SubPop", values_to = "TotalMcal", `NA`:AllWinterRange) %>%
  dplyr::mutate(bm.kg = 72.1) %>% # this is the mean body mass (in kg) that we determined to represent 'one sheep'; methods summarized in manuscript
  dplyr::mutate(daily.kcal.rate = ((460*(bm.kg^0.75))*(1/4.184))) %>% # 1 kcal = 4.184 kJ; convert to calories; methods to determine this value are summarized in manuscript
  # based on 2009-2010 data, winter season was 150 days
  # based on 2010-2011 data, winter season was 136 days
  # based on 2011-2012 data, winter season was 137 days
  # mean is 141 days
  dplyr::mutate(days.winter = 141) %>% # mean number of days in winter season; based on telemetry data
  dplyr::rename(middle = daily.kcal.rate) %>% # apply three performance classes
  dplyr::mutate(good = (0.10*middle)+middle) %>%
  dplyr::mutate(maint = (middle/(1+0.10))) %>%
  dplyr::select(-middle,middle) %>%
  pivot_longer(!(SubPop:days.winter), names_to = "performance", values_to = "daily.kcal") %>%
  dplyr::mutate(FR.Mcal = daily.kcal*days.winter*(1/1000)) %>% # convert from kcal to Mcal
  dplyr::mutate(intense = 0.75) %>% # apply three SUF
  dplyr::mutate(moderate = 0.50) %>%
  dplyr::mutate(conservative = 0.25) %>%
  pivot_longer(!(SubPop:FR.Mcal), names_to = "scenario", values_to = "SUF") %>%
  dplyr::mutate(CC = (TotalMcal*SUF)/FR.Mcal) # calculate CC for each subpop x perf class x SUF

# note that there are three subpops not in the EVE, which can be ignored

## END OF CODE ##