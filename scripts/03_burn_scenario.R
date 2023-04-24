##############################################################################################
##            R Code for BHS CC manuscript - CSP                                            ##
##            Burn scenario                                                                 ##
##            Script by Meghan Beale, WSP Canada Inc.                                       ##
##				    to use with R version 4.2.2 (2022-10-31) -- "Innocent and Trusting"           ##
##						Last updated: 23 April 2023                                                   ##
##############################################################################################

# clear environments
rm(list = ls())

# load libraries
library(tidyverse)
library(raster)
library(ggplot2)

###### Load raster TIFFs and compile into dataframe ######

# read rasters
rsf.rel.current <- raster("./input_data/raster_TIFFs/current_bhs_rsf_rel_burned_area.tif")
rsf.rel.postburn <- raster("./input_data/raster_TIFFs/postburn_bhs_rsf_rel_burned_area.tif")
rsf.bin.current <- raster("./input_data/raster_TIFFs/current_bhs_rsf_binned_burned_area.tif")
rsf.bin.postburn <- raster("./input_data/raster_TIFFs/postburn_bhs_rsf_binned_burned_area.tif")
forage.current <- raster("./input_data/raster_TIFFs/current_forage_qual_Mcal_burned_area.tif")
forage.postburn <- raster("./input_data/raster_TIFFs/postburn_forage_qual_Mcal_burned_area.tif")

# make all into df, remove NAs, join
df1 <- as.data.frame(rsf.rel.current) %>%
  drop_na()

df2 <- as.data.frame(rsf.rel.postburn) %>%
  drop_na()

df3 <- as.data.frame(rsf.bin.current) %>%
  drop_na() 

df4 <- as.data.frame(rsf.bin.postburn) %>%
  drop_na()

df5 <- as.data.frame(forage.current) %>%
  drop_na()

df6 <- as.data.frame(forage.postburn) %>%
  drop_na()

current <- cbind(df1,df3,df5) %>%
  dplyr::mutate(time = "Existing Landscape") %>%
  dplyr::rename(rsf.rel = "current_bhs_rsf_rel_burned_area",
                rsf.bin = "current_bhs_rsf_binned_burned_area",
                forage = "current_forage_qual_Mcal_burned_area")

postburn <- cbind(df2,df4,df6) %>%
  dplyr::mutate(time = "Post-Burn") %>%
  dplyr::rename(rsf.rel = "postburn_bhs_rsf_rel_burned_area",
                rsf.bin = "postburn_bhs_rsf_binned_burned_area",
                forage = "postburn_forage_qual_Mcal_burned_area")

join <- rbind(current,postburn)

###### Calculate proportion selected vs. avoided, pre- and post-burn ######

selection <- join %>%
  dplyr::group_by(rsf.bin,time) %>%
  dplyr::tally() %>%
  pivot_wider(names_from = "time", values_from = "n")

sum(selection$`Existing Landscape`) # 4454 total cells

# bins 9 + 10 are selected areas
(457 + 869) / 4454
0.2977099 # about 30% of area selected previously, pre-burn

(964 + 2044) / 4454
0.675348 # about 68% of area selected currently, post-burn

###### Equation for BHS CC, post-burn ######

# BHS CC = (sum of (F*U*A))*SUF / FR

# F = forage quality; measured in Mcal
# U = relative selection; proportion between 0-1, which weights the forage quality by how likely sheep are to use it
# A = area; based on winter range; proportion between 0-1, which weights F*U by the proportion of the area in winter range
# SUF = safe use factor; we can try multiple values here
# FR = forage requirements; we can try multiple values here; based on composition of BHS in Elk Valley East

###### Read raw data for F, U, and A ######

FUA <- read.csv("./input_data/bhs_energy_Fording_postburn.csv") # takes a few seconds; there are 5.3 million rows

# clean up df
fua <- FUA %>%
  dplyr::select(-X)

###### Calculate new post-burn carrying capacity for Fording subpop ######

cc <- fua %>%
  dplyr::mutate(FUA = ForageQual*PropWinter*Rel_BHSUse) %>% # calculate product of F, U, and A
  dplyr::summarise(TotalMcal = sum(FUA)) %>% # sum total energy in each bhs subpopulation; there are some winter range pixels falling outside pre-determined subpopulation polygons; these are represented in blank subpopulation
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
  pivot_longer(!(TotalMcal:days.winter), names_to = "performance", values_to = "daily.kcal") %>%
  dplyr::mutate(FR.Mcal = daily.kcal*days.winter*(1/1000)) %>% # convert from kcal to Mcal
  dplyr::mutate(intense = 0.75) %>% # apply three SUF
  dplyr::mutate(moderate = 0.50) %>%
  dplyr::mutate(conservative = 0.25) %>%
  pivot_longer(!(TotalMcal:FR.Mcal), names_to = "scenario", values_to = "SUF") %>%
  dplyr::mutate(CC = (TotalMcal*SUF)/FR.Mcal) # calculate CC for each subpop x perf class x SUF

## END OF CODE