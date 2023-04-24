
library(tidyverse)

setwd("C:/Users/gld_mbeale/OneDrive - WSP O365/Beale/2023/bhs_cc_ms")

bhs <- read.csv("bhs_winter_data_20230422.csv")

clean <- bhs %>%
  dplyr::mutate(SheepID = as.factor(SheepID),
                Sex = as.factor(Sex),
                Year_ = as.factor(Year_),
                Month_ = as.factor(Month_)) %>%
  dplyr::select(SheepID,Year_,Month_) %>%
  distinct() %>%
  dplyr::group_by(SheepID,Year_,) %>%
  tally()

mean(clean$n) # 2.98 months
median(clean$n)

# of sheep
num <- bhs %>%
  dplyr::select(SheepID) %>%
  distinct()
nrow(num) # 40 --> only sheep contributing data to RSF are summarized in df -- good
