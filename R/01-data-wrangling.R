# ---- Load required libraries -------------------------------------------------
library(readr)
library(tidyr)
library(dplyr)
library(lubridate)
library(tsibble)
library(feasts)
library(ggplot2)
library(fable)

# ---- Load utility functions --------------------------------------------------
source("R/utils.R")

########################### FOR MONTHLY ANALYSIS ###############################

# ---- Tidy the data -----------------------------------------------------------
monthly_admissions <- admissions |> 
  pivot_longer(
    cols = !c(region, district, lsystems),
    names_to = "time",
    values_to = "admissions"
  ) |> 
  mutate(
    time = ymd(time),
    Monthly = yearmonth(time)
  ) |> 
  relocate(
    Monthly, 
    .before = admissions
) |> 
  select(-time)

# ---- Remove districts with zero admissions ---------------------------------
list <- c("Ceel_Dheere", "Jalalaqsi", "Jamaame", "Kurtunwaarey", "Sablaale", 
"Adan Yabaal", "Bu'aale", "Jilib", "Saakow/Salagle", "Sheik", "Cadale", 
"Xarardheere")

monthly_admissions <- monthly_admissions |> 
  filter(!(district %in% list))


######################## FOR QUARTERLY ANALYSIS ################################

# ---- Tidy the data -----------------------------------------------------------
quarterly_admissions <- admissions |> 
  pivot_longer(
    cols = !c(region, district, lsystems),
    names_to = "time",
    values_to = "admissions"
  ) |> 
  mutate(
    time = ymd(time),
    Quarterly = yearquarter(time)
  ) |> 
  relocate(
    Quarterly, 
    .before = admissions
) |> 
  select(-time)

# ---- Remove districts with zero admissions -----------------------------------
list <- c("Ceel_Dheere", "Jalalaqsi", "Jamaame", "Kurtunwaarey", "Sablaale", 
"Adan Yabaal", "Bu'aale", "Jilib", "Saakow/Salagle", "Sheik", "Cadale", 
"Xarardheere")

quarterly_admissions <- quarterly_admissions |> 
  filter(!(district %in% list))
  