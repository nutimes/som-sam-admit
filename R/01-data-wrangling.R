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

# ---- Load data ---------------------------------------------------------------
## Remove the "X"'s before the dates ----
colnames(admissions) <- gsub("^X", "", colnames(admissions))
colnames(admissions) <- gsub("\\.", "-", colnames(admissions))

########################### QUARTERLY ANALYSIS #################################

# ---- Tidy the data -----------------------------------------------------------
quarterly_admissions <- admissions |> 
  select(2:75) |> 
  pivot_longer(
    cols = !c(region, district),
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
list <- c("Ceel_Dheere", "Jalalaqsi", "Sablaale", "Adan Yabaal",
 "Bu'aale", "Jilib", "Saakow/Salagle", "Sheik"
)
quarterly_admissions <- quarterly_admissions |> 
  filter(!(district %in% list))

# ---- TS Features -------------------------------------------------------------
## Sum of admissions by Region ----
quarterly_admissions |> 
  summarise_admissions(
    .group = TRUE,
    time = "Q"
  ) |> 
  features(
    .var = admissions, 
    features = quantile
  )

############################# MONTHLY ANALYSIS #################################

  monthly_admissions <- admissions |> 
    select(2:75) |> 
    pivot_longer(
      cols = !c(region, district),
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
  list <- c("Ceel_Dheere", "Jalalaqsi", "Sablaale", "Adan Yabaal",
   "Bu'aale", "Jilib", "Saakow/Salagle", "Sheik"
  )
  monthly_admissions <- monthly_admissions |> 
    filter(!(district %in% list))
  