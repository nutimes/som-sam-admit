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
sam_admit <- read_csv(
  file = "data/som_admissions.csv",
  col_types = NULL,
  col_select = -u5_population
)

########################### QUARTERLY ANALYSIS #############################

# ---- Tidy the data -----------------------------------------------------------
som_admissions_quarterly <- sam_admit |> 
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
som_admissions_quarterly <- som_admissions_quarterly |> 
  filter(!(district %in% list))

# ---- TS Features -------------------------------------------------------------
## Sum of admissions by Region ----
som_admissions_quarterly |> 
  summarise_admissions(
    .group = TRUE,
    time = "Q"
  ) |> 
  features(
    .var = admissions, 
    features = quantile
  )

############################# MONTHLY ANALYSIS #############################

  som_admissions_monthly <- sam_admit |> 
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
  )
  
  # ---- Remove districts with zero admissions -----------------------------------
  list <- c("Ceel_Dheere", "Jalalaqsi", "Sablaale", "Adan Yabaal",
   "Bu'aale", "Jilib", "Saakow/Salagle", "Sheik"
  )
  som_admissions_monthly <- som_admissions_monthly |> 
    filter(!(district %in% list))
  