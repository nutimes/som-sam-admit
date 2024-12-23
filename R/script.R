#%% Load required libraries ----
library(readr)
library(tidyr)
library(dplyr)
library(lubridate)
library(tsibble)
library(feasts)

#%% Load data ----
sam_admit <- read_csv(
  file = "data/som_admissions.csv",
  col_types = NULL,
  col_select = -u5_population
)

#%% Tidy data ---
  som_sam_admissions <- sam_admit |> 
    pivot_longer(
      cols = !c(region, district),
      names_to = "time",
      values_to = "admissions"
    ) |> 
    mutate(
      time = ymd(time),
      quarter = yearquarter(time)
    ) |> 
    relocate(
      quarter, 
      .before = admissions
    ) |> 
    group_by(region, district, quarter) |> 
    summarise(admissions = sum(admissions, na.rm = TRUE)) |> 
    tsibble(
      key = c(region, district),
      index = quarter
    )