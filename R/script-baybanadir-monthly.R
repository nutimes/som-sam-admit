# ---- Load required libraries -------------------------------------------------
library(readr)
library(tidyr)
library(dplyr)
library(lubridate)
library(tsibble)
library(feasts)
library(ggplot2)

# ---- Load utility functions --------------------------------------------------
source("R/utils.R")

# ---- Load data ---------------------------------------------------------------
sam_admit <- read_csv(
  file = "data/som_admissions.csv",
  col_types = NULL,
  col_select = -u5_population
)

# ---- Tidy the data -----------------------------------------------------------
som_sam_admissions <- sam_admit |> 
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
 "Bu'aale", "Jilib", "Saakow/Salagle", "Sheik")

bay_banadir <- som_sam_admissions |> 
  filter(!(district %in% list)) |> 
  filter(region %in% c("Bay", "Banadir"))  # Keep Bay and Banadir 

# ---- TS Features -------------------------------------------------------------
## Sum of admissions by Region ----
summarise_admissions(
  ts = bay_banadir,
  .group = TRUE,
  time = "M"
) |> 
  features(
    .var = admissions, 
    features = quantile
  )

# ---- Graphics ----------------------------------------------------------------
## Time plot ----
bay_ban <- summarise_admissions(
  ts = bay_banadir,
  .group = TRUE,
  time = "M"
)

bay_ban |> 
  autoplot() +
  labs(
    title = "Time plot: Bay and Banadir's SAM admissions",
    subtitle = "From January 2019 - November 2024",
    y = "Number of cases admitted"
  )

## Seasonal plot ----
bay_ban |> 
  gg_season() +
  labs(
    title = "Seasonal plot: Bay and Banadir's SAM admissions",
    subtitle = "From January 2019 - November 2024",
    y = "Number of cases admitted"
  )

## Subseries plot ----
bay_ban |> 
  gg_subseries() +
  labs(
    title = "Subseries plot: Bay and Banadir's SAM admissions",
    subtitle = "From January 2019 - November 2024",
    y = "Number of cases admitted"
  )

# ---- Decomposition -----------------------------------------------------------
### Decompose non-transformed data using STL ----
dcmp_bay_ban <- bay_ban |> 
  model(STL(admissions)) |> 
  components()

### Visualize components ----
autoplot(dcmp_bay_ban)

### Seasonal component over years ----
dcmp_bay_ban |> 
  select(season_year) |> 
  gg_season()