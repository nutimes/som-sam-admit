# ---- Load required libraries -----------------------------------------------------------
library(readr)
library(tidyr)
library(dplyr)
library(lubridate)
library(tsibble)
library(feasts)
library(ggplot2)

# ---- Load data -------------------------------------------------------------------------
sam_admit <- read_csv(
  file = "data/som_admissions.csv",
  col_types = NULL,
  col_select = -u5_population
)

# ---- Tidy the data ---------------------------------------------------------------------
som_sam_admissions <- sam_admit |> 
  pivot_longer(
    cols = !c(region, district),
    names_to = "time",
    values_to = "admissions"
  ) |> 
  mutate(
    time = ymd(time),
    Monthly = yearmonth(time),
    quarterly = yearquarter(time)
  ) |> 
  relocate(
    Monthly, 
    .before = admissions
)

# ---- Remove districts with zero admissions --------------------------------------------
list <- c("Ceel_Dheere", "Jalalasi", "Sablaale", "Adan Yabaal",
 "Bu'aale", "Jilib", "Saakow/Salagle", "Sheik")

som_sam_admissions <- som_sam_admissions |> 
  filter(!(district %in% list)) |> 
  as_tsibble(index = Monthly, key = c(region, district))

# ---- Keep Bay and Banadir  ------------------------------------------------------------
list <- c("Bay", "Banadir")
bay_banadir <- som_sam_admissions |> 
  filter(region %in% list) |> 
  as_tsibble(
    index = Monthly,
    key = c(region, district)
  )

# ---- TS Features ----------------------------------------------------------------------
## Sum of admissions by Region ----
quantiles <- bay_banadir |> 
  group_by(region) |> 
  summarise(admissions = sum(admissions, na.rm = T)) |> 
  features(admissions, quantile)

# ---- Graphics -------------------------------------------------------------------------
## Time plot ----
bb <- manipulate_tsibble(
  ts = bay_banadir,
  .by = "grouped"
)

timeplt <- bb |> 
  autoplot() +
  labs(
    title = "Time plot: Bay and Banadir's SAM admissions",
    subtitle = "From January 2019 - November 2024",
    y = "Number of cases admitted"
  )

## Seasonal plot ----
sesonplt <- bb |> 
  gg_season() +
  labs(
    title = "Seasonal plot: Bay and Banadir's SAM admissions",
    subtitle = "From January 2019 - November 2024",
    y = "Number of cases admitted"
  )

## Subseries plot ----
subsplt <- bb |> 
  gg_subseries() +
  labs(
    title = "Subseries plot: Bay and Banadir's SAM admissions",
    subtitle = "From January 2019 - November 2024",
    y = "Number of cases admitted"
  )

# ---- Decomposition --------------------------------------------------------------
### Decompose non-transformed data using STL ----
dcmp <- bb |> 
  model(stl = STL(admissions)) |> 
  components()

dcmp |> 
  autoplot()
