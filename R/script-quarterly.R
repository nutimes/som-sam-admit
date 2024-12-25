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
  filter(!(district %in% list)) |> 
  filter(!(region %in% c("Bay", "Banadir")))

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

# ---- Graphics ----------------------------------------------------------------
## Ungrouped quarterly time plot ----
som_admissions_quarterly |> 
  summarise_admissions(
    .group = FALSE,
    time = "Q"
  ) |> 
  autoplot() +
  labs(
    title = "Time plot: Somalia's SAM admissions by quarter",
    subtitle = "2019 Q1 : 2024 Q4",
    y = "Number of cases admitted"
  )

### Quarterly time plot by Region ----
som_admissions_quarterly |> 
  summarise_admissions(
    .group = TRUE,
    time = "Q"
  ) |> 
  autoplot() +
  facet_wrap(vars(region)) +
  labs(
    title = "Time plot: Somalia's SAM admissions by Region by quarter",
    subtitle = "2019 Q1 : 2024 Q4",
    y = "Number of cases admitted"
  ) +
  theme(legend.position = "none")


### Seasonal plot ----
som_admissions_quarterly |> 
  summarise_admissions(
    .group = FALSE,
    time = "Q"
  ) |> 
  gg_season(labels = "right") +
  labs(
    title = "Seasonal plot: Somalia's SAM admissions by quarter",
    subtitle = "2019 Q1 : 2024 Q4",
    y = "Number of cases admitted",
    y = "Time"
  )

### Seasonal plot facetted by Region ----
som_admissions_quarterly |> 
  summarise_admissions(
    .group = TRUE,
    time = "Q"
  ) |> 
  gg_season() +
  facet_wrap(vars(region), scales = "free_y") +
  labs(
    title = "Seasonal plot: Somalia's SAM admissions by Region by quarter",
    subtitle = "2019 Q1 : 2024 Q4",
    y = "Number of cases admitted"
  )

### Subseries plot ----
som_admissions_quarterly |> 
  summarise_admissions(
    .group = FALSE,
    time = "Q"
  )|> 
  filter(year(Quarterly) == 2021) |> 
  gg_subseries() +
  labs(
    title = "Subseries plot: Somalia's SAM admissions by quarter",
    subtitle = "2019 Q1 : 2024 Q4",
    y = "Number of cases admitted"
  )

# ---- Decomposition -----------------------------------------------------------
## Decompose with STL, non-transformed data ----
cmpnts <- som_admissions_quarterly |> 
  summarise_admissions(
    .group = FALSE,
    time = "Q"
  )|> 
  model(STL(admissions)) |> 
  components()

### Visualize the seasonal component ----
### Seasonal component over years ----
cmpnts |> 
  select(season_year) |> 
  gg_season()

cmpnts|> 
  select(season_year) |> 
  gg_subseries()

### Trend component over years ----
cmpnts |> 
  select(trend) |> 
  gg_season()
