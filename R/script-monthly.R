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
  filter(!(district %in% list)) |> 
  filter(!(region %in% c("Bay", "Banadir")))

# ---- TS Features -------------------------------------------------------------
## Sum of admissions by Region ----
som_admissions_monthly |> 
  summarise_admissions(
    .group = TRUE,
    time = "M"
  ) |> 
  features(
    .var = admissions, 
    features =quantile
  )

# ---- Graphics ----------------------------------------------------------------
## Ungrouped time plot ----
summarise_admissions(
  ts = som_admissions_monthly,
  .group = FALSE,
  time = "M"
)|> 
  autoplot() +
  labs(
    title = "Time plot: Somalia's SAM admissions",
    subtitle = "January 2019 - November 2024",
    y = "Number of cases admitted"
  )

### Time plot by Region ----
summarise_admissions(
  ts = som_admissions_monthly, 
  .group = TRUE,
  time = "M"
) |> 
  autoplot() +
  facet_wrap(vars(region)) +
  labs(
    title = "Time plot: Somalia's SAM admissions by Region",
    subtitle = "January 2019 - November 2024",
    y = "Number of cases admitted",
    x = "Time"
  ) +
  theme(legend.position = "none")

## Seasonal plot ----
summarise_admissions(
  ts = som_admissions_monthly,
  .group = FALSE
) |> 
  gg_season(labels = "right") +
  labs(
    title = "Seasonal plot: Somalia's SAM admissions",
    subtitle = "January 2019 - November 2024",
    y = "Number of cases admitted",
    y = "Time"
  )

### Seasonal plot facetted by Region ----
summarise_admissions(
  ts = som_admissions_monthly,
  .group = TRUE,
  time = "M"
) |> 
  gg_season() +
  facet_wrap(vars(region), scales = "free_y") +
  labs(
    title = "Seasonal plot: Somalia's SAM admissions by Region",
    subtitle = "January 2019 - November 2024",
    y = "Number of cases admitted"
  )

#### Seasonal plot before 2022 ----
summarise_admissions(
  ts = som_admissions_monthly,
  .group = FALSE,
  time = "M"
) |> 
  filter(year(Monthly) < 2022) |> 
  gg_season(labels = "right") +
  labs(
    title = "Seasonal plot: Somalia's SAM admissions",
    subtitle = "January 2019 - December 2021",
    caption = "End of 2021: impact of the severe drought",
    y = "Number of cases admitted"
  )

#### Seasonal plot after 2022 ----
summarise_admissions(
  ts = som_admissions_monthly,
  .group = FALSE,
  time = "M"
) |> 
  filter(year(Monthly) >= 2022) |> 
  gg_season(labels = "right") +
  labs(
    title = "Seasonal plot: Somalia's SAM admissions",
    subtitle = "January 2022 - November 2024",
    y = "Number of cases admitted"
  )

## Subseries plot ----
summarise_admissions(
  ts = som_admissions_monthly,
  .group = FALSE,
  time = "M"
) |> 
  gg_subseries() +
  labs(
    title = "Subseries plot: Somalia's SAM admissions",
    subtitle = "January 2019 - November 2024",
    y = "Number of cases admitted",
    y = "Time"
  )

### Subseries plot before 2022 ----
summarise_admissions(
  ts = som_admissions_monthly,
  .group = FALSE,
  time = "M"
) |> 
  filter(year(Monthly) < 2022) |> 
  gg_subseries() +
  labs(
    title = "Subseries plot: Somalia's SAM admissions",
    subtitle = "January 2019 - December 2021",
    y = "Number of cases admitted",
    y = "Time"
  )

### Subseries plot as of 2022 ----
summarise_admissions(
  ts = som_admissions_monthly,
  .group = FALSE,
  time = "M"
) |> 
  filter(year(Monthly) >= 2022) |> 
  gg_subseries() +
  labs(
    title = "Subseries plot: Somalia's SAM admissions",
    subtitle = "January 2022 - November 2024",
    y = "Number of cases admitted",
    y = "Time"
  )

# ---- Decomposition -----------------------------------------------------------
## Decompose non-transformed data using STL ----
cmpnts_mo <- summarise_admissions(
  ts = som_admissions_monthly,
  .group = TRUE,
  time = "M"
) |> 
  model(stl = STL(admissions)) |> 
  components()

## View components ----
autoplot(cmpnts_mo)

### Visualize the seasonal component by Region ----
#### Seasonal component over years ----
cmpnts_mo |> 
  select(season_year) |> 
  gg_season()

#### Using ungrouped data ----
summarise_admissions(
  ts = som_admissions_monthly,
  .group = FALSE,
  time = "M"
) |> 
  model(stl = STL(admissions)) |> 
  components() |> 
  select(season_year) |> 
  gg_season()
