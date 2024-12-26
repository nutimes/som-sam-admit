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
    y = "Number of cases admitted"
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
  gg_subseries() +
  labs(
    title = "Subseries plot: Somalia's SAM admissions by quarter",
    subtitle = "2019 Q1 : 2024 Q4",
    y = "Number of cases admitted"
  )

### Subseries plot by Region ----
som_admissions_quarterly |> 
  summarise_admissions(
    .group = TRUE,
    time = "Q"
  )|>
  gg_subseries() +
  labs(
    title = "Subseries plot: Somalia's SAM admissions by by Region by quarter",
    subtitle = "2019 Q1 : 2024 Q4",
    y = "Number of cases admitted"
  )

# ---- Decomposition -----------------------------------------------------------
## Decompose with STL, non-transformed data ----
### Grouped by Region ----
cmpnts <- som_admissions_quarterly |> 
  summarise_admissions(
    .group = TRUE,
    time = "Q"
  )|> 
  model(STL(admissions)) |> 
  components()

#### Visualize the components by Region ----
cmpnts |> 
autoplot()

#### Visualize the seasonal component by Region ----
### Seasonal component over years ----
cmpnts |> 
  select(season_year) |> 
  gg_season()

# ---- Multiple Regression Model -----------------------------------------------
## Check trend linearity ----
som_admissions_quarterly |> 
  summarise_admissions(
    .group = FALSE,
    time = "Q"
  ) |> 
  ggplot(aes(x = Quarterly, y = admissions)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE)

## Specify models ----
fit_admissions <- som_admissions_quarterly |> 
  summarise_admissions(
    .group = FALSE,
    time = "Q"
  ) |> 
  model(
    linear = TSLM(admissions ~ trend() + season()),
    exp_log_trend = TSLM(admissions ~ log(trend()) + season()),
    exp_log_admissions = TSLM(log(admissions) ~ log(trend()) + season()),
    piecewise = TSLM(
      log(admissions) ~ trend(knots = c(yearquarter("2020 Q1"),
                                   yearquarter("2020 Q2"),
                                   yearquarter("2021 Q1"),
                                   yearquarter("2023 Q1"))) + season()
    )
  )

### Evaluate residuals ----
fit_admissions |> 
  select(piecewise) |> 
  gg_tsresiduals()
