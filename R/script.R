# Load required libraries ----
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
) |> 
  as_tsibble(
    index = Monthly, 
    key = c(region, district)
  )

# ---- Remove districts with zero admissions --------------------------------------------
list <- c("Ceel_Dheere", "Jalalasi", "Sablaale", "Adan Yabaal",
 "Bu'aale", "Jilib", "Saakow/Salagle", "Sheik"
)
som_sam_admissions <- som_sam_admissions |> 
  filter(!(district %in% list)) |> 
  filter(!(region %in% c("Bay", "Banadir")))

# ---- TS Features ----------------------------------------------------------------------
## Sum of admissions by Region ----
som_sam_admissions |> 
  group_by(region) |> 
  summarise(admissions = sum(admissions, na.rm = T)) |> 
  features(admissions, quantile)

# ---- Graphics -------------------------------------------------------------------------
## Ungrouped time plot ----
manipulate_tsibble(
  ts = som_sam_admissions,
  .by = "ungrouped"
)|> 
  autoplot() +
  labs(
    title = "Time plot: Somalia's SAM admissions",
    subtitle = "January 2019 - November 2024",
    y = "Number of cases admitted"
  )

### Time plot by Region ----
manipulate_tsibble(
  ts = som_sam_admissions, 
  .by = "grouped"
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
manipulate_tsibble(
  ts = som_sam_admissions,
  .by = "ungrouped"
) |> 
  gg_season(labels = "right") +
  labs(
    title = "Seasonal plot: Somalia's SAM admissions",
    subtitle = "January 2019 - November 2024",
    y = "Number of cases admitted",
    y = "Time"
  )

### Seasonal plot facetted by Region ----
manipulate_tsibble(
  ts = som_sam_admissions,
  .by = "grouped"
) |> 
  gg_season() +
  facet_wrap(vars(region)) +
  labs(
    title = "Seasonal plot: Somalia's SAM admissions by Region",
    subtitle = "January 2019 - November 2024",
    y = "Number of cases admitted"
  )

#### Seasonal plot before 2022 ----
manipulate_tsibble(
  ts = som_sam_admissions,
  .by = "ungrouped"
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
manipulate_tsibble(
  ts = som_sam_admissions,
  .by = "ungrouped"
) |> 
  filter(year(Monthly) >= 2022) |> 
  gg_season(labels = "right") +
  labs(
    title = "Seasonal plot: Somalia's SAM admissions",
    subtitle = "January 2022 - November 2024",
    y = "Number of cases admitted"
  )

## Subseries plot ----
manipulate_tsibble(
  ts = som_sam_admissions,
  .by = "ungrouped"
) |> 
  gg_subseries() +
  labs(
    title = "Subseries plot: Somalia's SAM admissions",
    subtitle = "January 2019 - November 2024",
    y = "Number of cases admitted",
    y = "Time"
  )

### Subseries plot before 2022 ----
manipulate_tsibble(
  ts = som_sam_admissions,
  .by = "ungrouped"
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
manipulate_tsibble(
  ts = som_sam_admissions,
  .by = "ungrouped"
) |> 
  filter(year(Monthly) >= 2022) |> 
  gg_subseries() +
  labs(
    title = "Subseries plot: Somalia's SAM admissions",
    subtitle = "January 2022 - November 2024",
    y = "Number of cases admitted",
    y = "Time"
  )

# ---- Decomposition --------------------------------------------------------------
## Box-cox transformation to make evenly distributed the variation in the seasonal patterns ----
### Calculate lambda ----
lambda <- manipulate_tsibble(
    ts = som_sam_admissions,
    .by = "ungrouped"
  ) |> 
  features(admissions, features = guerrero) |> 
  pull(lambda_guerrero)

### Data transformation ----
box_cox_transformed <- manipulate_tsibble(
  ts = som_sam_admissions,
  .by = "ungrouped"
) |> 
  mutate(
    admissions = box_cox(x = admissions, lambda = lambda)
  )

### Decompose using STL ----
box_cox_transformed |> 
  filter(as.character(Monthly) != "2024 Dec") |> 
  model(stl = STL(admissions)) |> 
  components() |> 
  autoplot()

### Decompose non-transformed data using STL ----
dcmp <- manipulate_tsibble(
  ts = som_sam_admissions,
  .by = "ungrouped"
) |> 
  model(stl = STL(admissions)) |> 
  components()

### View components ----
autoplot(dcmp)

### Seasonal component over year ----
dcmp |> 
  select(season_year) |> 
  gg_season()

### Trend component over year ----
dcmp |> 
  select(trend) |> 
  gg_season()
