# ---- Exploratory Data Analysis: Graphic --------------------------------------
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
