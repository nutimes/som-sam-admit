# ---- Exploratory Data Analysis: Graphic --------------------------------------

########################### QUARTERLY ANALYSIS #############################

## Ungrouped quarterly time plot ----
quarterly_admissions |> 
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
quarterly_admissions |> 
  summarise_admissions(
    .group = TRUE,
    time = "Q"
  ) |> 
  autoplot() +
  facet_wrap(vars(region), scales = "free_y") +
  labs(
    title = "Time plot: Somalia's SAM admissions by Region by quarter",
    subtitle = "2019 Q1 : 2024 Q4",
    y = "Number of cases admitted"
  ) +
  theme(legend.position = "none")


### Seasonal plot ----
quarterly_admissions |> 
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
quarterly_admissions |> 
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
quarterly_admissions |> 
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
quarterly_admissions |> 
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

############################# MONTHLY ANALYSIS #############################

## Ungrouped time plot ----
summarise_admissions(
  ts = monthly_admissions,
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
  ts = monthly_admissions, 
  .group = TRUE,
  time = "M"
) |> 
  autoplot() +
  facet_wrap(vars(region), scales = "free_y") +
  labs(
    title = "Time plot: Somalia's SAM admissions by Region",
    subtitle = "January 2019 - November 2024",
    y = "Number of cases admitted"
  ) +
  theme(legend.position = "none")

## Seasonal plot ----
summarise_admissions(
  ts = monthly_admissions,
  .group = FALSE
) |> 
  gg_season(labels = "right") +
  labs(
    title = "Seasonal plot: Somalia's SAM admissions",
    subtitle = "January 2019 - November 2024",
    y = "Number of cases admitted"
  )

### Seasonal plot facetted by Region ----
summarise_admissions(
  ts = monthly_admissions,
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
  ts = monthly_admissions,
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
  ts = monthly_admissions,
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
  ts = monthly_admissions,
  .group = FALSE,
  time = "M"
) |> 
  gg_subseries() +
  labs(
    title = "Subseries plot: Somalia's SAM admissions",
    subtitle = "January 2019 - November 2024",
    y = "Number of cases admitted"
  )

### Subseries plot before 2022 ----
summarise_admissions(
  ts = monthly_admissions,
  .group = FALSE,
  time = "M"
) |> 
  filter(year(Monthly) < 2022) |> 
  gg_subseries() +
  labs(
    title = "Subseries plot: Somalia's SAM admissions",
    subtitle = "January 2019 - December 2021",
    y = "Number of cases admitted"
  )

### Subseries plot as of 2022 ----
summarise_admissions(
  ts = monthly_admissions,
  .group = FALSE,
  time = "M"
) |> 
  filter(year(Monthly) >= 2022) |> 
  gg_subseries() +
  labs(
    title = "Subseries plot: Somalia's SAM admissions",
    subtitle = "January 2022 - November 2024",
    y = "Number of cases admitted"
  )