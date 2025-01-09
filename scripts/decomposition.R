################################################################################
# As shown in the time, seasonal and subseries plot in the `eda-graphics.R` file,
# the data shows variations that increase and decrease with the level of the serie
# This necessitates the data to be transformed to stabelize the seasonal variation.

#   Decomposition is done at national and then split into livelihood systems.
################################################################################

################################# NATIONAL #####################################

## ---- Box-Cox transformation -------------------------------------------------

### Summarise data ----
na <- monthly_admissions |> 
  summarise_admissions(
    .group = FALSE,
    time = "M"
  )

### Get lambda ----
lambda_national <- na |> 
  features(
    .var = admissions,
    features = guerrero
  ) |> 
  pull(lambda_guerrero)

### Visualize the transformation ----
na |> 
  autoplot(
    box_cox(
      x = admissions,
      lambda = lambda_national
    )
  )


## ---- Decomposition ----------------------------------------------------------

### Get components ----
cmpnts_national <- na |> 
  model(
    STL(
      box_cox(
        x = admissions, 
        lambda = lambda_national
      ) ~ trend(window = 9) + season(window = 7)
    )
  ) |> 
  components()

### Visualize the components ----
cmpnts_plot_national <- cmpnts_national |> 
  autoplot() + 
  labs(
    title = "The Components of the SAM Admissions at National Level",
    subtitle = "Seasonal patterns shifted as of 2022"
  )+
  theme(
    plot.subtitle = element_text(colour = "#706E6D"),
    axis.title.y = element_text(size = 10, margin = margin(r = 5)),
    axis.title.x = element_text(size = 10, margin = margin(r = 5))
  )

### Plot the seasonal component over years ----
seasonal_cmpnt_national <- cmpnts_national |> 
  select(season_year) |> 
  gg_season(y = season_year) +
  labs(
    title = "Seasonal Patterns Over Time at National Level",
    subtitle = "The highest peak of admissions is reached in June every year",
    caption = "",
    y = "Seasonal effects",
    colour = "Year"
  ) +
  theme(
    plot.subtitle = element_text(colour = "#706E6D"),
    plot.caption = element_text(colour = '#706E6D'), 
    axis.title.y = element_text(size = 10, margin = margin(r = 5)),
    axis.title.x = element_text(size = 10, margin = margin(t = 5))
  )

#### Subset the seasonal component before 2022 ----
seasonal_cmpnt_national_b2022 <- cmpnts_national |> 
  filter(year(Monthly) < 2022) |> 
  select(season_year) |> 
  gg_season(y = season_year) +
  labs(
    title = "Seasonal Patterns Over Time at National Level",
    subtitle = "The highest peak of admissions is reached in June every year",
    caption = "",
    y = "Seasonal effects",
    colour = "Year"
  ) +
  theme(
    plot.subtitle = element_text(colour = "#706E6D"),
    plot.caption = element_text(colour = '#706E6D'), 
    axis.title.y = element_text(size = 10, margin = margin(r = 5)),
    axis.title.x = element_text(size = 10, margin = margin(t = 5))
  )

#### Subset the seasonal component as of 2022 ----
seasonal_cmpnt_national_a2022 <- cmpnts_national |> 
  filter(year(Monthly) >= 2022) |> 
  select(season_year) |> 
  gg_season(y = season_year) +
  labs(
    title = "Seasonal Patterns Over Time at National Level",
    subtitle = "The highest peak of admissions is reached in June every year",
    caption = "",
    y = "Seasonal effects",
    colour = "Year"
  ) +
  theme(
    plot.subtitle = element_text(colour = "#706E6D"),
    plot.caption = element_text(colour = '#706E6D'), 
    axis.title.y = element_text(size = 10, margin = margin(r = 5)),
    axis.title.x = element_text(size = 10, margin = margin(t = 5))
  )

######################### BY LIVELIHOOD SYSTEMS ################################

### Summarise data ----
mo <- monthly_admissions |> 
  summarise_admissions(
    .group = TRUE,
    time = "M"
  )

## ---- Box-Cox transformation -------------------------------------------------

### Get lambda for livelihood systems ----
#### For Pastoral ----
lambda_pastoral <- mo |> 
  filter(lsystems == "Pastoral") |> 
  features(
    .var = admissions,
    features = guerrero
  ) |> 
  pull(lambda_guerrero)

#### For Agropastoral ----
lambda_agropastoral <- mo |> 
  filter(lsystems == "Agropastoral") |> 
  features(
    .var = admissions,
    features = guerrero
  ) |> 
  pull(lambda_guerrero)

#### For Riverine ----
lambda_rivernine <- mo |> 
  filter(lsystems == "Riverine") |> 
  features(
    .var = admissions,
    features = guerrero
  ) |> 
  pull(lambda_guerrero)

#### For Urban/IDP's ----
lambda_urban_idps <- mo |> 
  filter(lsystems == "Urban/IDPs") |> 
  features(
    .var = admissions,
    features = guerrero
  ) |> 
  pull(lambda_guerrero)


## Visualize the time series after transformation ----
### Pastoral ----
mo |> 
  filter(lsystems == "Pastoral") |> 
  autoplot(
    box_cox(
      x = admissions, 
      lambda = lambda_pastoral
    )
  )

### Agropastoral ----
mo |> 
  filter(lsystems == "Agropastoral") |> 
  autoplot(
    box_cox(
      x = admissions, 
      lambda = lambda_agropastoral
    )
  )

### Riverine ----
mo |> 
  filter(lsystems == "Riverine") |> 
  autoplot(
    box_cox(
      x = admissions, 
      lambda = lambda_rivernine
    )
  )

### Urban/IDPs ----
mo |> 
  filter(lsystems == "Urban/IDPs") |> 
  autoplot(
    box_cox(
      x = admissions, 
      lambda = lambda_urban_idps
    )
  )


## ---- Decomposition ----------------------------------------------------------
### -------------------------------------------- Pastoral livelihood system ----

#### Get components ----
cmpnts_pastoral <- mo |> 
  filter(lsystems == "Pastoral") |> 
  model(
    STL(
      box_cox(
        x = admissions, 
        lambda = lambda_pastoral
      ) ~ trend(window = 9) + season(window = 7)
    )
  ) |> 
  components()

#### Visualize the components ----
cmpnts_plot_pastoral <- cmpnts_pastoral |> 
  autoplot() + 
  labs(
    title = "Trends and Seasonal Patterns in Pastoral Livelihood System",
    subtitle = NULL
  ) +
  theme(
    plot.title = element_text(size = 12, margin = margin(b = 10)),
    axis.title.y = element_text(size = 10, margin = margin(r = 5)),
    axis.title.x = element_text(size = 10, margin = margin(r = 5))
  )
  
#### Plot the seasonal component over years ----
seasonal_cmpnt_pastoral <- cmpnts_pastoral |> 
  select(season_year) |> 
  gg_season(y = season_year) + 
  labs(
    title = "Seasonal Patterns in Pastoral Livelihood Systems",
    subtitle = "Seasonal patterns shifted as of 2022",
    y = "Seasonal effects"
  ) +
  theme(
    plot.subtitle = element_text(colour = "#706E6D"),
    plot.caption = element_text(colour = '#706E6D'), 
    axis.title.y = element_text(size = 10, margin = margin(r = 5)),
    axis.title.x = element_text(size = 10, margin = margin(r = 5))
  )

##### Subset the seasonal component before 2022 ----
seasonal_cmpnt_pastoral_b2022 <- cmpnts_pastoral |> 
  filter(year(Monthly) < 2022) |> 
  select(season_year) |> 
  gg_season(y = season_year) + 
  labs(
    title = "Seasonal Patterns in Pastoral Livelihood Systems",
    subtitle = "Seasonal patterns shifted as of 2022",
    y = "Seasonal effects"
  ) +
  theme(
    plot.subtitle = element_text(colour = "#706E6D"),
    plot.caption = element_text(colour = '#706E6D'), 
    axis.title.y = element_text(size = 10, margin = margin(r = 5)),
    axis.title.x = element_text(size = 10, margin = margin(r = 5))
  )

##### Subset the seasonal component as of 2022 ----
seasonal_cmpnt_pastoral_a2022 <- cmpnts_pastoral |> 
  filter(year(Monthly) >= 2022) |> 
  select(season_year) |> 
  gg_season(y = season_year) + 
  labs(
    title = "Seasonal Patterns in Pastoral Livelihood Systems",
    subtitle = "Seasonal patterns shifted as of 2022",
    y = "Seasonal effects"
  ) +
  theme(
    plot.subtitle = element_text(colour = "#706E6D"),
    plot.caption = element_text(colour = '#706E6D'), 
    axis.title.y = element_text(size = 10, margin = margin(r = 5)),
    axis.title.x = element_text(size = 10, margin = margin(r = 5))
  )

### ---------------------------------------- Agropastoral livelihood system ----

#### Get components ----
cmpnts_agropastoral <- mo |> 
  filter(lsystems == "Agropastoral") |> 
  model(
    STL(
      box_cox(
        x = admissions, 
        lambda = lambda_agropastoral
      ) ~ trend(window = 9) + season(window = 7)
    )
  ) |> 
  components()

#### Visualize the components ----
cmpnts_plot_agropastoral <- cmpnts_agropastoral |> 
  autoplot() +
  labs(
    title = "Trends and Seasonal Patterns in Agropastoral Livelihood System",
    subtitle = NULL,
  ) +
  theme(
    plot.title = element_text(size = 12, margin = margin(b = 10)),
    axis.title.y = element_text(size = 10, margin = margin(r = 5)),
    axis.title.x = element_text(size = 10, margin = margin(r = 5))
  )

#### Plot the seasonal component over years ----
seasonal_cmpnt_agropastoral <- cmpnts_agropastoral |> 
  select(season_year) |> 
  gg_season(y = season_year) + 
  labs(
    title = "Seasonal Patterns in Agropastoral Livelihood Systems",
    subtitle = "The highest peak of admissions is reached in June every year",
    y = "Seasonal effects"
  ) +
  theme(
    plot.subtitle = element_text(colour = "#706E6D"),
    axis.title.y = element_text(size = 10, margin = margin(r = 5)),
    axis.title.x = element_text(size = 10, margin = margin(r = 5))
  )

#### Subset the seasonal component before 2022 ----
seasonal_cmpnt_agropastoral_b2022 <- cmpnts_agropastoral |> 
  filter(year(Monthly) < 2022) |> 
  select(season_year) |> 
  gg_season(y = season_year) + 
  labs(
    title = "Seasonal Patterns in Agropastoral Livelihood Systems",
    subtitle = "Seasonal patterns shifted as of 2022",
    y = "Seasonal effects"
  ) +
  theme(
    plot.subtitle = element_text(colour = "#706E6D"),
    plot.caption = element_text(colour = '#706E6D'), 
    axis.title.y = element_text(size = 10, margin = margin(r = 5)),
    axis.title.x = element_text(size = 10, margin = margin(r = 5))
  )

#### Subset the seasonal component as of 2022 ----
seasonal_cmpnt_agropastoral_a2022 <- cmpnts_agropastoral |> 
  filter(year(Monthly) >= 2022) |> 
  select(season_year) |> 
  gg_season(y = season_year) + 
  labs(
    title = "Seasonal Patterns in Agropaastoral Livelihood Systems",
    subtitle = "Seasonal patterns shifted as of 2022",
    y = "Seasonal effects"
  ) +
  theme(
    plot.subtitle = element_text(colour = "#706E6D"),
    plot.caption = element_text(colour = '#706E6D'), 
    axis.title.y = element_text(size = 10, margin = margin(r = 5)),
    axis.title.x = element_text(size = 10, margin = margin(r = 5))
  )

### -------------------------------------------- Riverine livelihood system ----

#### Get components ----
cmpnts_riverine <- mo |> 
  filter(lsystems == "Riverine") |> 
  model(
    STL(
      box_cox(
        x = admissions, 
        lambda = lambda_rivernine
      ) ~ trend(window = 9) + season(window = 7)
    )
  ) |> 
  components()

#### Visualize the components ----
cmpnts_plot_riverine <- cmpnts_riverine |> 
  autoplot() +
  labs(
    title = "Trends and Seasonal Patterns in Riverine Livelihood System",
    subtitle = NULL,
  ) +
  theme(
    plot.title = element_text(size = 12, margin = margin(b = 10)),
    axis.title.y = element_text(size = 10, margin = margin(r = 5)),
    axis.title.x = element_text(size = 10, margin = margin(r = 5))
  )
  
#### Plot the seasonal component over years ----
seasonal_cmpnt_riverine <- cmpnts_riverine |> 
  select(season_year) |> 
  gg_season(y = season_year) + 
  labs(
    title = "Seasonal Patterns in Riverine Livelihood Systems",
    subtitle = "Several high peaks in the admissions",
    y = "Seasonal effects",
  ) +
  theme(
    plot.subtitle = element_text(colour = "#706E6D"),
    axis.title.y = element_text(size = 10, margin = margin(r = 5)),
    axis.title.x = element_text(size = 10, margin = margin(r = 5))
  )

##### Subset the seasonal component before 2022 ----
seasonal_cmpnt_riverine_b2022 <- cmpnts_riverine |> 
  filter(year(Monthly) < 2022) |> 
  select(season_year) |> 
  gg_season(y = season_year) + 
  labs(
    title = "Seasonal Patterns in Riverine Livelihood Systems",
    subtitle = "Seasonal patterns shifted as of 2022",
    y = "Seasonal effects"
  ) +
  theme(
    plot.subtitle = element_text(colour = "#706E6D"),
    plot.caption = element_text(colour = '#706E6D'), 
    axis.title.y = element_text(size = 10, margin = margin(r = 5)),
    axis.title.x = element_text(size = 10, margin = margin(r = 5))
  )

##### Subset the seasonal component as of 2022 ----
seasonal_cmpnt_riverine_a2022 <- cmpnts_riverine |> 
  filter(year(Monthly) >= 2022) |> 
  select(season_year) |> 
  gg_season(y = season_year) + 
  labs(
    title = "Seasonal Patterns in Riverine Livelihood Systems",
    subtitle = "Seasonal patterns shifted as of 2022",
    y = "Seasonal effects"
  ) +
  theme(
    plot.subtitle = element_text(colour = "#706E6D"),
    plot.caption = element_text(colour = '#706E6D'), 
    axis.title.y = element_text(size = 10, margin = margin(r = 5)),
    axis.title.x = element_text(size = 10, margin = margin(r = 5))
  )

### ------------------------------------------ Urban/IDPs livelihood system ----

#### Get component ----
cmpnts_urban_idps <- mo |> 
  filter(lsystems == "Urban/IDPs") |> 
  model(
    STL(
      box_cox(
        x = admissions, 
        lambda = lambda_urban_idps
      ) ~ trend(window = 9) + season(window = 7)
    )
  ) |> 
  components()

#### Visualize the components ----
cmpnts_plot_urban_idps <- cmpnts_urban_idps |> 
  autoplot() + 
  labs(
    title = "Trends and Seasonal Patterns in Urban/IDPs Livelihood System",
    subtitle = NULL,
  ) +
  theme(
    plot.title = element_text(size = 12, margin = margin(b = 10)),
    axis.title.y = element_text(size = 10, margin = margin(r = 5)),
    axis.title.x = element_text(size = 10, margin = margin(r = 5))
  )

#### Plot the seasonal component over years ----
seasonal_cmpnt_urban_idps <- cmpnts_urban_idps |> 
  select(season_year) |> 
  gg_season(y = season_year) + 
  labs(
    title = "Seasonal Patterns Over Time in Urban/IDPs Systems",
    subtitle = "The highest peak of admissions is reached in June every year",
    y = "Seasonal effects"
  ) +
  theme(
    plot.subtitle = element_text(colour = "#706E6D"),
    axis.title.y = element_text(size = 10, margin = margin(r = 5)),
    axis.title.x = element_text(size = 10, margin = margin(r = 5))
  )

#### Subset the seasonal component before 2022 ----
seasonal_cmpnt_urban_idps_b2022 <- cmpnts_urban_idps |> 
  filter(year(Monthly) < 2022) |> 
  select(season_year) |> 
  gg_season(y = season_year) + 
  labs(
    title = "Seasonal Patterns in Urban/IDPs Livelihood Systems",
    subtitle = "Seasonal patterns shifted as of 2022",
    y = "Seasonal effects"
  ) +
  theme(
    plot.subtitle = element_text(colour = "#706E6D"),
    plot.caption = element_text(colour = '#706E6D'), 
    axis.title.y = element_text(size = 10, margin = margin(r = 5)),
    axis.title.x = element_text(size = 10, margin = margin(r = 5))
  )

#### Subset the seasonal component as of 2022 ----
seasonal_cmpnt_urban_idps_a2022 <- cmpnts_urban_idps |> 
  filter(year(Monthly) >= 2022) |> 
  select(season_year) |> 
  gg_season(y = season_year) + 
  labs(
    title = "Seasonal Patterns in Urban/IDPs Livelihood Systems",
    subtitle = "Seasonal patterns shifted as of 2022",
    y = "Seasonal effects"
  ) +
  theme(
    plot.subtitle = element_text(colour = "#706E6D"),
    plot.caption = element_text(colour = '#706E6D'), 
    axis.title.y = element_text(size = 10, margin = margin(r = 5)),
    axis.title.x = element_text(size = 10, margin = margin(r = 5))
  )

################################### End ########################################