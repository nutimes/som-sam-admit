# ==============================================================================
# As shown in the time, seasonal and subseries plot in the `eda-graphics.R` file,
# the data shows variations that increase and decrease with the level of the serie
# This necessitates the data to be transformed to stabelize the seasonal variation.

#   Decomposition is done at national and then split into livelihood systems.
# ==============================================================================


## ---- Decomposition at National level ----------------------------------------

### Get components ----
cmpnts_national <- na |>
  model(
    STL(admissions ~ trend(window = 9) + season(window = 7))
  ) |>
  components() |> 
  mutate(
    admissions = inv_box_cox(x = admissions, lambda = lambda_national),
    trend = inv_box_cox(x = trend, lambda = lambda_national),
    season_year = inv_box_cox(x = season_year, lambda = lambda_national),
    remainder = inv_box_cox(x = remainder, lambda = lambda_national)
  ) 

### Visualize the components ----
cmpnts_plot_national <- cmpnts_national |>
  autoplot() +
  labs(
    title = "The Components of the SAM Admissions at National Level",
    subtitle = "Seasonal patterns shifted as of 2022"
  ) +
  theme(
    plot.subtitle = element_text(colour = "#706E6D"),
    axis.title.y = element_text(size = 10, margin = margin(r = 5)),
    axis.title.x = element_text(size = 10, margin = margin(r = 5))
  )

#### Plot the seasonal component over years ----
seasonal_cmpnt_national <- cmpnts_national |>
  select(season_year) |>
  gg_season(y = season_year) +
  labs(
    title = "Seasonal Patterns Over Time at National Level",
    subtitle = "The higher peak of admissions is reached in May-June every year, with a lower nadir in April",
    y = "Seasonal effects",
    colour = "Year"
  ) +
  theme(
    plot.subtitle = element_text(colour = "#706E6D"),
    plot.caption = element_text(colour = "#706E6D"),
    axis.title.y = element_text(size = 10, margin = margin(r = 5)),
    axis.title.x = element_text(size = 10, margin = margin(t = 5))
  )

##### Subset the seasonal component before 2022 ----
seasonal_cmpnt_national_b2022 <- cmpnts_national |>
  filter(year(Monthly) < 2022) |>
  select(season_year) |>
  gg_season(y = season_year) +
  labs(
    title = "Seasonal Patterns Before 2022",
    subtitle = "The higher peak of admissions is reached in May-June every year, with a lower nadir in April",
    y = "Seasonal effects",
    colour = "Year"
  ) +
  theme(
    plot.subtitle = element_text(colour = "#706E6D"),
    plot.caption = element_text(colour = "#706E6D"),
    axis.title.y = element_text(size = 10, margin = margin(r = 5)),
    axis.title.x = element_text(size = 10, margin = margin(t = 5))
  )

##### Subset the seasonal component as of 2022 ----
seasonal_cmpnt_national_a2022 <- cmpnts_national |>
  filter(year(Monthly) >= 2022) |>
  select(season_year) |>
  gg_season(y = season_year) +
  labs(
    title = "Seasonal Patterns As Of 2022",
    subtitle = "Two peaks: May-June and then Nov-Jan, and lower nadir in April",
    y = "Seasonal effects",
    colour = "Year"
  ) +
  theme(
    plot.subtitle = element_text(colour = "#706E6D"),
    plot.caption = element_text(colour = "#706E6D"),
    axis.title.y = element_text(size = 10, margin = margin(r = 5)),
    axis.title.x = element_text(size = 10, margin = margin(t = 5))
  )


## ---- Decomposition by Livelihood systems ------------------------------------

## ---- Pastoral livelihood system ---------------------------------------------

### Get components ----
cmpnts_pastoral <- ls |>
  filter(lsystems == "Pastoral") |>
  model(
    STL(admissions ~ trend(window = 9) + season(window = 7))
  ) |>
  components() |> 
  mutate(
    admissions = inv_box_cox(x = admissions, lambda = lambda_pastoral),
    trend = inv_box_cox(x = trend, lambda = lambda_pastoral),
    season_year = inv_box_cox(x = season_year, lambda = lambda_pastoral),
    remainder = inv_box_cox(x = remainder, lambda = lambda_pastoral)
  ) 

### Visualize the components ----
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

### Plot the seasonal component over years ----
seasonal_cmpnt_pastoral <- cmpnts_pastoral |>
  select(season_year) |>
  gg_season(y = season_year) +
  labs(
    title = "Seasonal Patterns in Pastoral Livelihood Systems",
    subtitle = "Seasonal patterns changed as of 2022, with low amplitude of the peaks compared to before 2022",
    y = "Seasonal effects",
    colour = "Year"
  ) +
  theme(
    plot.subtitle = element_text(colour = "#706E6D"),
    plot.caption = element_text(colour = "#706E6D"),
    axis.title.y = element_text(size = 10, margin = margin(r = 5)),
    axis.title.x = element_text(size = 10, margin = margin(r = 5))
  )

#### Subset the seasonal component before 2022 ----
seasonal_cmpnt_pastoral_b2022 <- cmpnts_pastoral |>
  filter(year(Monthly) < 2022) |>
  select(season_year) |>
  gg_season(y = season_year) +
  labs(
    title = "Seasonal Patterns in Pastoral Livelihood Systems",
    subtitle = "Admissions rose remarkably from April, with a high peak in June",
    y = "Seasonal effects",
    colour = "Year"
  ) +
  theme(
    plot.subtitle = element_text(colour = "#706E6D"),
    plot.caption = element_text(colour = "#706E6D"),
    axis.title.y = element_text(size = 10, margin = margin(r = 5)),
    axis.title.x = element_text(size = 10, margin = margin(r = 5))
  )

#### Subset the seasonal component as of 2022 ----
seasonal_cmpnt_pastoral_a2022 <- cmpnts_pastoral |>
  filter(year(Monthly) >= 2022) |>
  select(season_year) |>
  gg_season(y = season_year) +
  labs(
    title = "Seasonal Patterns in Pastoral Livelihood Systems",
    subtitle = "Seasonal patterns shifted as of 2022",
    y = "Seasonal effects",
    colour = "Year"
  ) +
  theme(
    plot.subtitle = element_text(colour = "#706E6D"),
    plot.caption = element_text(colour = "#706E6D"),
    axis.title.y = element_text(size = 10, margin = margin(r = 5)),
    axis.title.x = element_text(size = 10, margin = margin(r = 5))
  )

## ---- Agropastoral livelihood system -----------------------------------------

### Get components ----
cmpnts_agropastoral <- ls |>
  filter(lsystems == "Agropastoral") |>
  model(
    STL(admissions ~ trend(window = 9) + season(window = 7))
  ) |>
  components() |> 
  mutate(
    admissions = do.call(
      what = inv_box_cox,
      args = list(x = admissions, lambda = lambda_agropastoral)
    ),
    trend = do.call(
      what = inv_box_cox,
      args = list(x = trend, lambda = lambda_agropastoral)
    ), 
    season_year = do.call(
      what = inv_box_cox,
      args = list(x = season_year, lambda = lambda_agropastoral)
    ),
    remainder = do.call(
      what = inv_box_cox,
      args = list(x = remainder, lambda = lambda_agropastoral)
    )
  )

### Visualize the components ----
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

### Plot the seasonal component over years ----
seasonal_cmpnt_agropastoral <- cmpnts_agropastoral |>
  select(season_year) |>
  gg_season(y = season_year) +
  labs(
    title = "Seasonal Patterns in Agropastoral Livelihood Systems",
    subtitle = "The highest peak of admissions is reached in June every year",
    y = "Seasonal effects",
    colour = "Year"
  ) +
  theme(
    plot.subtitle = element_text(colour = "#706E6D"),
    axis.title.y = element_text(size = 10, margin = margin(r = 5)),
    axis.title.x = element_text(size = 10, margin = margin(r = 5))
  )

### Subset the seasonal component before 2022 ----
seasonal_cmpnt_agropastoral_b2022 <- cmpnts_agropastoral |>
  filter(year(Monthly) < 2022) |>
  select(season_year) |>
  gg_season(y = season_year) +
  labs(
    title = "Seasonal Patterns in Agropastoral Livelihood Systems",
    subtitle = "One  higher peak in May-June, and lower nadirs between February and April",
    y = "Seasonal effects",
    colour = "Year"
  ) +
  theme(
    plot.subtitle = element_text(colour = "#706E6D"),
    plot.caption = element_text(colour = "#706E6D"),
    axis.title.y = element_text(size = 10, margin = margin(r = 5)),
    axis.title.x = element_text(size = 10, margin = margin(r = 5))
  )

### Subset the seasonal component as of 2022 ----
seasonal_cmpnt_agropastoral_a2022 <- cmpnts_agropastoral |>
  filter(year(Monthly) >= 2022) |>
  select(season_year) |>
  gg_season(y = season_year) +
  labs(
    title = "Seasonal Patterns in Agropaastoral Livelihood Systems as of 2022",
    subtitle = "Two high peaks in January and June, and one lower nadir in April",
    y = "Seasonal effects"
  ) +
  theme(
    plot.subtitle = element_text(colour = "#706E6D"),
    plot.caption = element_text(colour = "#706E6D"),
    axis.title.y = element_text(size = 10, margin = margin(r = 5)),
    axis.title.x = element_text(size = 10, margin = margin(r = 5))
  )

## ---- Riverine livelihood system ---------------------------------------------

### Get components ----
cmpnts_riverine <- ls |>
  filter(lsystems == "Riverine") |>
  model(
    STL(admissions ~ trend(window = 9) + season(window = 7))
  ) |>
  components() |> 
  mutate(
    admissions = do.call(
      what = inv_box_cox,
      args = list(x = admissions, lambda = lambda_riverine)
    ),
    trend = do.call(
      what = inv_box_cox,
      args = list(x = trend, lambda = lambda_riverine)
    ), 
    season_year = do.call(
      what = inv_box_cox,
      args = list(x = season_year, lambda = lambda_riverine)
    ),
    remainder = do.call(
      what = inv_box_cox,
      args = list(x = remainder, lambda = lambda_riverine)
    )
  )

### Visualize the components ----
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

### Plot the seasonal component over years ----
seasonal_cmpnt_riverine <- cmpnts_riverine |>
  select(season_year) |>
  gg_season(y = season_year) +
  labs(
    title = "Seasonal Patterns in Riverine Livelihood Systems",
    subtitle = "Several high peaks in admissions",
    y = "Seasonal effects",
    colours = "Year"
  ) +
  theme(
    plot.subtitle = element_text(colour = "#706E6D"),
    axis.title.y = element_text(size = 10, margin = margin(r = 5)),
    axis.title.x = element_text(size = 10, margin = margin(r = 5))
  )

#### Subset the seasonal component before 2022 ----
seasonal_cmpnt_riverine_b2022 <- cmpnts_riverine |>
  filter(year(Monthly) < 2022) |>
  select(season_year) |>
  gg_season(y = season_year) +
  labs(
    title = "Seasonal Patterns in Riverine Livelihood Systems Before 2022",
    subtitle = "Two higher peaks in May-June, then December-January; and two lower nadirs in April and in November",
    y = "Seasonal effects",
    colour = "Year"
  ) +
  theme(
    plot.subtitle = element_text(colour = "#706E6D"),
    plot.caption = element_text(colour = "#706E6D"),
    axis.title.y = element_text(size = 10, margin = margin(r = 5)),
    axis.title.x = element_text(size = 10, margin = margin(r = 5))
  )

#### Subset the seasonal component as of 2022 ----
seasonal_cmpnt_riverine_a2022 <- cmpnts_riverine |>
  filter(year(Monthly) >= 2022) |>
  select(season_year) |>
  gg_season(y = season_year) +
  labs(
    title = "Seasonal Patterns in Riverine Livelihood Systems",
    subtitle = "Several peaks and lower nadir in November",
    y = "Seasonal effects",
    colour = "Year"
  ) +
  theme(
    plot.subtitle = element_text(colour = "#706E6D"),
    plot.caption = element_text(colour = "#706E6D"),
    axis.title.y = element_text(size = 10, margin = margin(r = 5)),
    axis.title.x = element_text(size = 10, margin = margin(r = 5))
  )

## ---- Urban/IDPs livelihood system -------------------------------------------

### Get component ----
cmpnts_urban_idps <- ls |>
  filter(lsystems == "Urban/IDPs") |>
  model(
    STL(admissions ~ trend(window = 9) + season(window = 7))
  ) |>
  components() |> 
  mutate(
    admissions = do.call(
      what = inv_box_cox,
      args = list(x = admissions, lambda = lambda_urban_idps)
    ),
    trend = do.call(
      what = inv_box_cox,
      args = list(x = trend, lambda = lambda_urban_idps)
    ), 
    season_year = do.call(
      what = inv_box_cox,
      args = list(x = season_year, lambda = lambda_urban_idps)
    ),
    remainder = do.call(
      what = inv_box_cox,
      args = list(x = remainder, lambda = lambda_urban_idps)
    )
  )

### Visualize the components ----
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

### Plot the seasonal component over years ----
seasonal_cmpnt_urban_idps <- cmpnts_urban_idps |>
  select(season_year) |>
  gg_season(y = season_year) +
  labs(
    title = "Seasonal Patterns Over Time in Urban/IDPs Systems",
    subtitle = "The higher peak of admissions is reached in May-June, and lower nadir in April",
    y = "Seasonal effects",
    colour = "Year"
  ) +
  theme(
    plot.subtitle = element_text(colour = "#706E6D"),
    axis.title.y = element_text(size = 10, margin = margin(r = 5)),
    axis.title.x = element_text(size = 10, margin = margin(r = 5))
  )

### Subset the seasonal component before 2022 ----
seasonal_cmpnt_urban_idps_b2022 <- cmpnts_urban_idps |>
  filter(year(Monthly) < 2022) |>
  select(season_year) |>
  gg_season(y = season_year) +
  labs(
    title = "Seasonal Patterns in Urban/IDPs Livelihood Systems Before 2022",
    subtitle = "The higher peak of admissions is reached in May-June, and lower nadir in April",
    y = "Seasonal effects",
    colour = "Year"
  ) +
  theme(
    plot.subtitle = element_text(colour = "#706E6D"),
    plot.caption = element_text(colour = "#706E6D"),
    axis.title.y = element_text(size = 10, margin = margin(r = 5)),
    axis.title.x = element_text(size = 10, margin = margin(r = 5))
  )

### Subset the seasonal component as of 2022 ----
seasonal_cmpnt_urban_idps_a2022 <- cmpnts_urban_idps |>
  filter(year(Monthly) >= 2022) |>
  select(season_year) |>
  gg_season(y = season_year) +
  labs(
    title = "Seasonal Patterns in Urban/IDPs Livelihood Systems as of 2022",
    subtitle = "The higher peak of admissions is reached in May-June, and lower nadir in April",
    y = "Seasonal effects",
    colour = "Year"
  ) +
  theme(
    plot.subtitle = element_text(colour = "#706E6D"),
    plot.caption = element_text(colour = "#706E6D"),
    axis.title.y = element_text(size = 10, margin = margin(r = 5)),
    axis.title.x = element_text(size = 10, margin = margin(r = 5))
  )

# ============================  End of Workflow ================================