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
  autoplot(box_cox(admissions, lambda = lambda_national))


## ---- Decomposition ----------------------------------------------------------
cmpnts_national <- na |> 
  model(
    STL(box_cox(x = admissions, lambda = lambda_national) ~ trend(window = 9) + season(window = 7))
  ) |> 
  components()

### Visualize the components ----
cmpnts_plot_national <- cmpnts_national |> 
  autoplot()

### Plot the seasonal component over years ----
seasonal_cmpnt_national <- cmpnts_national |> 
  select(season_year) |> 
  gg_season(y = season_year)


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
  autoplot(box_cox(admissions, lambda = lambda_pastoral))

### Agropastoral ----
mo |> 
  filter(lsystems == "Agropastoral") |> 
  autoplot(box_cox(admissions, lambda = lambda_agropastoral))

### Riverine ----
mo |> 
  filter(lsystems == "Riverine") |> 
  autoplot(box_cox(admissions, lambda = lambda_rivernine))

### Urban/IDPs ----
mo |> 
  filter(lsystems == "Urban/IDPs") |> 
  autoplot(box_cox(admissions, lambda = lambda_urban_idps))


## ---- Decomposition ----------------------------------------------------------

### Pastoral ----
cmpnts_pastoral <- mo |> 
  filter(lsystems == "Pastoral") |> 
  model(
    STL(box_cox(admissions, lambda_pastoral) ~ trend(window = 9) + season(window = 7))
  ) |> 
  components()

### Visualize the components ----
cmpnts_plot_pastoral <- cmpnts_pastoral |> 
  autoplot()

### Plot the seasonal component over years ----
seasonal_cmpnt_pastoral <- cmpnts_pastoral |> 
  select(season_year) |> 
  gg_season(y = season_year)

### Agropastoral ----
cmpnts_agropastoral <- mo |> 
  filter(lsystems == "Agropastoral") |> 
  model(
    STL(box_cox(admissions, lambda_agropastoral) ~ trend(window = 9) + season(window = 7))
  ) |> 
  components()

### Visualize the components ----
cmpnts_plot_agropastoral <- autoplot(cmpnts_agropastoral)

### Plot the seasonal component over years ----
seasonal_cmpnt_agropastoral <- cmpnts_agropastoral |> 
  select(season_year) |> 
  gg_season(y = season_year)

### Riverine ----
cmpnts_riverine <- mo |> 
  filter(lsystems == "Riverine") |> 
  model(
    STL(box_cox(admissions, lambda_rivernine) ~ trend(window = 9) + season(window = 7))
  ) |> 
  components()

### Visualize the components ----
cmpnts_plot_riverine <- autoplot(cmpnts_riverine)

### Plot the seasonal component over years ----
seasonal_cmpnt_riverine <- cmpnts_riverine |> 
  select(season_year) |> 
  gg_season(y = season_year)

### Urban/IDPs ----
cmpnts_urban_idps <- mo |> 
  filter(lsystems == "Urban/IDPs") |> 
  model(
    STL(box_cox(admissions, lambda_urban_idps) ~ trend(window = 9) + season(window = 7))
  ) |> 
  components()

### Visualize the components ----
cmpnts_plot_urban_idps <- autoplot(cmpnts_urban_idps)

### Plot the seasonal component over years ----
seasonal_cmpnt_urban_idps <- cmpnts_urban_idps |> 
  select(season_year) |> 
  gg_season(y = season_year)

################################### End ########################################