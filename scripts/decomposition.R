# ---- Decomposition -----------------------------------------------------------

############################# MONTHLY ANALYSIS  ################################

################################################################################
# As shown in the time, seasonal and subseries plot in the `eda-graphics.R` file,
# the data shows variations that increase and decrease with the level of the serie
# This necessitates the data to be transformed to stabelize the seasonal variation.
################################################################################

## Box-Cox transformation ----
x <- monthly_admissions |> 
  summarise_admissions(
    .group = TRUE,
    time = "M"
  )

### Get a lambda value of each livelihood system time series ----
#### For Pastoral ----
lambda_pastoral <- x |> 
  filter(lsystems == "Pastoral") |> 
  features(
    .var = admissions,
    features = guerrero
  ) |> 
  pull(lambda_guerrero)

#### For Agropastoral ----
lambda_agropastoral <- x |> 
  filter(lsystems == "Agropastoral") |> 
  features(
    .var = admissions,
    features = guerrero
  ) |> 
  pull(lambda_guerrero)

#### For Riverine ----
lambda_rivernine <- x |> 
  filter(lsystems == "Riverine") |> 
  features(
    .var = admissions,
    features = guerrero
  ) |> 
  pull(lambda_guerrero)

#### For Urban/IDP's ----
lambda_urban_idps <- x |> 
  filter(lsystems == "Urban/IDPs") |> 
  features(
    .var = admissions,
    features = guerrero
  ) |> 
  pull(lambda_guerrero)


## Visualize the time series after transformation ----
### Pastoral ----
x |> 
  filter(lsystems == "Pastoral") |> 
  autoplot(box_cox(admissions, lambda = lambda_pastoral))

### Agropastoral ----
x |> 
  filter(lsystems == "Agropastoral") |> 
  autoplot(box_cox(admissions, lambda = lambda_agropastoral))

### Riverine ----
x |> 
  filter(lsystems == "Riverine") |> 
  autoplot(box_cox(admissions, lambda = lambda_rivernine))

### Urban/IDPs ----
x |> 
  filter(lsystems == "Urban/IDPs") |> 
  autoplot(box_cox(admissions, lambda = lambda_urban_idps))

## Decompose with STL ----
### Pastoral ----
cmpnts_pastoral <- x |> 
  filter(lsystems == "Pastoral") |> 
  model(
    STL(box_cox(admissions, lambda_pastoral) ~ trend(window = 9) + season(window = 7))
  ) |> 
  components()

### Visualize the components ----
cmpnts_plot_pastoral <- cmpnts_pastoral |> 
  autoplot() +

### Seasonal component over years ----
seasonal_cmpnt_pastoral <- cmpnts_pastoral |> 
  select(season_year) |> 
  gg_season(y = season_year)

### Agropastoral ----
cmpnts_agropastoral <- x |> 
  filter(lsystems == "Agropastoral") |> 
  model(
    STL(box_cox(admissions, lambda_agropastoral) ~ trend(window = 9) + season(window = 7))
  ) |> 
  components()

### Visualize the components ----
cmpnts_plot_agropastoral <- autoplot(cmpnts_agropastoral)

### Seasonal component over years ----
seasonal_cmpnt_agropastoral <- cmpnts_agropastoral |> 
  select(season_year) |> 
  gg_season(y = season_year)

### Riverine ----
cmpnts_riverine <- x |> 
  filter(lsystems == "Riverine") |> 
  model(
    STL(box_cox(admissions, lambda_rivernine) ~ trend(window = 9) + season(window = 7))
  ) |> 
  components()

### Visualize the components ----
cmpnts_plot_riverine <- autoplot(cmpnts_riverine)

### Seasonal component over years ----
seasonal_cmpnt_riverine <- cmpnts_riverine |> 
  select(season_year) |> 
  gg_season(y = season_year)

### Urban/IDPs ----
cmpnts_urban_idps <- x |> 
  filter(lsystems == "Urban/IDPs") |> 
  model(
    STL(box_cox(admissions, lambda_urban_idps) ~ trend(window = 9) + season(window = 7))
  ) |> 
  components()

### Visualize the components ----
cmpnts_plot_urban_idps <- autoplot(cmpnts_urban_idps)

### Seasonal component over years ----
seasonal_cmpnt_urban_idps <- cmpnts_urban_idps |> 
  select(season_year) |> 
  gg_season(y = season_year)

################################### End ########################################