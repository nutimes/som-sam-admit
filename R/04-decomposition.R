# ---- Decomposition -----------------------------------------------------------

########################### QUARTERLY ANALYSIS #############################
# Data shows variations that increase and decrease with the level of the serie
# Apply transformation before decomposition

## Box-Cox transformation ----
df <- som_admissions_quarterly |> 
  summarise_admissions(
    .group = FALSE,
    time = "Q"
  ) 

  lambda <- df |> 
    features(
    .var = admissions, 
    features = guerrero
  ) |> 
  pull(lambda_guerrero)

## Visualize the time series after transformation ----
df |> 
  autoplot(box_cox(admissions, lambda = lambda))

## Decompose with STL ----
### Formula ----
formula <- box_cox(admissions, lambda) ~ trend(window = 5) + season(window = 8)

components <- df |> 
  model(STL(formula)) |> 
  components()

### Visualize the components ----
autoplot(components)

### Seasonal component over years ----
components |> 
  select(season_year) |> 
  gg_season()


############################# MONTHLY ANALYSIS #############################

## Box-Cox transformation ----
df_mo <- som_admissions_monthly |> 
  summarise_admissions(
    .group = FALSE,
    time = "M"
  ) 

  lambda <- df_mo |> 
    features(
    .var = admissions, 
    features = guerrero
  ) |> 
  pull(lambda_guerrero)

## Visualize the time series after transformation ----
df_mo |> 
  autoplot(box_cox(admissions, lambda = lambda))

## Decompose with STL ----
### Formula ----
formula <- box_cox(admissions, lambda) ~ trend(window = 8) + season(window = 6)
components_mo <- df_mo |> 
  model(STL(formula)) |> 
  components()

### Visualize the components ----
autoplot(components_mo)

### Seasonal component over years ----
components_mo |> 
  select(season_year) |> 
  gg_season()
