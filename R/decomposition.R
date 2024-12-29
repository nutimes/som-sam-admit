# ---- Decomposition -----------------------------------------------------------

########################### QUARTERLY ANALYSIS #############################

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


############################# MONTHLY ANALYSIS #############################

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
