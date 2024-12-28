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
