
summarise_admissions <- function(ts, .group = TRUE) {
  .group = match.arg(.group)
  
  ## Grouped time series ----
  if (.group) {
    ts <- ts |> 
    select(region, Monthly, admissions) |> 
      group_by(region) |> 
      summarise(admissions = sum(admissions, na.rm = TRUE))
  } else {
      ts <- ts |> 
        summarise(admissions = sum(admissions, na.rm = TRUE))
    }
  ## Return ----
  ts
}
