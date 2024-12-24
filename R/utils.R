
manipulate_tsibble <- function(ts, .by = c("grouped", "ungrouped")) {
 .by = match.arg(.by)
  
  ## Grouped time series ----
  if (.by == "grouped") {
    ts <- ts |> 
    select(region, Monthly, admissions) |> 
      group_by(region) |> 
      summarise(admissions = sum(admissions, na.rm = TRUE))
  }

  ## Ungrouped time series ----
  if (.by == "ungrouped") {
    ts <- ts |> 
      summarise(admissions = sum(admissions, na.rm = TRUE))
  }
  ## Return ----
  ts
}
