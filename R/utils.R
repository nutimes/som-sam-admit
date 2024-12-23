
manipulate_tsibble <- function(ts, .by = c("grouped", "ungrouped")) {
 .by = match.arg(.by)
  
  ## Grouped time series ----
  if (.by == "grouped") {
    ts <- ts |> 
    select(region, monthly, admissions) |> 
      summarise(
        admissions = sum(admissions, na.rm = TRUE),
        .by = c(region, monthly)
      ) |> 
      as_tsibble(
        index = monthly,
        key = region
      )
  }

  ## Ungrouped time series ----
  if (.by == "ungrouped") {
    ts <- ts |> 
      select(region, monthly, admissions) |> 
        summarise(
          admissions = sum(admissions, na.rm = TRUE),
          .by = monthly
        ) |> 
        as_tsibble(
          index = monthly
        )
  }

  ## Return ----
  ts
}
