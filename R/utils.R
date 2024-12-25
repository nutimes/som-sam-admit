
summarise_admissions <- function(ts, 
                                .group = TRUE,
                                time = c("M", "Q")) { 
  
  ## Enforce options in `time` ----
  time <- match.arg(time)

  ## Grouped time series ----
  if (.group) {
    if (time == "M") {
      ts <- ts |> 
        select(region, Monthly, admissions) |> 
        group_by(region) |> 
        summarise(
          admissions = sum(admissions, na.rm = TRUE), 
          .groups = "drop"
        ) |> 
          as_tsibble(
            index = Monthly, 
            key = region
          )
    }
     if (time == "Q") {
      ts <- ts |> 
        select(region, Quarterly, admissions) |> 
        group_by(region) |> 
        summarise(
          admissions = sum(admissions, na.rm = TRUE),
          .groups = "drop"
        ) |> 
        as_tsibble(
          key = region, 
          index = Quarterly
        )
    } 
  } else {
    if (time == "M") {
      ts <- ts |> 
        select(Monthly, admissions) |> 
        summarise(
          admissions = sum(admissions, na.rm = TRUE)
        ) |> 
        as_tsibble(
          index = Monthly
        )
    } 
    
    if (time == "Q") {
      ts <- ts |> 
        select(Quarterly, admissions) |> 
        summarise(
          admissions = sum(admissions, na.rm = TRUE)
        ) |> 
        as_tsibble(
          index = Quarterly
        )
    }
  }
## Return ----
ts
}
