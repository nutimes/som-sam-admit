#'
#'
#' Summarise time series data
#'
#' @param ts A time series object of class `tsibble`.
#'
#' @param .group Logical. Whether the `tsibble` should be grouped or not, as
#'    it would be required in subsequent analysis.
#'
#' @param time A choice of the time series interval of occurrence.
#' `"M"` for monthly and `"Q"` for quarterly.
#'
#'

summarise_admissions <- function(
    ts,
    .group = TRUE,
    time = c("M", "Q")) {
  ## Enforce options in `time` ----
  time <- match.arg(time)

  ## Grouped time series ----
  if (.group) {
    if (time == "M") {
      ts <- ts |>
        select(lsystems, Monthly, admissions) |>
        group_by(lsystems, Monthly) |>
        summarise(
          admissions = sum(admissions, na.rm = TRUE),
          .groups = "drop"
        ) |>
        as_tsibble(
          index = Monthly,
          key = lsystems
        )
    }
    if (time == "Q") {
      ts <- ts |>
        select(lsystems, Quarterly, admissions) |>
        group_by(lsystems, Quarterly) |>
        summarise(
          admissions = sum(admissions, na.rm = TRUE),
          .groups = "drop"
        ) |>
        as_tsibble(
          key = lsystems,
          index = Quarterly
        )
    }
  } else {
    if (time == "M") {
      ts <- ts |>
        select(Monthly, admissions) |>
        group_by(Monthly) |>
        summarise(
          admissions = sum(admissions, na.rm = TRUE),
          .groups = "drop"
        ) |>
        as_tsibble(
          index = Monthly
        )
    }

    if (time == "Q") {
      ts <- ts |>
        select(Quarterly, admissions) |>
        group_by(Quarterly) |>
        summarise(
          admissions = sum(admissions, na.rm = TRUE),
          .groups = "drop"
        ) |>
        as_tsibble(
          index = Quarterly
        )
    }
  }
  ## Return ----
  ts
}
