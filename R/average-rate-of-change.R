#'
#'
#'
#' Average rate of change (ARC) for a continuos time series
#'
#' The ARC (Average Rate of Change) quantifies the typical rate of change over
#' a specified time interval. It is calculated from the trend line and
#' represents the rate of difference between the average occurrence of a given
#' measurement at the end of the time series and its average occurrence at the start.
#' This difference is divided by the total duration of the interval.
#' The ARC is expressed in the same units as the original measurement scale,
#' providing an interpretable rate of change.
#'
#' @param ts A time series object of class `tsibble`.
#'
#' @param start A basis of time that represents the start of the whole series or
#'  a point of piece of the series for which the ARC would be calculated for.
#'
#' @param end A basis of time that represents the end point of the whole series
#' or just a piece of the series.
#'
#' @param .for An indication on whether the ARC should be calculated for the
#' entire time series (`"whole_ts`) or for a piece of it (`"knots"`).
#'
#' @returns A list of ouputs that are useful to build a narrative on the findigs.
#'
#' @references
#' Kelley, K. The average rate of change for continuous time models.
#' *Behavior Research Methods* 41, 268–278 (2009). https://doi.org/10.3758/BRM.41.2.268
#'
#'
#'
#'

ARC <- function(ts,
                start = NULL,
                end = NULL,
                .for = c("whole_ts", "knots")) {
  # Enforce options in `.for` ----
  .for <- match.arg(.for)

  switch(.for,
    "whole_ts" = {
      ## Calculate the ARC ----
      trend <- ts$trend
      average1 <- trend[1]
      averageT <- trend[length(trend)]
      time_interval <- end - start
      arc <- (averageT - average1) / time_interval

      ## Return a list of ouputs ----
      list(
        average_trend1 = average1,
        start = start,
        average_trendT = averageT,
        end = end,
        total_time_interval = time_interval,
        ARC = arc
      )
    },
    "knots" = {
      ## Filter out the time series based on the given time interval ----
      x <- ts |>
        filter(Monthly >= start & Monthly <= end)

      ## Pull the average admission cases at the start of the time interval ----
      average1 <- x |>
        slice(1) |>
        pull(trend)

      ## Pull the average admission cases at the end of the time interval ----
      averageT <- x |>
        slice(n()) |>
        pull(trend)

      ## Get the number of time interval ----
      time_interval <- end - start

      ## Calculate the average rate of change ----
      arc <- (averageT - average1) / time_interval

      ## Return a list of ouputs ----
      list(
        average_trend1 = average1,
        start = start,
        average_trendT = averageT,
        end = end,
        total_time_interval = time_interval,
        ARC = arc
      )
    }
  )
}
