# ---- Average Rate of Change --------------------------------------------------

################################# NATIONAL #####################################

### Reverse Box-Cox transformation done in the `decomposition.R` file ----
trend_national <- cmpnts_national |> 
  select(trend) |> 
  mutate(trend = inv_box_cox(x = trend, lambda = lambda_national))

## ---------------  Piecewise trend: 2019 Jan-2019 Jun; variation: increase ----

### Plot the slope ----
slope_beforeJul2019 <- trend_national |> 
  filter(Monthly <= yearmonth("2019 Jun")) |> 
  autoplot()

### ARC ----
arc_beforeJul2019 <- trend_national |> 
  ARC(
    start = yearmonth("2019 Jan"),
    end = yearmonth("2019 Jun"),
    .for = "knots"
  )

## --------------------- Piecewise trend: Aug-Oct 2019; variation: decrease ----

### Plot the slope ----
slope_aug2019_oct2019 <- trend_national |> 
  filter(Monthly >= yearmonth("2019 Aug") & Monthly <= yearmonth("2019 Oct")) |> 
  autoplot()

### ARC ----
arc_aug2019_oct2019 <- trend_national |> 
  ARC(
    start = yearmonth("2019 Aug"),
    end = yearmonth("2019 Oct"),
    .for = "knots"
  )

## --------------------- Piecewise trend: Nov 2019-Feb 2020; variation: decrease ----

### Plot the slope ----
slope_nov2019_feb2020 <- trend_national |> 
  filter(Monthly >= yearmonth("2019 Nov") & Monthly <= yearmonth("2020 Feb")) |> 
  autoplot()

### ARC ----
arc_nov2019_feb2020 <- trend_national |> 
  ARC(
    start = yearmonth("2019 Nov"),
    end = yearmonth("2020 Feb"),
    .for = "knots"
  )

## --------------------- Piecewise trend: Mar 2020-Jan 2021; variation: decrease ----

### Plot the slope ----
slope_mar2020_jan2021 <- trend_national |> 
  filter(Monthly >= yearmonth("2020 Mar") & Monthly <= yearmonth("2021 Jan")) |> 
  autoplot()

### ARC ----
arc_mar2020_jan2021 <- trend_national |> 
  ARC(
    start = yearmonth("2020 Mar"),
    end = yearmonth("2021 Jan"),
    .for = "knots"
  )


## ---------------- Piecewise trend: Feb 2021-Mar 2023; variation: increase ----

### Plot the slope ----
slope_feb2021_mar2023 <- trend_national |> 
  filter(Monthly >= yearmonth("2021 Feb") & Monthly <= yearmonth("2023 Mar")) |> 
  autoplot()

### ARC ----
arc_feb2021_mar2023 <- trend_national |> 
  ARC(
    start = yearmonth("2021 Feb"),
    end = yearmonth("2023 Mar"),
    .for = "knots"
  )


## ---------------- Piecewise trend: Apr 2023-Dec 2024; variation: decrease ----

### Plot the slope ----
slope_april2023_dec2024 <- trend_national |> 
  filter(Monthly >= yearmonth("2023 Apr")) |> 
  autoplot()

### ARC ----
arc_april2023_dec2024 <- trend_national |> 
  ARC(
    start = yearmonth("2023 Apr"),
    end = yearmonth("2024 Dec"),
    .for = "knots"
  )


################################### End ########################################