################################################################################
#                          AVERAGE RATE OF CHANGE                              #
################################################################################


# ---- National ----------------------------------------------------------------

### Reverse Box-Cox transformation done in the `decomposition.R` file ----
trend_national <- cmpnts_national |> 
  select(trend) |> 
  mutate(trend = inv_box_cox(x = trend, lambda = lambda_national))

## ----------------------------------- Piecewise trend; direction: increase ----

### Slope ----
slope_beforeJul2019 <- trend_national |> 
  filter(Monthly <= yearmonth("2019 Jun")) |> 
  autoplot(.vars = trend)

### ARC ----
arc_beforeJul2019 <- trend_national |> 
  ARC(
    start = yearmonth("2019 Jan"),
    end = yearmonth("2019 Jun"),
    .for = "knots"
  )

## ----------------------------------- Piecewise trend; direction: decrease ----

### Slope ----
slope_aug2019_oct2019 <- trend_national |> 
  filter(Monthly >= yearmonth("2019 Aug") & Monthly <= yearmonth("2019 Oct")) |> 
  autoplot(.vars = trend)

### ARC ----
arc_aug2019_oct2019 <- trend_national |> 
  ARC(
    start = yearmonth("2019 Aug"),
    end = yearmonth("2019 Oct"),
    .for = "knots"
  )

## ----------------------------------- Piecewise trend; direction: decrease ---- 

### Slope ----
slope_nov2019_feb2020 <- trend_national |> 
  filter(Monthly >= yearmonth("2019 Nov") & Monthly <= yearmonth("2020 Feb")) |> 
  autoplot(.vars = trend)

### ARC ----
arc_nov2019_feb2020 <- trend_national |> 
  ARC(
    start = yearmonth("2019 Nov"),
    end = yearmonth("2020 Feb"),
    .for = "knots"
  )

## ----------------------------------- Piecewise trend; direction: decrease ----

### Slope ----
slope_mar2020_jan2021 <- trend_national |> 
  filter(Monthly >= yearmonth("2020 Mar") & Monthly <= yearmonth("2021 Jan")) |> 
  autoplot(.vars = trend)

### ARC ----
arc_mar2020_jan2021 <- trend_national |> 
  ARC(
    start = yearmonth("2020 Mar"),
    end = yearmonth("2021 Jan"),
    .for = "knots"
  )


## ----------------------------------- Piecewise trend; direction: increase ----

### Slope ----
slope_feb2021_mar2023 <- trend_national |> 
  filter(Monthly >= yearmonth("2021 Feb") & Monthly <= yearmonth("2023 Mar")) |> 
  autoplot(.vars = trend)

### ARC ----
arc_feb2021_mar2023 <- trend_national |> 
  ARC(
    start = yearmonth("2021 Feb"),
    end = yearmonth("2023 Mar"),
    .for = "knots"
  )


## ----------------------------------- Piecewise trend; direction: decrease ----

### Slope ----
slope_april2023_dec2024 <- trend_national |> 
  filter(Monthly >= yearmonth("2023 Apr")) |> 
  autoplot(.vars = trend)

### ARC ----
arc_april2023_dec2024 <- trend_national |> 
  ARC(
    start = yearmonth("2023 Apr"),
    end = yearmonth("2024 Dec"),
    .for = "knots"
  )


# ---- By Livelihood systems ---------------------------------------------------

## -------------------------------------------- PASTORAL LIVELIHOOD SYSTEM -----
trend_pastoral <- cmpnts_pastoral |> 
  select(trend) |> 
  mutate(trend = inv_box_cox(x = trend, lambda = lambda_pastoral))

## ---- Piecewise trend; direction: increase -----------------------------------

### Slope ----
slope_pastoral_beforemay2019 <- trend_pastoral |> 
  filter(Monthly <= yearmonth("2019 May")) |> 
  autoplot(.vars = trend)

### ARC ----
arc_pastoral_beforemay2019 <- ARC(
  ts = trend_pastoral,
  start = yearmonth("2019 Jan"),
  end = yearmonth("2019 May"), 
  .for = "knots"
)

## ---- Piecewise trend; direction: decrease -----------------------------------

### Slope ----
slope_pastoral_jun_sep2019 <- trend_pastoral |> 
  filter(Monthly >= yearmonth("2019 Jun") & Monthly <= yearmonth("2019 Sep")) |> 
  autoplot(.vars = trend)

### ARC ----
arc_pastoral_jun_sep2019 <- ARC(
  ts = trend_pastoral,
  start = yearmonth("2019 Jun"),
  end = yearmonth("2019 Sep"), 
  .for = "knots"
)

## ---- Piecewise trend; direction: increase -----------------------------------

### Slope ----
slope_pastoral_oct2019_aug2020 <- trend_pastoral |> 
  filter(Monthly >= yearmonth("2019 Oct") & Monthly <= yearmonth("2020 Aug")) |> 
  autoplot(.vars = trend)

### ARC ----
arc_pastoral_oct2019_aug2020 <- ARC(
  ts = trend_pastoral,
  start = yearmonth("2019 Sep"),
  end = yearmonth("2020 Aug"), 
  .for = "knots"
)

## ---- Piecewise trend; direction: decrease -----------------------------------

### Slope ----
slope_pastoral_sep2020_apr2021 <- trend_pastoral |> 
  filter(Monthly >= yearmonth("2020 Sep") & Monthly <= yearmonth("2021 Apr")) |> 
  autoplot(.vars = trend)

### ARC ----
arc_pastoral_sep2020_apr2021 <- ARC(
  ts = trend_pastoral,
  start = yearmonth("2020 Sep"),
  end = yearmonth("2021 Apr"), 
  .for = "knots"
)

## ---- Piecewise trend; direction: increase -----------------------------------

### Slope ----
slope_pastoral_may2021_dec2022 <- trend_pastoral |> 
  filter(Monthly >= yearmonth("2021 May") & Monthly <= yearmonth("2022 Dec")) |> 
  autoplot(.vars = trend)

### ARC ----
arc_pastoral_may2021_dec2022 <- ARC(
  ts = trend_pastoral,
  start = yearmonth("2021 May"),
  end = yearmonth("2022 Dec"), 
  .for = "knots"
)

## ---- Piecewise trend; direction: decrease -----------------------------------

### Slope ----
slope_pastoral_jan2023_dec2024 <- trend_pastoral |> 
  filter(Monthly >= yearmonth("2023 Jan") & Monthly <= yearmonth("2024 Dec")) |> 
  autoplot(.vars = trend)

### ARC ----
arc_pastoral_jan2023_dec2024 <- ARC(
  ts = trend_pastoral,
  start = yearmonth("2023 Jan"),
  end = yearmonth("2024 Dec"), 
  .for = "knots"
)


### ---------------------------------------- AGROPASTORAL LIVELIHOOD SYSTEM ----

trend_agropastoral <- cmpnts_agropastoral |> 
  select(trend) |> 
  mutate(trend = inv_box_cox(x = trend, lambda = lambda_agropastoral)) 

## ---- Piecewise trend; direction: increase -----------------------------------

### Slope ----
slope_agropastoral_beforefeb2020 <- trend_agropastoral |> 
  filter(Monthly <= yearmonth("2020 Jan")) |> 
  autoplot(.vars = trend)

### ARC ----
arc_agropastoral_beforefeb2020 <- ARC(
  ts = trend_agropastoral,
  start = yearmonth("2019 Jan"),
  end = yearmonth("2020 Jan"), 
  .for = "knots"
)

## ---- Piecewise trend; direction: decrease -----------------------------------

### Slope ----
slope_agropastoral_feb2020_jan2021 <- trend_agropastoral |> 
  filter(Monthly >= yearmonth("2020 Feb") & Monthly <= yearmonth("2021 Jan")) |> 
  autoplot(.vars = trend)

### ARC ----
arc_agropastoral_feb2020_jan2021 <- ARC(
  ts = trend_agropastoral,
  start = yearmonth("2020 Jan"),
  end = yearmonth("2021 Jan"), 
  .for = "knots"
)

## ---- Piecewise trend; direction: increase -----------------------------------

### Slope ----
slope_agropastoral_feb2021_feb2023 <- trend_agropastoral |> 
  filter(Monthly >= yearmonth("2021 Feb") & Monthly <= yearmonth("2023 Feb")) |> 
  autoplot(.vars = trend)

### ARC ----
arc_agropastoral_feb2021_feb2023 <- ARC(
  ts = trend_agropastoral,
  start = yearmonth("2021 Feb"),
  end = yearmonth("2023 Feb"), 
  .for = "knots"
)

## ---- Piecewise trend; direction: decrease -----------------------------------

### Slope ----
slope_agropastoral_mar2023_dec2024 <- trend_agropastoral |> 
  filter(Monthly >= yearmonth("2023 Mar") & Monthly <= yearmonth("2024 Dec")) |> 
  autoplot(.vars = trend)

### ARC ----
arc_agropastoral_mar2023_dec2024 <- ARC(
  ts = trend_agropastoral,
  start = yearmonth("2023 Mar"),
  end = yearmonth("2024 Dec"), 
  .for = "knots"
)


### -------------------------------------------- RIVERINE LIVELIHOOD SYSTEM ----

trend_riverine <- cmpnts_riverine |> 
  select(trend) |> 
  mutate(trend = inv_box_cox(x = trend, lambda = lambda_riverine)) 

## ---- Piecewise trend; direction: increase -----------------------------------

### Slope ----
slope_riverine_beforemay2020 <- trend_riverine |> 
  filter(Monthly <= yearmonth("2020 May")) |> 
  autoplot(.vars = trend)

### ARC ----
arc_riverine_beforemay2020 <- ARC(
  ts = trend_riverine,
  start = yearmonth("2019 Jan"),
  end = yearmonth("2020 May"), 
  .for = "knots"
)

## ---- Piecewise trend; direction: decrease -----------------------------------

### Slope ----
slope_riverine_jun_nov2020 <- trend_riverine |> 
  filter(Monthly >= yearmonth("2020 Jun") & Monthly <= yearmonth("2020 Nov")) |> 
  autoplot(.vars = trend)

### ARC ----
arc_riverine_jun_nov2020 <- ARC(
  ts = trend_riverine,
  start = yearmonth("2020 Jun"),
  end = yearmonth("2020 Nov"), 
  .for = "knots"
)

## ---- Piecewise trend; direction: increase -----------------------------------

### Slope ----
slope_riverine_dec2020_apr2021 <- trend_riverine |> 
  filter(Monthly >= yearmonth("2020 Dec") & Monthly <= yearmonth("2021 Apr")) |> 
  autoplot(.vars = trend)

### ARC ----
arc_riverine_dec2020_apr2021 <- ARC(
  ts = trend_riverine,
  start = yearmonth("2020 Dec"),
  end = yearmonth("2021 Apr"), 
  .for = "knots"
)

## ---- Piecewise trend; direction: decrease -----------------------------------

### Slope ----
slope_riverine_may2021_sep2021 <- trend_riverine |> 
  filter(Monthly >= yearmonth("2021 May") & Monthly <= yearmonth("2021 Sep")) |> 
  autoplot(.vars = trend)

### ARC ----
arc_riverine_may2021_sep2021 <- ARC(
  ts = trend_riverine,
  start = yearmonth("2021 May"),
  end = yearmonth("2021 Sep"), 
  .for = "knots"
)

## ---- Piecewise trend; direction: increase -----------------------------------

### Slope ----
slope_riverine_oct2021_aug2023 <- trend_riverine |> 
  filter(Monthly >= yearmonth("2021 Oct") & Monthly <= yearmonth("2023 Aug")) |> 
  autoplot(.vars = trend)

### ARC ----
arc_riverine_oct2021_aug2023 <- ARC(
  ts = trend_riverine,
  start = yearmonth("2021 Oct"),
  end = yearmonth("2023 Aug"), 
  .for = "knots"
)

## ---- Piecewise trend; direction: decrease -----------------------------------

### Slope ----
slope_riverine_sep2023_dec2024 <- trend_riverine |> 
  filter(Monthly >= yearmonth("2023 Sep")) |> 
  autoplot(.vars = trend)

### ARC ----
arc_riverine_sep2023_dec2024 <- ARC(
  ts = trend_riverine,
  start = yearmonth("2023 Sep"),
  end = yearmonth("2024 Dec"), 
  .for = "knots"
)

### ------------------------------------------ URBAN/IDPs LIVELIHOOD SYSTEM ----

################################### End ########################################