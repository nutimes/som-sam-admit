
# ---- Average Rate of Change --------------------------------------------------

########################### QUARTERLY ANALYSIS #############################

## Ungrouped data ----
### Components ----
cm <- som_admissions_quarterly |> 
summarise_admissions(
  .group = FALSE,
  time = "Q"
) |> 
  model(STL(admissions)) |> 
  components()
  
### Trend plot of Piece 1: 2019 Q1 - 2019 Q4; variation: increase ----
cm |> 
filter(Quarterly <= yearquarter("2019 Q4")) |> 
  select(trend) |> 
  autoplot()

### ARC ----
arc_2019 <- cm |> 
  ARC(
    start = yearquarter("2019 Q1"),
    end = yearquarter("2019 Q4"),
    .for = "knots"
  )

### Trend plot of Piece 2: 2020 Q1 - 2020 Q4; variation: decrease (Covid19) ----
cm |> 
filter(Quarterly >= yearquarter("2020 Q1") & Quarterly <= yearquarter("2020 Q4")) |> 
  select(trend) |> 
  autoplot()

### ARC ----
arc_2020 <- cm |> 
  ARC(
    start = yearquarter("2020 Q1"),
    end = yearquarter("2020 Q4"),
    .for = "knots"
  )

### Trend plot of Piece 3: 2021 Q1 - 2023 Q2; variation: increase (drought) ----
cm |> 
filter(Quarterly >= yearquarter("2021 Q1") & Quarterly <= yearquarter("2023 Q2")) |> 
  select(trend) |> 
  autoplot()

### ARC ----
arc_2021 <- cm |> 
  ARC(
    start = yearquarter("2021 Q1"),
    end = yearquarter("2023 Q2"),
    .for = "knots"
  )

### Trend plot of Piece 4: 2023 Q3 - 2024 Q4; variation: decrease ----
cm |> 
filter(Quarterly >= yearquarter("2023 Q3") & Quarterly <= yearquarter("2024 Q4")) |> 
  select(trend) |> 
  autoplot()

### ARC ----
arc_2023 <- cm |> 
  ARC(
    start = yearquarter("2023 Q3"),
    end = yearquarter("2024 Q4"),
    .for = "knots"
  )
