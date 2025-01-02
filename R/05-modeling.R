# ---- Multiple Regression Model -----------------------------------------------

########################### QUARTERLY ANALYSIS #############################
## Specify models ----
fit_admissions <- quarterly_admissions |> 
  summarise_admissions(
    .group = FALSE,
    time = "Q"
  ) |> 
  model(
    linear = TSLM(
      admissions ~ trend() + season()
    ),
    exp_log_trend = TSLM(
      admissions ~ log(trend()) + season()
    ),
    exp_log_admissions = TSLM(
      log(admissions) ~ log(trend()) + season()
    ),
    piecewise = TSLM(
      log(admissions) ~ trend(knots = c(yearquarter("2019 Q4"),
                                   yearquarter("2020 Q4"),
                                   yearquarter("2023 Q2"),
                                   yearquarter("2024 Q4"))) + season()
    ),
    box_cox = TSLM(
      box_cox(admissions, lambda) ~ trend(knots = c(yearquarter("2019 Q4"),
                                                    yearquarter("2020 Q4"),
                                                    yearquarter("2023 Q2"),
                                                    yearquarter("2024 Q4"))) + season())
  )

### Evaluate residuals ----
fit_admissions |> 
  select(box_cox) |> 
  gg_tsresiduals()

### Report model estimates ----
model_estimates <- report(fit_admissions)

#### See details of the model estimate ----
fit_admissions |> 
  select(box_cox) |> 
  report()
