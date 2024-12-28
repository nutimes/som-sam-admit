# ---- Multiple Regression Model -----------------------------------------------
## Check trend linearity ----
som_admissions_quarterly |> 
  summarise_admissions(
    .group = FALSE,
    time = "Q"
  ) |> 
  ggplot(aes(x = Quarterly, y = admissions)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE)

## Specify models ----
fit_admissions <- som_admissions_quarterly |> 
  summarise_admissions(
    .group = FALSE,
    time = "Q"
  ) |> 
  model(
    linear = TSLM(admissions ~ trend() + season()),
    exp_log_trend = TSLM(admissions ~ log(trend()) + season()),
    exp_log_admissions = TSLM(log(admissions) ~ log(trend()) + season()),
    piecewise = TSLM(
      log(admissions) ~ trend(knots = c(yearquarter("2020 Q1"),
                                   yearquarter("2020 Q2"),
                                   yearquarter("2021 Q1"),
                                   yearquarter("2023 Q1"))) + season()
    )
  )

### Evaluate residuals ----
fit_admissions |> 
  select(piecewise) |> 
  gg_tsresiduals()

### Report model estimates ----
model_estimates <- report(fit_admissions)

#### See details of the model estimate ----
fit_admissions |> 
  select(piecewise) |> 
  report()
