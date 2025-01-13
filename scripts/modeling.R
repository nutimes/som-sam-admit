# ---- Multiple Regression Model -----------------------------------------------

# ################################## NATIONAL ####################################
# ## Specify models ---- 
# fit_admissions <- summarise_admissions(
#   ts = monthly_admissions,
#   .group = FALSE,
#   time = "M"
# ) |> 
#   model(
#     linear = TSLM(admissions ~ trend() + season()),
#     exp_log_trend = TSLM(admissions ~ log(trend()) + season()),
#     exp_log_admissions = TSLM(log(admissions) ~ log(trend()) + season()),
#     box_cox = TSLM(
#       box_cox(admissions, lambda_national) ~ trend(
#         knots = c(
#           yearmonth("2019 Jun"), 
#           yearmonth("2019 Oct"),
#           yearmonth("2020 Feb"),
#           yearmonth("2021 Jan"),
#           yearmonth("2023 Mar"),
#           yearmonth("2024 Dec"))) + season())
#   )

# ### Evaluate residuals ----
# fit_admissions |> 
#   select(box_cox) |> 
#   gg_tsresiduals()

# ### Report model estimates ----
# model_estimates <- report(fit_admissions)

# #### See details of the model estimate ----
# fit_admissions |> 
#   select(box_cox) |> 
#   report()
