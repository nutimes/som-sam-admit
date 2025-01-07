############################# MONTHLY ANALYSIS #################################
# ---- Average Rate of Change --------------------------------------------------

## Ungrouped data ----
### Components ----
cm <- monthly_admissions |> 
summarise_admissions(
  .group = FALSE,
  time = "M"
) |> 
  model(STL(admissions)) |> 
  components()
  
### Piecewise trend: 2019 Jan - 2019 Oct; variation: increase ----
cm |> 
filter(Monthly < yearmonth("2019 Oct")) |> 
  select(trend) |> 
  autoplot()+
  labs(
    title = "Trend Slope in SAM Cases Admitted from January to October 2019",
    subtitle = "Number of SAM cases admitted increase by 67 cases each month during this period",
    caption = "Time span: Jan-Oct 2019",
    y = "Average number of cases admitted"
  )+
  theme(
    plot.subtitle = element_text(colour = "#706E6D"),
    plot.caption = element_text(colour = '#706E6D')
  )

### ARC ----
arc_2019 <- cm |> 
  ARC(
    start = yearmonth("2019 Jan"),
    end = yearmonth("2019 Oct"),
    .for = "knots"
  )

### Piecewise trend: 2020 Nov - 2020 Dec; variation: decrease (Covid19) ----
cm |> 
filter(Monthly >= yearmonth("2019 Nov") & Monthly <= yearmonth("2020 Dec")) |> 
  select(trend) |> 
  autoplot()+
  labs(
    title = "Trend Slope in SAM Cases Admitted from November 2019 to December 2020",
    subtitle = "Number of SAM cases admitted decreased by -123 cases each month during this period",
    caption = "Time span: Nov 2019-Dec 2020",
    y = "Average number of cases admitted"
  )+
  theme(
    plot.subtitle = element_text(colour = "#706E6D"),
    plot.caption = element_text(colour = '#706E6D')
  )

### ARC ----
arc_2020 <- cm |> 
  ARC(
    start = yearmonth("2019 Nov"),
    end = yearmonth("2020 Dec"),
    .for = "knots"
  )

### Piecewise trend: 2021 Jan - 2023 Apr; variation: increase (drought) ----
cm |> 
filter(Monthly >= yearmonth("2021 Jan") & Monthly <= yearmonth("2023 Apr")) |> 
  select(trend) |> 
  autoplot()+
  labs(
    title = "Trend Slope in SAM Cases Admitted from January 2021 to April 2023",
    subtitle = "Number of SAM cases admitted increased by 1,160 cases each month during this period",
    caption = "Time span: Jan 2021-Apr 2023",
    y = "Average number of cases admitted"
  )+
  theme(
    plot.subtitle = element_text(colour = "#706E6D"),
    plot.caption = element_text(colour = '#706E6D')
  )

### ARC ----
arc_2021 <- cm |> 
  ARC(
    start = yearmonth("2021 Jan"),
    end = yearmonth("2023 Apr"),
    .for = "knots"
  )

### Piecewise trend: 2023 May - 2024 Dec; variation: decrease ----
cm |> 
filter(Monthly >= yearmonth("2023 May") & Monthly <= yearmonth("2024 Dec")) |> 
  select(trend) |> 
  autoplot()+
  labs(
    title = "Trend Slope in SAM Cases Admitted from May 2023 to December 2024",
    subtitle = "Number of SAM cases admitted decreased by -1,010 cases each month during this period",
    caption = "Time span: May 2023-Dec 2024",
    y = "Average number of cases admitted"
  )+
  theme(
    plot.subtitle = element_text(colour = "#706E6D"),
    plot.caption = element_text(colour = '#706E6D')
  )
  
### ARC ----
arc_2023 <- cm |> 
  ARC(
    start = yearmonth("2023 May"),
    end = yearmonth("2024 Dec"),
    .for = "knots"
  )


######################### BY LIVELIHOOD SYSTEM #################################

cmp_lsystem <- monthly_admissions |> 
summarise_admissions(
  .group = TRUE,
  time = "M"
) |> 
  model(STL(admissions)) |> 
  components() |> 
  select(lsystems, Monthly, trend)


### Piecewise trend: 2023 May - 2024 Dec; variation: decrease ----
cmp_lsystem |> 
  filter(lsystems == "Agropastoral") |>  
  autoplot()+
  labs(
    title = "Trend Slope in SAM Cases Admitted from May 2023 to December 2024",
    subtitle = "Number of SAM cases admitted decreased by -1,010 cases each month during this period",
    caption = "Time span: May 2023-Dec 2024",
    y = "Average number of cases admitted"
  )+
  theme(
    plot.subtitle = element_text(colour = "#706E6D"),
    plot.caption = element_text(colour = '#706E6D')
  )
