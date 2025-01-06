############################# MONTHLY ANALYSIS #################################
# ---- Exploratory Data Analysis: Graphic --------------------------------------

## Admissions grouped at national level ----
## Time plot ----
tsplot_national <- summarise_admissions(
  ts = monthly_admissions,
  .group = FALSE,
  time = "M"
)|> 
  autoplot() +
  labs(
    title = "Time plot: Somalia's SAM Admissions Over Time",
    subtitle = "A changing trend with an upward rise in 2022, and a fall as of 2023, with an irregular seasonal pattern over time",
    caption = "Time span: Jan 2019-Dec 2024",
    y = "Number of cases admitted"
  )+
  theme(
    plot.subtitle = element_text(colour = "#706E6D"),
    plot.caption = element_text(colour = '#706E6D')
  )

## Seasonal plot ----
ssnplot_national <- summarise_admissions(
  ts = monthly_admissions,
  .group = FALSE
) |> 
  gg_season(labels = "right") +
  labs(
    title = "Seasonal plot: Somalia's Monthly SAM Admissions by Year",
    subtitle = "An irregular seasonal pattern with changes in the amplitude before and after 2022",
    caption = "Time span: January 2019-November 2024",
    y = "Number of cases admitted"
  )+
    theme(
      plot.subtitle = element_text(colour = "#706E6D"),
      plot.caption = element_text(colour = '#706E6D')
    )

#### Seasonal plot before 2022 ----
ssplot_national_b2022 <- summarise_admissions(
  ts = monthly_admissions,
  .group = FALSE,
  time = "M"
) |> 
  filter(year(Monthly) < 2022) |> 
  gg_season(labels = "right") +
  labs(
    title = "Seasonal plot: Somalia's Monthly SAM Admissions by Year Before 2022",
    subtitle = "A consistent rise in June, then in September, and then from November to December",
    caption = "Time span: January 2019-December 2021",
    y = "Number of cases admitted"
  )+
    theme(
      plot.subtitle = element_text(colour = "#706E6D"),
      plot.caption = element_text(colour = '#706E6D')
    )

#### Seasonal plot after 2022 ----
ssplot_national_a2022 <- summarise_admissions(
  ts = monthly_admissions,
  .group = FALSE,
  time = "M"
) |> 
  filter(year(Monthly) >= 2022) |> 
  gg_season(labels = "right") +
  labs(
    title = "Seasonal plot: Somalia's Monthly SAM Admissions by Year as of 2022",
    subtitle = "A consistent fall in April, followed by a rise in May, and another rise from November to January",
    caption = "Time span: Jan 2022-Dec 2024",
    y = "Number of cases admitted"
  )+
    theme(
      plot.subtitle = element_text(colour = "#706E6D"),
      plot.caption = element_text(colour = '#706E6D')
    )


## Subseries plot ----
sbsplot_national <- summarise_admissions(
  ts = monthly_admissions,
  .group = FALSE,
  time = "M"
) |> 
  gg_subseries() +
  labs(
    title = "Subseries plot: Somalia's SAM admissions",
    subtitle = "Number of SAM cases admitted increased exponentially as of 2020 and started to decline as of 2023",
    caption = "Time span: Jan 2019-Dec 2024",
    y = "Number of cases admitted",
  )+
    theme(
      plot.subtitle = element_text(colour = "#706E6D"),
      plot.caption = element_text(colour = '#706E6D')
    )

### Subseries plot before 2022 ----
sbsplot_national_b2022 <- summarise_admissions(
  ts = monthly_admissions,
  .group = FALSE,
  time = "M"
) |> 
  filter(year(Monthly) < 2022) |> 
  gg_subseries() +
  labs(
    title = "Subseries plot: Somalia's SAM admissions",
    caption = "Time span: Jan 2019-Dec 2021",
    y = "Number of cases admitted"
  )+
    theme(
      plot.subtitle = element_text(colour = "#706E6D"),
      plot.caption = element_text(colour = '#706E6D')
    )

### Subseries plot as of 2022 ----
sbsplot_national_a2022 <- summarise_admissions(
  ts = monthly_admissions,
  .group = FALSE,
  time = "M"
) |> 
  filter(year(Monthly) >= 2022) |> 
  gg_subseries() +
  labs(
    title = "Subseries plot: Somalia's SAM admissions",
    caption = "Time span: Jan 2022-Dec 2024",
    y = "Number of cases admitted"
  )+
    theme(
      plot.subtitle = element_text(colour = "#706E6D"),
      plot.caption = element_text(colour = '#706E6D')
    )

######################### BY LIVELIHOOD SYSTEM #################################

### Time plot ----
tsplot_lsysttem <- summarise_admissions(
  ts = monthly_admissions, 
  .group = TRUE,
  time = "M"
) |> 
  autoplot() +
  facet_wrap(vars(lsystems), scales = "free_y") +
  labs(
    title = "Time plot: Somalia's SAM admissions Over Time by Livelihood Systems",
    subtitle = "Nearly the same trend and seasonal patterns as the national, except the Riverines",
    caption = "Time span: Jan 2019-Dec 2024",
    y = "Number of cases admitted"
  ) +
  theme(
    legend.position = "none",
    plot.subtitle = element_text(colour = "#706E6D"),
    plot.caption = element_text(colour = '#706E6D')
  )

### Seasonal plot ----
ssnplot_lsystem <- summarise_admissions(
  ts = monthly_admissions,
  .group = TRUE,
  time = "M"
) |> 
  gg_season() +
  facet_wrap(vars(lsystems), scales = "free_y") +
  labs(
    title = "Seasonal plot: Somalia's Monthly SAM Admissions by Year by Livelihood Systems",
    subtitle = "",
    caption = "Time span: Jan 2019-Dec 2024",
    y = "Number of cases admitted"
  )+
    theme(
      plot.subtitle = element_text(colour = "#706E6D"),
      plot.caption = element_text(colour = '#706E6D')
    )

### Seasonal plot ----
sbsplot_lsystem <- summarise_admissions(
  ts = monthly_admissions,
  .group = TRUE,
  time = "M"
) |> 
  gg_subseries() +
  labs(
    title = "Subseries plot: Somalia's Monthly SAM Admissions by Year by Livelihood Systems",
    subtitle = "Time span: Jan 2019-Dec 2024",
    y = "Number of cases admitted"
  )+
    theme(
      plot.subtitle = element_text(colour = "#706E6D"),
      plot.caption = element_text(colour = '#706E6D')
    )

# ########################### QUARTERLY ANALYSIS #############################

# ## Ungrouped quarterly time plot ----
# quarterly_admissions |> 
#   summarise_admissions(
#     .group = FALSE,
#     time = "Q"
#   ) |> 
#   autoplot() +
#   labs(
#     title = "Time plot: Somalia's SAM admissions by quarter",
#     subtitle = "2019 Q1 : 2024 Q4",
#     y = "Number of cases admitted"
#   )

# ### Quarterly time plot by Region ----
# quarterly_admissions |> 
#   summarise_admissions(
#     .group = TRUE,
#     time = "Q"
#   ) |> 
#   autoplot() +
#   facet_wrap(vars(region), scales = "free_y") +
#   labs(
#     title = "Time plot: Somalia's SAM admissions by Region by quarter",
#     subtitle = "2019 Q1 : 2024 Q4",
#     y = "Number of cases admitted"
#   ) +
#   theme(legend.position = "none")


# ### Seasonal plot ----
# quarterly_admissions |> 
#   summarise_admissions(
#     .group = FALSE,
#     time = "Q"
#   ) |> 
#   gg_season(labels = "right") +
#   labs(
#     title = "Seasonal plot: Somalia's SAM admissions by quarter",
#     subtitle = "2019 Q1 : 2024 Q4",
#     y = "Number of cases admitted"
#   )

# ### Seasonal plot facetted by Region ----
# quarterly_admissions |> 
#   summarise_admissions(
#     .group = TRUE,
#     time = "Q"
#   ) |> 
#   gg_season() +
#   facet_wrap(vars(region), scales = "free_y") +
#   labs(
#     title = "Seasonal plot: Somalia's SAM admissions by Region by quarter",
#     subtitle = "2019 Q1 : 2024 Q4",
#     y = "Number of cases admitted"
#   )

# ### Subseries plot ----
# quarterly_admissions |> 
#   summarise_admissions(
#     .group = FALSE,
#     time = "Q"
#   )|>
#   gg_subseries() +
#   labs(
#     title = "Subseries plot: Somalia's SAM admissions by quarter",
#     subtitle = "2019 Q1 : 2024 Q4",
#     y = "Number of cases admitted"
#   )

# ### Subseries plot by Region ----
# quarterly_admissions |> 
#   summarise_admissions(
#     .group = TRUE,
#     time = "Q"
#   )|>
#   gg_subseries() +
#   labs(
#     title = "Subseries plot: Somalia's SAM admissions by by Region by quarter",
#     subtitle = "2019 Q1 : 2024 Q4",
#     y = "Number of cases admitted"
#   )