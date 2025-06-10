# ==============================================================================
#                           EXPLORATORY DATA ANALYIS
# ==============================================================================


## ---- National level ---------------------------------------------------------

### Time plot ----
tsplot_national <- summarise_admissions(
  ts = monthly_admissions,
  .group = FALSE,
  time = "M"
) |>
  autoplot(.vars = admissions) +
  labs(
    title = "Somalia's SAM Admissions Over Time",
    subtitle = "A changing trend with an rise in 2022, and a fall as of 2023, with an irregular seasonal pattern over time",
    y = "Cases admitted"
  ) +
  theme(
    plot.subtitle = element_text(colour = "#706E6D", size = 9.5),
    plot.caption = element_text(colour = "#706E6D"),
    axis.title.y = element_text(size = 10, margin = margin(r = 5)),
    axis.title.x = element_text(size = 10, margin = margin(t = 7)),
    plot.title = element_text(size = 12)
  )

### Seasonal plot ----
ssnplot_national <- summarise_admissions(
  ts = monthly_admissions,
  .group = FALSE
) |>
  gg_season(
    y = admissions,
    labels = "right"
  ) +
  labs(
    title = "Somalia's Monthly SAM Admissions by Year",
    subtitle = "An irregular seasonal pattern with changes in the amplitude, and a shift in the seasonality before and after 2022",
    y = "Cases admitted"
  ) +
  theme(
    plot.subtitle = element_text(colour = "#706E6D"),
    plot.caption = element_text(colour = "#706E6D"),
    axis.title.y = element_text(size = 10, margin = margin(r = 5)),
    axis.title.x = element_text(size = 10, margin = margin(r = 5)),
    plot.title = element_text(size = 12)
  )

#### Seasonal plot before 2022 ----
ssplot_national_b2022 <- summarise_admissions(
  ts = monthly_admissions,
  .group = FALSE,
  time = "M"
) |>
  filter(year(Monthly) < 2022) |>
  gg_season(
    y = admissions,
    labels = "right"
  ) +
  labs(
    title = "Somalia's Monthly SAM Admissions by Year Before 2022",
    subtitle = "A consistent rise in June, then in September, and then from November to December",
    y = "Cases admitted"
  ) +
  theme(
    plot.subtitle = element_text(colour = "#706E6D"),
    plot.caption = element_text(colour = "#706E6D"),
    axis.title.y = element_text(size = 10, margin = margin(r = 5)),
    axis.title.x = element_text(size = 10, margin = margin(r = 5)),
    plot.title = element_text(size = 12)
  )

#### Seasonal plot after 2022 ----
ssplot_national_a2022 <- summarise_admissions(
  ts = monthly_admissions,
  .group = FALSE,
  time = "M"
) |>
  filter(year(Monthly) >= 2022) |>
  gg_season(
    y = admissions,
    labels = "right"
  ) +
  labs(
    title = "Somalia's Monthly SAM Admissions by Year as of 2022",
    subtitle = "A consistent fall in April, followed by a rise in May, and another rise from November to January",
    y = "Cases admitted"
  ) +
  theme(
    plot.subtitle = element_text(colour = "#706E6D"),
    plot.caption = element_text(colour = "#706E6D"),
    axis.title.y = element_text(size = 10, margin = margin(r = 5)),
    axis.title.x = element_text(size = 10, margin = margin(r = 5)),
    plot.title = element_text(size = 12)
  )

### Subseries plot ----
sbsplot_national <- summarise_admissions(
  ts = monthly_admissions,
  .group = FALSE,
  time = "M"
) |>
  gg_subseries(y = admissions) +
  labs(
    title = "Somalia's SAM admissions",
    subtitle = "Number of SAM cases admitted increased exponentially as of 2020 and started to decline as of 2023",
    y = "Cases admitted",
  ) +
  theme(
    plot.subtitle = element_text(colour = "#706E6D"),
    plot.caption = element_text(colour = "#706E6D"),
    axis.title.y = element_text(size = 10, margin = margin(r = 5)),
    axis.title.x = element_text(size = 10, margin = margin(r = 5)),
    plot.title = element_text(size = 12)
  )

#### Subseries plot before 2022 ----
sbsplot_national_b2022 <- summarise_admissions(
  ts = monthly_admissions,
  .group = FALSE,
  time = "M"
) |>
  filter(year(Monthly) < 2022) |>
  gg_subseries(y = admissions) +
  labs(
    title = "Somalia's SAM admissions",
    y = "Cases admitted"
  ) +
  theme(
    plot.subtitle = element_text(colour = "#706E6D"),
    plot.caption = element_text(colour = "#706E6D"),
    axis.title.y = element_text(size = 10, margin = margin(r = 5)),
    axis.title.x = element_text(size = 10, margin = margin(r = 5)),
    plot.title = element_text(size = 12)
  )

#### Subseries plot as of 2022 ----
sbsplot_national_a2022 <- summarise_admissions(
  ts = monthly_admissions,
  .group = FALSE,
  time = "M"
) |>
  filter(year(Monthly) >= 2022) |>
  gg_subseries(y = admissions) +
  labs(
    title = "Somalia's SAM admissions",
    y = "Cases admitted"
  ) +
  theme(
    plot.subtitle = element_text(colour = "#706E6D"),
    plot.caption = element_text(colour = "#706E6D"),
    axis.title.y = element_text(size = 10, margin = margin(r = 5)),
    axis.title.x = element_text(size = 10, margin = margin(r = 5)),
    plot.title = element_text(size = 12)
  )

## ---- Livelihood systems -----------------------------------------------------

### Time plot ----
tsplot_lsysttem <- summarise_admissions(
  ts = monthly_admissions,
  .group = TRUE,
  time = "M"
) |>
  autoplot(.vars = admissions) +
  facet_wrap(vars(lsystems), scales = "free_y") +
  labs(
    title = "Somalia's SAM admissions Over Time by Livelihood Systems",
    subtitle = "Nearly the same trend and seasonal patterns as the national, except the Riverines",
    y = "Cases admitted"
  ) +
  theme(
    legend.position = "none",
    plot.subtitle = element_text(colour = "#706E6D"),
    plot.caption = element_text(colour = "#706E6D"),
    axis.title.y = element_text(size = 10, margin = margin(r = 5)),
    axis.title.x = element_text(size = 10, margin = margin(r = 5)),
    plot.title = element_text(size = 12)
  )

### Seasonal plot ----
ssnplot_lsystem <- summarise_admissions(
  ts = monthly_admissions,
  .group = TRUE,
  time = "M"
) |>
  gg_season(y = admissions) +
  facet_wrap(vars(lsystems), scales = "free_y") +
  labs(
    title = "Somalia's Monthly SAM Admissions by Year by Livelihood Systems",
    subtitle = "",
    y = "Cases admitted"
  ) +
  theme(
    plot.subtitle = element_text(colour = "#706E6D"),
    plot.caption = element_text(colour = "#706E6D"),
    axis.title.y = element_text(size = 10, margin = margin(r = 5)),
    axis.title.x = element_text(size = 10, margin = margin(r = 5)),
    plot.title = element_text(size = 12)
  )

### Seasonal plot ----
sbsplot_lsystem <- summarise_admissions(
  ts = monthly_admissions,
  .group = TRUE,
  time = "M"
) |>
  gg_subseries(y = admissions) +
  labs(
    title = "Somalia's Monthly SAM Admissions by Year by Livelihood Systems",
    y = "Cases admitted"
  ) +
  theme(
    plot.subtitle = element_text(colour = "#706E6D"),
    plot.caption = element_text(colour = "#706E6D"),
    axis.title.y = element_text(size = 10, margin = margin(r = 5)),
    axis.title.x = element_text(size = 10, margin = margin(r = 5)),
    plot.title = element_text(size = 12)
  )

# ============================  End of Workflow ================================