

# Feature insights into Somalia’s severe acute malnutrition admissions (SAM): a time series analysis spanning from January 2019 to November 2024

This repository contains an R implementation of a time series analysis
aimed at gleaning feature insights from Somalia’s severe acute
malnutrition (SAM) admission data and forming a basis for evidence-based
decision-making regarding the overall country’s nutrition information
system and programming. Data is reported on a monthly basis, at district
level, in 15 regions that constitute Somalia. The window of admissions
spans from January 2019 to November 2024.

> [!NOTE]
>
> The analysis was made possible thanks to the collaboration of the
> [Somalia Nutrition
> Cluster](https://response.reliefweb.int/somalia/nutrition)
> coordination team.

All in all, the analysis seeks to:

- **Identify trend patterns**: a long-term direction or movement in the
  admissions that persists across the series. This represents underlying
  patterns in the admissions after removing short-term fluctuations. The
  analysis will entail identify:

  - The **direction** of the trend along the series:
    - *upward*: a general increase over time.
    - *downward*: a general decrease/decay over time.
    - *flat*: relatively constant over time.
  - The **Shape** of the trend:
    - *Linear trend*: a straight line best fits the data along the
      series, indicating a constant rate of change over time.
    - *Nonlinear trend*: the trend follows a curved line, indicating
      acceleration and deceleration of the rate of change.
  - The **Stability** of the trend:
    - *Stable trend*: one that remains consistent over the entire time
      series.
    - *Changing trend*: one that evolves over time, possibly with abrupt
      shifts.

The average rate of change (ARC) of the trend will be estimated.

- **Identify seasonality patterns**: recurring patterns that occur in a
  fixed and specific period every year. Seasonal patterns can be *fixed*
  or *time-varying*. If the latter, the analysis will explore:

  - *Amplitute of changes*: when the strength and intensity of the peak
    or off-season vary over time.
  - *Phase shifts*: when the timing of the peak season and off-season
    changes over time.
  - *Irregular patterns*: when the periodicity of the seasonal patterns
    are irregular.

- **Quantify the seasonal influences on the admissions**: the extent to
  which different times of the year impact the number of SAM cases
  admitted into the treatment program.

- **Explore key drivers**: identify correlations between admissions and
  contextual factors. This included analyzing the impact of the
  2021/2022 drought on the admissions cases.

- **Visualize data insights**: creating intuitive plots for clearer
  interpretation and communication of results.

The above objectives are addressed by decomposing the time series,
applying other feature extraction techniques, and fitting a model.

## Repository Structure

The repository is structured in the following way:

- `data/`: a data.frame of class `tsibble` containing the admissions of
  SAM cases over time. Data is reported on a monthly basis, with a
  reporting rate \>= 80%, as advised by the data owner. The reporting
  rate is defined as the number of catchment areas that submitted their
  reported in a given month, divided by the overall number of catchment
  areas that are expected to report.
- `R/`: some handy user-defined functions to comply with the principle
  of DRY.  
- `reports/`: The analysis report.
- `scripts/`: A set of `R` scripts used for the analysis. These are
  split into different files, based on the specific objective they
  address:
  - `data-wrangling.R`: load required libraries, then data and then
    wrangle it.
  - `eda-graphics.R`: graphical exploratory data analysis.
  - `decomposition.R`: decompose the time series into trend, seasonal
    effect, and ramainder.
  - `arc.R`: calculate the average rate of change of the trend.
  - `modeling.R:` fit a time series model.

  The following workflow is recommended:

``` mermaid
    flowchart LR
    A[Run data-wrangling.R] 
    B(Run eda-graphics.R)
    C(Run decomposition.R)
    D(Run arc.R)
    E(Run 05-modeling.R)

    A --> B --> C --> D --> E
```

The above flowchart can be implemented simply by running the `scrip.R`
file.

## Reproducibility information

The repository was created in `R` version 4.4.2, and the following
dependencies were used:  
- `{readr}` version 2.1.5  
- `{tidyr}` version 1.3.1  
- `{dplyr}` version 1.1.4  
- `{lubridate}` version 1.9.3  
- `{tsibble}` version 1.1.5  
- `{feasts}` version 0.4.1  
- `{ggplot2}` version 3.5.1  
- `{fable}` version 0.4.1

## License

This repository is licensed under a GNU General Public License 3
(GPL-3).

## Feedback

If you wish to give feedback, file an issue or seek support, kindly do
so [here](https://github.com/nutimes/som-sam-admit/issues).
