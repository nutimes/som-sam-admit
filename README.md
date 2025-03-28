

# Feature insights into Somalia’s severe acute malnutrition admissions (SAM): a time series analysis spanning from January 2019 to December 2024

This repository contains an R implementation of a time series analysis
aimed at gleaning feature insights from Somalia’s severe acute
malnutrition (SAM) admission data and forming a basis for evidence-based
decision-making regarding the overall country’s nutrition information
system and programming. Data is reported on a monthly basis, at district
level, in 15 regions that constitute Somalia. The window of admissions
spans from January 2019 to December 2024.

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
- **Visualize data insights**: creating intuitive plots for clearer
  interpretation and communication of results.

The above objectives are addressed by utilizing time series analysis
techniques.

## Repository Structure

The repository is structured in the following way:

- `data/`: a data.frame of class `tsibble` containing the admissions of
  SAM cases over time. Data is reported on a monthly basis, with a
  reporting rate \>= 80%, as advised by the data owner. The reporting
  rate is defined as the number of catchment areas that submitted their
  reported in a given month, divided by the overall number of catchment
  areas that are expected to report.
- `R/`: some handy user-defined functions for the project.  
- `reports/`: Analysis report and presentation.
- `scripts/`: A set of `R` scripts used for the analysis. These are
  split into different files, based on the specific task they address:
  - `read-in-data.R`: read input data and shapefiles.
  - `data-wrangling.R`: prepare the admission data for downstream
    worflow.
  - `maps.R`: some ilustrative maps.
  - `eda-graphics.R`: graphical exploratory data analysis.
  - `decomposition.R`: decompose the time series into trend, seasonal
    effect, and ramainder.
  - `arc.R`: calculate the average rate of change of the trend component

  The following workflow is recommended:

``` mermaid
    flowchart LR
    A(Retrieve secret key for decryption)
    B(Load project-specific functions.R)
    C(Run read-in-data.R)
    D[Run data-wrangling.R] 
    E(Run maps.R)
    F(Run eda-graphics.R)
    G(Run decomposition.R)
    H(Run arc.R)

    A --> B --> C --> D --> E --> F --> G --> H
```

The above flowchart can be implemented simply by running the `scrip.R`
file found in the root directory.

## Reproducibility information

The repository was created in `R` version 4.4.2. This project uses the
`{renv}` framework to record `R` package dependencies and versions.
Packages and versions used are recorded in `renv.lock` and code used to
manage dependencies is in `renv/` and other files in the root project
directory. On starting an `R` session in the working directory, run
`renv::restore()` to install R package dependencies.

## Data encryption

This project uses `{cyphr}` to encrypt the raw data that lives in
`data-raw/` directory. In order to be able to access and decrypt the
encrypted data, the user will need to have created their own personal
SSH key and make a request to be added to the project. An easy-to-grasp
guide on how to make a request will be found
[here](https://github.com/OxfordIHTM/cyphr-encryption-demonstration#)

## License

This repository is licensed under a GNU General Public License 3
(GPL-3).

## Feedback

If you wish to give feedback, file an issue or seek support, kindly do
so [here](https://github.com/nutimes/som-sam-admit/issues).

## Author

Tomás Zaba
