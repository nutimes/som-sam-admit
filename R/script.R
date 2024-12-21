# Load required libraries ----
library(tsibble)
library(dplyr)
library(tidyr)
library(lubridate)
library(feasts)

# Load data ----
som <- openxlsx::read.xlsx(
  xlsxFile = "data/som_admissions.xlsx",
  sheet = 1,
  detectDates = TRUE,
  colNames = TRUE,
  cols = 1:74
  )
