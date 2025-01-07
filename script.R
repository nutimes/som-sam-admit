################################################################################
###   WORKFLOW FOR CONDUCTING A TIME SERIES ANALYSIS ON SOMALIA'S SAM DATA   ###
################################################################################

# ---- Load required libraries -------------------------------------------------
library(readr)
library(tidyr)
library(dplyr)
library(lubridate)
library(tsibble)
library(feasts)
library(ggplot2)
library(fable)

# ---- Data wrangling ----------------------------------------------------------
source("scripts/data-wrangling.R")

# ---- Exploratory Data Analysis -----------------------------------------------
source("scripts/eda-graphics.R")

# ---- Average Rate of Change --------------------------------------------------
source("scripts/arc.R")

# ---- Decomposition  ----------------------------------------------------------
source("scripts/decomposition.R")

# ---- Modeling  ---------------------------------------------------------------
source("scripts/modeling.R")
