################################################################################
###   WORKFLOW FOR CONDUCTING A TIME SERIES ANALYSIS ON SOMALIA'S SAM DATA   ###
################################################################################

## ---- Load required libraries ------------------------------------------------
library(tidyr)
library(dplyr)
library(lubridate)
library(tsibble)
library(feasts)
library(ggplot2)
library(fable)
library(sf)
library(cyphr)

## ---- Retrieve secret key ----------------------------------------------------
secret_key <- data_key(".")

## ---- Read project-specific functions ----------------------------------------
lapply(list.files(path = "R", full.names = TRUE), FUN = source)

## ---- Data wrangling ---------------------------------------------------------
source("scripts/data-wrangling.R")

## ---- Maps -------------------------------------------------------------------
source("scripts/maps.R")

## ---- Exploratory Data Analysis ----------------------------------------------
source("scripts/eda-graphics.R")

## ---- Decomposition  ---------------------------------------------------------
source("scripts/decomposition.R")

## ---- Average Rate of Change -------------------------------------------------
source("scripts/arc.R")