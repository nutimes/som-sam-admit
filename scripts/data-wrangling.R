
# ---- Load utility functions --------------------------------------------------
source("R/utils.R")

# ---- Read data ---------------------------------------------------------------
admissions <- read_csv(
  file = "data-raw/admissions.csv",
  col_select = -u5_population
)

########################### FOR MONTHLY ANALYSIS ###############################

# ---- Tidy the data -----------------------------------------------------------
monthly_admissions <- admissions |> 
  pivot_longer(
    cols = !c(region, district, lsystems),
    names_to = "time",
    values_to = "admissions"
  ) |> 
  mutate(
    time = ymd(time),
    Monthly = yearmonth(time)
  ) |> 
  relocate(
    Monthly, 
    .before = admissions
) |> 
  select(-time)

# ---- Remove districts with zero admissions ---------------------------------
list <- c("Ceel_Dheere", "Jalalaqsi", "Jamaame", "Kurtunwaarey", "Sablaale", 
"Adan Yabaal", "Bu'aale", "Jilib", "Saakow/Salagle", "Sheik", "Cadale", 
"Xarardheere")

monthly_admissions <- monthly_admissions |> 
  filter(!(district %in% list))
