################################################################################
#                                DATA WRANGLING                                #
################################################################################


## ---------------------------------------------------------- Tidy the data ----
monthly_admissions <- admissions |>
  pivot_longer(
    cols = !c(region, district, lsystems),
    names_to = "time",
    values_to = "admissions"
  ) |>
  mutate(
    time = gsub("^X", "", time),
    time = gsub("\\.", "/", time), 
    time = ymd(as.Date(time, format = "%d/%m/%Y")),
    Monthly = yearmonth(time)
  ) |>
  relocate(
    Monthly,
    .before = admissions
  ) |>
  select(-time)

## ---- Remove districts with zero admissions ----------------------------------

### --------------------------------------- List of district to be excluded ----
list <- c(
  "Ceel_Dheere", "Jalalaqsi", "Jamaame", "Kurtunwaarey", "Sablaale",
  "Adan Yabaal", "Bu'aale", "Jilib", "Saakow/Salagle", "Sheik", "Cadale",
  "Xarardheere"
)

### --------------------------------------------------- Apply the exclusion ----
monthly_admissions <- monthly_admissions |>
  filter(!(district %in% list))


############################## End of workflow #################################
