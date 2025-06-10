# ==============================================================================
#                                DATA WRANGLING                                
# ==============================================================================


## ---- Tidy the data ----------------------------------------------------------

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

### List of district to be excluded ----
list <- c(
  "Ceel_Dheere", "Jalalaqsi", "Jamaame", "Kurtunwaarey", "Sablaale",
  "Adan Yabaal", "Bu'aale", "Jilib", "Saakow/Salagle", "Sheik", "Cadale",
  "Xarardheere"
)

### Apply the exclusion ----
monthly_admissions <- monthly_admissions |>
  filter(!(district %in% list))

## ---- Summarise admissions ---------------------------------------------------

### At the National level ----
na <- monthly_admissions |>
  summarise_admissions(
    .group = FALSE,
    time = "M"
  )

### By livelihood systems ----
ls <- monthly_admissions |>
  summarise_admissions(
    .group = TRUE,
    time = "M"
  )

## ---- Box-Cox Transformation to stabilize variance ---------------------------

### Get lambda of the data summarised at the nationa level ----
lambda_national <- na |>
  features(
    .var = admissions,
    features = guerrero
  ) |>
  pull(lambda_guerrero)

### Apply transformation ----
na <- na |> 
  mutate(
    admissions = do.call(
      what = box_cox,
      args = list(x = admissions, lambda = lambda_national)
    )
  )

### Visualize the transformation ----
na |>
  autoplot(
    .vars = admissions
  )

## ---- Get lambda of the data summarised at the livelihood systems level ------

## For Pastoral ----
lambda_pastoral <- ls |>
  filter(lsystems == "Pastoral") |>
  features(
    .var = admissions,
    features = guerrero
  ) |>
  pull(lambda_guerrero)

### For Agropastoral ----
lambda_agropastoral <- ls |>
  filter(lsystems == "Agropastoral") |>
  features(
    .var = admissions,
    features = guerrero
  ) |>
  pull(lambda_guerrero)

### For Riverine ----
lambda_riverine <- ls |>
  filter(lsystems == "Riverine") |>
  features(
    .var = admissions,
    features = guerrero
  ) |>
  pull(lambda_guerrero)

### For Urban/IDP's ----
lambda_urban_idps <- ls |>
  filter(lsystems == "Urban/IDPs") |>
  features(
    .var = admissions,
    features = guerrero
  ) |>
  pull(lambda_guerrero)

### Apply row-wise transformation ----
ls <- ls |>
  mutate(
    admissions = do.call(
      what = row_wise_box_cox,
      args = list(admissions = admissions, lsystems = lsystems)
    )
  )

## ---- Visualize the time series after transformation ------------------------

#### Pastoral ----
ls |>
  filter(lsystems == "Pastoral") |>
  autoplot(
    .vars = admissions
  )

#### Agropastoral ----
ls |>
  filter(lsystems == "Agropastoral") |>
  autoplot(
    .vars = admissions
  )

#### Riverine ----
ls |>
  filter(lsystems == "Riverine") |>
  autoplot(
    .vars = admissions
  )

#### Urban/IDPs ----
ls |>
  filter(lsystems == "Urban/IDPs") |>
  autoplot(
    .vars = admissions
  )

# ============================  End of Workflow ================================