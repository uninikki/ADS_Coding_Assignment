library(pharmaversesdtm)
library(admiral)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(metacore)
library(metatools)

# Read in input SDTM data
dm <- pharmaversesdtm::dm
ds <- pharmaversesdtm::ds
ex <- pharmaversesdtm::ex
ae <- pharmaversesdtm::ae
vs <- pharmaversesdtm::vs
suppdm <- pharmaversesdtm::suppdm

# When SAS datasets are imported into R using haven::read_sas(), missing
# character values from SAS appear as "" characters in R, instead of appearing
# as NA values. Further details can be obtained via the following link:
# https://pharmaverse.github.io/admiral/articles/admiral.html#handling-of-missing-values
dm <- convert_blanks_to_na(dm)
ds <- convert_blanks_to_na(ds)
ex <- convert_blanks_to_na(ex)
ae <- convert_blanks_to_na(ae)
vs <- convert_blanks_to_na(vs)
suppdm <- convert_blanks_to_na(suppdm)

dm_suppdm <- combine_supp(dm, suppdm)

# helper functions for age variables

format_agegr9 <- function(age) {
  case_when(
    age < 18 ~ "<18",
    between(age, 18, 50) ~ "18 - 50",
    age > 50 ~ ">50",
    TRUE ~ NA_character_   # Forces this to be pure text
  )
}

format_agegr9n <- function(age) {
  case_when(
    age < 18 ~ 1,
    between(age, 18, 50) ~ 2,
    age > 50 ~ 3,
    TRUE ~ NA_real_
  )
}

ex_ext <- ex %>%
  filter(
    (EXDOSE > 0 | (EXDOSE == 0 & str_detect(EXTRT, "PLACEBO"))) &
      str_length(substr(EXSTDTC, 1, 10)) >= 10
  ) %>%
  derive_vars_dtm(
    dtc = EXSTDTC,
    new_vars_prefix = "TRTS",
    
    highest_imputation = "h",  
    time_imputation = "00:00:00",
    
    flag_imputation = "time",
    ignore_seconds_flag = TRUE  
  ) %>%
  # Derive Treatment End Date to use later for LSTAVLDT
  derive_vars_dt(dtc = EXENDTC, new_vars_prefix = "TRTE")

# VS Domain: Filter valid tests & derive VSDT
vs_ext <- vs %>%
  filter(
    (!is.na(VSSTRESN) | !is.na(VSSTRESC)) &
      str_length(substr(VSDTC, 1, 10)) >= 10
  ) %>%
  derive_vars_dt(dtc = VSDTC, new_vars_prefix = "VS")

# AE Domain: Complete onset dates only
ae_ext <- ae %>%
  filter(str_length(substr(AESTDTC, 1, 10)) >= 10) %>%
  derive_vars_dt(dtc = AESTDTC, new_vars_prefix = "AEST")

# DS Domain: Complete disposition dates only
ds_ext <- ds %>%
  filter(str_length(substr(DSSTDTC, 1, 10)) >= 10) %>%
  derive_vars_dt(dtc = DSSTDTC, new_vars_prefix = "DSST")

# This tells admiral exactly where to look when comparing the 4 datasets
vs_date <- date_source(dataset_name = "vs_ext", date = VSDT)
ae_date <- date_source(dataset_name = "ae_ext", date = AESTDT)
ds_date <- date_source(dataset_name = "ds_ext", date = DSSTDT)
ex_date <- date_source(dataset_name = "ex_ext", date = TRTEDT)

adsl_cust <- dm_suppdm %>%
  
  # Derive Age Groups and ITTFL
  mutate(
    AGEGR9 = format_agegr9(AGE),
    AGER9N = format_agegr9n(AGE),
    ITTFL = if_else(!is.na(ARM), "Y", "N")
  ) %>%
  
  # Derive TRTSDTM and TRTSTMF
  derive_vars_merged(
    dataset_add = ex_ext,
    by_vars = exprs(STUDYID, USUBJID),
    new_vars = exprs(TRTSDTM, TRTSTMF),
    order = exprs(TRTSDTM, EXSEQ),
    mode = "first"
  ) %>%
  
  # Derive LSTAVLDT
  derive_var_extreme_dt(
    new_var = LSTAVLDT,
    vs_date, ae_date, ds_date, ex_date,
    source_datasets = list(
      vs_ext = vs_ext,
      ae_ext = ae_ext,
      ds_ext = ds_ext,
      ex_ext = ex_ext
    ),
    mode = "last"
  ) %>% select(AGEGR9, AGER9N, TRTSDTM, TRTSTMF, ITTFL, LSTAVLDT)

