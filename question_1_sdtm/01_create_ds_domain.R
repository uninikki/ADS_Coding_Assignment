# load required libraries
library(dplyr)
library(sdtm.oak)
library(admiral)
library(pharmaverseraw)


# import raw disposition file
ds_raw <- pharmaverseraw::ds_raw

# generate unique oak identifiers to link raw datasets to the target SDTM domain
ds_raw <- ds_raw %>%
  generate_oak_id_vars(
    pat_var = "PATNUM",
    raw_src = "ds_raw"
  )

ds <- assign_datetime(
  raw_dat = ds_raw,
  raw_var = "IT.DSSTDAT",
  tgt_var = "DSSTDTC",
  raw_fmt = "m-d-y"
) %>% 
  
# map standard DSDECOD
  assign_no_ct(
    raw_dat = condition_add(ds_raw, is.na(ds_raw$OTHERSP)),
    raw_var = "IT.DSDECOD",
    tgt_var = "DSDECOD",
    id_vars = oak_id_vars()
  ) %>%
  
# bring the raw IT.DSDECOD column back into the pipeline to use for conditional logic
  left_join(
    select(ds_raw, oak_id, IT.DSDECOD),
    by = "oak_id"
  ) %>%
  
# derive Disposition Category (DSCAT) based on the raw decode
  mutate(
    DSCAT = if_else(
      condition = IT.DSDECOD == "Randomized", 
      true      = "PROTOCOL MILESTONE", 
      false     = "DISPOSITION EVENT",
      missing   = "DISPOSITION EVENT"
    )
  ) %>% 
  
# if 'Other, specify' (OTHERSP) is populated, use it to overwrite DSTERM
  assign_no_ct(
    raw_dat = condition_add(ds_raw, !is.na(ds_raw$OTHERSP)),
    raw_var = "OTHERSP",
    tgt_var = "DSTERM",
    id_vars = oak_id_vars()
  ) %>% 
  
# if 'Other, specify' (OTHERSP) is populated, use it to overwrite DSDECOD
  assign_no_ct(
    raw_dat = condition_add(ds_raw, !is.na(ds_raw$OTHERSP)),
    raw_var = "OTHERSP",
    tgt_var = "DSDECOD",
    id_vars = oak_id_vars()
  ) %>%
  
# map standard DSTERM (only when 'Other, specify' is completely missing)
  assign_no_ct(
    raw_dat = condition_add(ds_raw, is.na(ds_raw$OTHERSP)),
    raw_var = "IT.DSTERM",
    tgt_var = "DSTERM",
    id_vars = oak_id_vars()
  ) %>%
  
# map and combine the raw date and time columns into a single ISO 8601 string
  assign_datetime(
    raw_dat = ds_raw,
    tgt_var = "DSDTC",
    raw_var = c("DSDTCOL", "DSTMCOL"), 
    raw_fmt = c("m-d-y", "H:M") 
  ) %>%
  
# finalize standard SDTM variables (Domain, IDs, and Visit)
  mutate(
    DOMAIN  = "DS",
    STUDYID = ds_raw$STUDY,
    USUBJID = paste0("01-", ds_raw$PATNUM),
    DSTERM  = toupper(DSTERM),
    VISIT   = ds_raw$INSTANCE
  ) %>%
  
# generate the mandated SDTM sequence number (DSSEQ)
  derive_seq(
    tgt_var = "DSSEQ",
    rec_vars = c("USUBJID", "DSTERM")
  ) %>%
  select(STUDYID, DOMAIN, USUBJID, DSSEQ, 
         DSTERM, DSDECOD, DSCAT, VISIT, DSDTC, DSSTDTC)  

