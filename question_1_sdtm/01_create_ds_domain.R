library(dplyr)
library(sdtm.oak)
library(admiral)
library(pharmaverseraw)
library(pharmaversesdtm)

ds_raw <- pharmaverseraw::ds_raw
dm_domain <- pharmaversesdtm::dm

# links raw dataset and linked ds domain
ds_raw <- ds_raw %>%
  generate_oak_id_vars(
    pat_var = "PATNUM",
    raw_src = "ds_raw"
  )

study_ct <-
  data.frame(
    stringsAsFactors = FALSE,
    codelist_code = c("C66727","C66727",
                      "C66727","C66727","C66727","C66727","C66727","C66727",
                      "C66727","C66727"),
    term_code = c("C41331","C25250",
                  "C28554","C48226","C48227","C48250","C142185","C49628",
                  "C49632","C49634"),
    term_value = c("ADVERSE EVENT",
                   "COMPLETED","DEATH","LACK OF EFFICACY","LOST TO FOLLOW-UP",
                   "PHYSICIAN DECISION","PROTOCOL VIOLATION",
                   "SCREEN FAILURE","STUDY TERMINATED BY SPONSOR",
                   "WITHDRAWAL BY SUBJECT"),
    collected_value = c("Adverse Event",
                        "Complete","Dead","Lack of Efficacy","Lost To Follow-Up",
                        "Physician Decision","Protocol Violation",
                        "Trial Screen Failure","Study Terminated By Sponsor",
                        "Withdrawal by Subject"),
    term_preferred_term = c("AE","Completed","Died",
                            NA,NA,NA,"Violation",
                            "Failure to Meet Inclusion/Exclusion Criteria",NA,"Dropout"),
    term_synonyms = c("ADVERSE EVENT",
                      "COMPLETE","Death",NA,NA,NA,NA,NA,NA,
                      "Discontinued Participation")
  )

ds <- assign_datetime(
        raw_dat = ds_raw,
        raw_var = c("IT.DSSTDAT"),
        tgt_var = "DSSTDTC",
        raw_fmt = c("m-d-y")
) %>% 
#OTHERSP  
  assign_no_ct(
  raw_dat = condition_add(ds_raw, is.na(ds_raw$OTHERSP)),
  raw_var = "IT.DSDECOD",
  tgt_var = "DSDECOD",
  id_vars = oak_id_vars()
) %>%
  left_join(
    select(ds_raw, oak_id, IT.DSDECOD),
    by = "oak_id"
  ) %>%
  
  mutate(
    DSCAT = if_else(
      condition = IT.DSDECOD == "Randomized", 
      true      = "PROTOCOL MILESTONE", 
      false     = "DISPOSITION EVENT",
      missing   = "DISPOSITION EVENT"
    )
  ) %>% assign_no_ct(
  raw_dat = condition_add(ds_raw, !is.na(ds_raw$OTHERSP)),
  raw_var = "OTHERSP",
  tgt_var = "DSTERM",
  id_vars = oak_id_vars()
) %>% assign_no_ct(
    raw_dat = condition_add(ds_raw, !is.na(ds_raw$OTHERSP)),
    raw_var = "OTHERSP",
    tgt_var = "DSDECOD",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = condition_add(ds_raw, is.na(ds_raw$OTHERSP)),
    raw_var = "IT.DSTERM",
    tgt_var = "DSTERM",
    id_vars = oak_id_vars()
  ) %>%
assign_datetime(
    raw_dat = ds_raw,
    tgt_var = "DSDTC",
    raw_var = c("DSDTCOL", "DSTMCOL"), 
    raw_fmt = c("m-d-y", "H:M") 
  ) 

ds <- ds %>%
  mutate(DOMAIN = "DS",
         STUDYID = ds_raw$STUDY,
         USUBJID = paste0("01-", ds_raw$PATNUM),
         DSTERM = toupper(DSTERM),
         VISIT = ds_raw$INSTANCE) %>%
  derive_seq(
    tgt_var = "DSSEQ",
    rec_vars = c("USUBJID", "DSTERM")
  ) 
    
  

