# import libraries
library(cards)
library(gtsummary)
library(pharmaverseadam)
adae <- pharmaverseadam::adae

teae_data <- adae %>%
  filter(TRTEMFL == "Y") %>%
  
  # choose unique to count patients, not events.
  # unique combinations of Subject, Treatment Arm, and AE Term are kept.
  distinct(USUBJID, ACTARM, AETERM) %>%
  
  #  select only the columns {gtsummary} needs for the table
  select(ACTARM, AETERM)

teae_table <- teae_data %>%
  tbl_summary(
    by = ACTARM,                                
    sort = list(everything() ~ "frequency"),   
    label = list(AETERM ~ "Reported Term for the Adverse Event") 
  ) %>%
  add_overall() %>%                             
  bold_labels() %>%                            
  modify_header(label = "**Primary SAystem Organ Class**")
