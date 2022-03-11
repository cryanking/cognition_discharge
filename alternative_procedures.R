# LSF_DOCKER_VOLUMES='/storage1/fs1/christopherking/Active/ActFastData:/research/' bsub -G 'compute-christopherking' -n 8 -R 'rusage[mem=64GB] span[hosts=1]' -M 64GB -q general-interactive -Is -a 'docker(cryanking/verse_words:1.2)' R

library(lubridate)
library(readxl)
library(magrittr)
library(dplyr)
library(readr)

########
## Comparing procedure code datasets
########

## a reference file of case_ids to map procedures to case events
all_ids <- read_csv('/research/Actfast_reident/mv_era_all_ids.csv', col_types = 'icTTcccccctl')


## The older I2 data
## this has 86755 distinct hospitalizations and 94698 distinct cases
## it stops in 2016
procedure_codes_late_location  <- "/research/ActFast_CLORE/Cohort_Identifiers_Results/king_cohort_procedures.txt"
Actfast_proc2 <- read.delim(procedure_codes_late_location  , stringsAsFactors=FALSE, colClasses="character")
Actfast_proc2$ICDX_PROCEDURE_CODE %<>% trimws
Actfast_proc2$PAN %<>% trimws

patient_list_early_location <- "/research/ActFast_CLORE/Cohort_Identifiers_Results/King_Cohort_Identifiers_results.xlsx"
Actfast_roll2 <- read_xlsx(patient_list_early_location , sheet = "Pat_List", col_types="text")
Actfast_roll2$PAN %<>% trimws

Actfast_proc2 %<>% left_join(Actfast_roll2 %>% select(PatientID=PATIENTID, PAN), by="PAN")
Actfast_proc2 %<>% inner_join(all_ids %>% select(PatientID, DoB, DoS ) , by="PatientID" )

## 11k matching target procedures
Actfast_proc2 %>% filter( ICDX_PROCEDURE_CODE %in% c("0FT44ZZ","0DTN0ZZ", "0DB64Z3", "0FBG0ZZ", "0FBG3ZZ","0UT90ZZ", "0UB74ZZ", "0SG00A0", "0SRC0J9","0SRB04Z","0RRJ00Z","0DQ53ZZ","0FT20ZZ","0TT10ZZ","0VT08ZZ","0TTB0ZZ","B50W","51.23","45.8","43.82","52.7","52.0","68.4","68.5","68.9","68.49","81.06","81.54","81.51","81.80","53.9","32.41","32.49","55.4","60.5","57.71","39.52","39.53")) %>% pull("PAN") %>% n_distinct


## a second file for the post-2016 data
## 28820 cases 24760 hospitalizations
clore_new_cohort_location <- "/research/ActFast_CLORE/Actfast_Results/ActFastResults.xlsx"

Actfast_roll <- read_xlsx(clore_new_cohort_location, sheet = "Pat_List", col_types=c("text" , "text", "date", "text", "text", "text", "date", "date"))
Actfast_proc <- read_xlsx(clore_new_cohort_location, sheet = "Procedures", col_types="text")

actfast_proc_late <- Actfast_proc %>% left_join(Actfast_roll %>% select(MRN, REG_NO, DOS), by="REG_NO") %>% inner_join(all_ids %>% select(PatientID,REG_NO=VisitIDCode ),  by="REG_NO")


### The recent I2 query
## this has 66816 distinct hospitalizations and 75948 distinct cases
## spans whole era - I have no idea why it is missing so many patients
procedure_code_location <- '/research/ActFast_Epic_Flow/Data 202004/CDS data/CDS Procedures.csv'
procedure_data <- read_csv(procedure_code_location, col_types = 'cccccc' )

## this maps all but 50 events, acceptable losses
# procedure_data %>% inner_join(all_ids %>% select(PatientID,VisitIDCode, DoB, DoS ) %>% rename(REG_NO=VisitIDCode) , by="REG_NO") %>% pull("REG_NO") %>% n_distinct

procedure_data %<>% inner_join(all_ids %>% select(PatientID,VisitIDCode, DoB, DoS ) %>% rename(REG_NO=VisitIDCode) , by="REG_NO")

procedure_data %>% filter( ICDX_PROCEDURE_CODE %in% c("0FT44ZZ","0DTN0ZZ", "0DB64Z3", "0FBG0ZZ", "0FBG3ZZ","0UT90ZZ", "0UB74ZZ", "0SG00A0", "0SRC0J9","0SRB04Z","0RRJ00Z","0DQ53ZZ","0FT20ZZ","0TT10ZZ","0VT08ZZ","0TTB0ZZ","B50W","51.23","45.8","43.82","52.7","52.0","68.4","68.5","68.9","68.49","81.06","81.54","81.51","81.80","53.9","32.41","32.49","55.4","60.5","57.71","39.52","39.53")) %>% pull("REG_NO") %>% n_distinct

save(file="/research/ActFast_Intermediates/matched_proc_codes.rdata" , Actfast_proc2, actfast_proc_late, procedure_data)


##############
## mapping CPAP events to surgery events
##############
newest_preop_location <- "/research/ActFast_Big/PreOp_Static/2020_07_ACTFAST_MV_PreOp.csv"

datapreop <- read_delim(newest_preop_location , na=c("NULL","","na","-", "*", "U", "NR", "?", "+") ,  delim="^", col_types="ccccccTccciiiddddiiiiiiiiiiiiiiiiiiiiiiiiiiiiidiiiiiiiidddddd")

CPAP_mapping <- datapreop %>% select(Surg_PatientID, Data_PatientID, EMPI, MRN, PAN, DoS) %>% distinct

write_csv(CPAP_mapping, file="/research/ActFast_Intermediates/CPAP_to_surg.csv"  )


###############
##Experimenting on the data
#################

# it has 1220 rows matching with procedure codes
signals_procedure <- Signals %>% filter(Age_at_CPAP >= 65)   %>%
  filter(!is.na(AD8) ) %>% filter(!is.na(SBT) ) %>%
  inner_join(ProcedureCodes %>% filter(ICD_CODE_VERSION == "9-CM" | ICD_CODE_VERSION =="10-PCS") %>% filter( ICD_PROCEDURE_CODE %in% c("0FT44ZZ","0DTN0ZZ", "0DB64Z3", "0FBG0ZZ", "0FBG3ZZ","0UT90ZZ", "0UB74ZZ", "0SG00A0", "0SRC0J9","0SRB04Z","0RRJ00Z","0DQ53ZZ","0FT20ZZ","0TT10ZZ","0VT08ZZ","0TTB0ZZ","B50W","51.23","45.8","43.82","52.7","52.0","68.4","68.5","68.9","68.49","81.06","81.54","81.51","81.80","53.9","32.41","32.49","55.4","60.5","57.71","39.52","39.53")) %>% distinct(CPAPPatientID, .keep_all=T), by="CPAPPatientID")

num_overlap(Signals, procedure_data) # I dont see any common Id's in this datasets to join them
num_overlap(Signals, all_ids) # I dont see any common Id's in this datasets to join them
num_overlap(Signals, CPAP_mapping) # i see signals$ CPAPPatientID and Data_PatientID has 24968 matchings but there are missing rows as well.

# I got 24293 matchings b/w Signals and CPAP_mapping
signal_preop <- Signals %>% filter(Age_at_CPAP >= 65) %>%
  filter(!is.na(AD8) ) %>% filter(!is.na(SBT)) %>%
  inner_join(CPAP_mapping, by= c("CPAPPatientID" = "Data_PatientID"))

num_overlap(signal_preop, procedure_data)
# I joined signals and procedure_data(CDS Procedures.csv) and got 7283 matches
procedures_data <- procedure_data %>% filter( ICDX_PROCEDURE_CODE %in% c("0FT44ZZ","0DTN0ZZ", "0DB64Z3", "0FBG0ZZ", "0FBG3ZZ","0UT90ZZ", "0UB74ZZ", "0SG00A0", "0SRC0J9","0SRB04Z","0RRJ00Z","0DQ53ZZ","0FT20ZZ","0TT10ZZ","0VT08ZZ","0TTB0ZZ","B50W","51.23","45.8","43.82","52.7","52.0","68.4","68.5","68.9","68.49","81.06","81.54","81.51","81.80","53.9","32.41","32.49","55.4","60.5","57.71","39.52","39.53"))  
signals_procedures <- signal_preop %>% inner_join(procedures_data, by = c("Surg_PatientID" = "PatientID"))
# there is difference b/w signals with ProcedureCodes(1220) and Signals with procedure_data(2048)


#combining Actfast_proc2 with cpap_mapping and filtering it with ICD_procedures and then combining it with signals
# I found this is not the best way
num_overlap(Actfast_proc2, CPAP_mapping)
Actfastproc2_cpapMapping <- Actfast_proc2 %>% inner_join(CPAP_mapping, by=c("PatientID" = "Data_PatientID")) %>%
  filter( ICDX_PROCEDURE_CODE %in% c("0FT44ZZ","0DTN0ZZ", "0DB64Z3", "0FBG0ZZ", "0FBG3ZZ","0UT90ZZ", "0UB74ZZ", "0SG00A0", "0SRC0J9","0SRB04Z","0RRJ00Z","0DQ53ZZ","0FT20ZZ","0TT10ZZ","0VT08ZZ","0TTB0ZZ","B50W","51.23","45.8","43.82","52.7","52.0","68.4","68.5","68.9","68.49","81.06","81.54","81.51","81.80","53.9","32.41","32.49","55.4","60.5","57.71","39.52","39.53"))
num_overlap(Signals, Actfastproc2_cpapMapping)



# I joined signal_preop with Actfast_proc2 by PatientID(2767) and by PAN(2937). Which combination is better?
num_overlap(signal_preop, Actfast_proc2)
signal_preop2<- signal_preop %>% inner_join(Actfast_proc2, by=c("Surg_PatientID" = "PatientID")) %>%
  filter( ICDX_PROCEDURE_CODE %in% c("0FT44ZZ","0DTN0ZZ", "0DB64Z3", "0FBG0ZZ", "0FBG3ZZ","0UT90ZZ", "0UB74ZZ", "0SG00A0", "0SRC0J9","0SRB04Z","0RRJ00Z","0DQ53ZZ","0FT20ZZ","0TT10ZZ","0VT08ZZ","0TTB0ZZ","B50W","51.23","45.8","43.82","52.7","52.0","68.4","68.5","68.9","68.49","81.06","81.54","81.51","81.80","53.9","32.41","32.49","55.4","60.5","57.71","39.52","39.53"))

signal_preop3<- signal_preop %>% inner_join(Actfast_proc2, by= "PAN") %>%
  filter( ICDX_PROCEDURE_CODE %in% c("0FT44ZZ","0DTN0ZZ", "0DB64Z3", "0FBG0ZZ", "0FBG3ZZ","0UT90ZZ", "0UB74ZZ", "0SG00A0", "0SRC0J9","0SRB04Z","0RRJ00Z","0DQ53ZZ","0FT20ZZ","0TT10ZZ","0VT08ZZ","0TTB0ZZ","B50W","51.23","45.8","43.82","52.7","52.0","68.4","68.5","68.9","68.49","81.06","81.54","81.51","81.80","53.9","32.41","32.49","55.4","60.5","57.71","39.52","39.53"))

# missing data b/w signal_preop2 and signal_preop3 is around 689 rows
missing_data <- signal_preop2 %>% anti_join(signal_preop3, by = c("Surg_PatientID" = "PatientID"))



#I joined signal_preop with Actfast_proc_late by PatientID(825), PAN(1289), MRN(1674). Which combination is better?

num_overlap(signal_preop, actfast_proc_late)

signal_preop_proc_late<-signal_preop %>% inner_join(actfast_proc_late, by = c("Surg_PatientID" = "PatientID")) %>%
  filter( ICDX_PROCEDURE_CODE %in% c("0FT44ZZ","0DTN0ZZ", "0DB64Z3", "0FBG0ZZ", "0FBG3ZZ","0UT90ZZ", "0UB74ZZ", "0SG00A0", "0SRC0J9","0SRB04Z","0RRJ00Z","0DQ53ZZ","0FT20ZZ","0TT10ZZ","0VT08ZZ","0TTB0ZZ","B50W","51.23","45.8","43.82","52.7","52.0","68.4","68.5","68.9","68.49","81.06","81.54","81.51","81.80","53.9","32.41","32.49","55.4","60.5","57.71","39.52","39.53"))

signal_preop_proc_late1<-signal_preop %>% inner_join(actfast_proc_late, by = c("PAN" = "REG_NO")) %>%
  filter( ICDX_PROCEDURE_CODE %in% c("0FT44ZZ","0DTN0ZZ", "0DB64Z3", "0FBG0ZZ", "0FBG3ZZ","0UT90ZZ", "0UB74ZZ", "0SG00A0", "0SRC0J9","0SRB04Z","0RRJ00Z","0DQ53ZZ","0FT20ZZ","0TT10ZZ","0VT08ZZ","0TTB0ZZ","B50W","51.23","45.8","43.82","52.7","52.0","68.4","68.5","68.9","68.49","81.06","81.54","81.51","81.80","53.9","32.41","32.49","55.4","60.5","57.71","39.52","39.53"))

signal_preop_proc_late2<-signal_preop %>% inner_join(actfast_proc_late, by = "MRN" ) %>%
  filter( ICDX_PROCEDURE_CODE %in% c("0FT44ZZ","0DTN0ZZ", "0DB64Z3", "0FBG0ZZ", "0FBG3ZZ","0UT90ZZ", "0UB74ZZ", "0SG00A0", "0SRC0J9","0SRB04Z","0RRJ00Z","0DQ53ZZ","0FT20ZZ","0TT10ZZ","0VT08ZZ","0TTB0ZZ","B50W","51.23","45.8","43.82","52.7","52.0","68.4","68.5","68.9","68.49","81.06","81.54","81.51","81.80","53.9","32.41","32.49","55.4","60.5","57.71","39.52","39.53"))

# missing data b/w signal_preop_proc_late and signal_preop_proc_late1 is around 2 rows
missing_data1 <- signal_preop_proc_late %>% anti_join(signal_preop_proc_late1, by = c("Surg_PatientID" = "PatientID"))

#missing data b/w signal_preop_proc_late and signal_preop_proc_late2 is around 0 rows
missing_data2 <- signal_preop_proc_late %>% anti_join(signal_preop_proc_late2, by = c("Surg_PatientID" = "PatientID"))






