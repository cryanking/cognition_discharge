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

