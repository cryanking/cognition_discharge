## docker run -v '/mnt/ris/lavanya/cognition_check/:/research/'  -v '/home/christopherking/gitdirs/cognition_discharge/:/code/' cryanking/cognitioncheck:1.1 R -f /code/merging_actdata_to_cog.R
## LSF_DOCKER_VOLUMES=' /storage1/fs1/christopherking/Active/lavanya/cognition_check/:/research/ /home/christopherking/cognition_discharge:/code' bsub -G 'compute-christopherking' -n 6 -R 'rusage[mem=32GB] span[hosts=1]' -M 32GB -q general -a 'docker(cryanking/cognitioncheck:1.1)' R -f /code/merging_actdata_to_cog.R


library(lubridate) 
library(readxl)
library(magrittr)
library(tidyverse)
library(dplyr) 
library(forcats)
library(readr)
library(splines)
library(nonnest2)
library(modelr)
library(purrr)
library(pROC)

## "Figure 1" data
##  -- all surgeries > 65 
##  -- distinct hospitalizations w / qualifying procedures
##  -- eval w/i 90 days
##  -- cog data present
##  -- admission data present -> 100%


figure1 <- data.frame(Stage=NULL, N=NULL, deltaN=NULL)

setwd("/research")
## NOTE: this does NOT work for readmission timing / length of stay because this file does not contain discharge times!
## it does contain ED visits and I think readmits
room_transfer <- read_csv("CDS ADT.csv", col_types = cols(REFERENCE_NO = col_character(), 
                                                    REG_NO = col_character(), FACILITY_CONCEPT_ID = col_character()))

admission_log <- read_csv("CDS Visits.csv" , col_types=cols( REFERENCE_NO = col_character(), PAN_AKA_REG_NO = col_character(), ADMIT_DATE=col_datetime(format="%Y-%m-%d-%H.%M.%S" ) , DISCHARGE_DATE=col_datetime("%Y-%m-%d-%H.%M.%S"), DISCHARGE_DISPOSITION = col_character()  ) )

## I don't think this file is useful compared to the new one
# old_admission_log <- read_csv("2020_02_MV_LoS.csv", 
#                             col_types = cols(PatientID = col_character(), 
#                                              PAN = col_character(),
#                                              EMPI = col_character(), 
#                                              ADMIT_TMSTP = col_datetime(),
#                                              DISCHARGE_TMSTP = col_datetime(),
#                                              LoS_Days = col_double()
#                                              ), na = "NULL")

# cpap_vs_surgery <- read_xlsx("/research/2020_01_King_ProcedureCodes_MostRecentCPAP.xlsx", sheet= "MostRecentCPAP",na = "NULL", col_types =c("text","date", "text", "date"))

Signals <- read_xlsx("2020_01_15_Gregory_Cognative_Dysfunction_Data.xlsx", 
                      sheet = "Signals", na = "NULL", 
                      col_types = c(
                        "text", 
                        "text", "date", "date", "date", 
                        "numeric", "numeric", "numeric", "numeric", 
                        "numeric", "numeric", "numeric", "numeric", "numeric", 
                        "numeric", "numeric", "numeric", "numeric", "numeric", 
                        "numeric", "numeric", "numeric", "numeric", "numeric", 
                        "numeric", "numeric", "numeric", "numeric", "numeric", 
                        "numeric", "numeric", "numeric", "numeric", "numeric", 
                        "numeric", "numeric", "numeric", "numeric", "numeric", 
                        "numeric", "numeric", "numeric", "numeric", "numeric", 
                        "numeric", "numeric", "numeric", "numeric", "numeric", 
                        "numeric", "numeric", "numeric", "numeric", "numeric", 
                        "numeric", "numeric", "numeric", "numeric", "numeric", 
                        "numeric", "numeric", "numeric"))

TextSignals <- read_xlsx("2020_01_15_Gregory_Cognative_Dysfunction_Data.xlsx", 
                          sheet = "TextSignals",na = "NULL", 
                          col_types = c(
                            "text", 
                            "numeric", "numeric", "numeric", "numeric", 
                            "numeric", "numeric", "numeric", "numeric", "numeric", 
                            "numeric", "numeric", "numeric", "numeric", "numeric", 
                            "numeric", "numeric", "numeric", "numeric", "numeric", 
                            "numeric", "numeric", "numeric", "numeric", "numeric", 
                            "numeric", "numeric", "numeric", "numeric", "numeric"))


TextSignals %<>% select(one_of("CPAPPatientID", "sex", "Race", "Ethnicity", "Alcohol use", "Drinks/week", "Functional capacity", "Dialysis history", "Cirrhosis etiology", "Surgical Service") )

TextSignals %<>% (janitor::clean_names)
TextSignals$race %<>% as.factor %>% fct_other(keep=c("7","9")) %>% fct_recode(White="9", Black="7") %>% fct_explicit_na("Other") %>% relevel(ref="White") 

## the base package ifelse doesn't check type consistency, which is a frequent source of difficult to detect bugs. use if_else (in tidyverse or fifelse in data.table)
TextSignals %<>% mutate(current_heavy = if_else(sex==1, drinks_week > 16, drinks_week >10)%>% as.factor %>% fct_explicit_na ) %>% select(-one_of("drinks_week"))
TextSignals$alcohol_use %<>% is_in(4:6) %>% as.factor %>% fct_explicit_na
TextSignals$dialysis_history %<>% is_in(76:80)%>% as.factor %>% fct_explicit_na
TextSignals$functional_capacity %<>% is_in(9:11)%>% as.factor %>% fct_explicit_na
TextSignals$cirrhosis_etiology %<>% is_in(107:113)%>% as.factor %>% fct_explicit_na


TextSignals %<>% rename(prior_heavy_alcohol=alcohol_use, low_functional_capacity=functional_capacity, cirrhosis=cirrhosis_etiology)

Signals %<>% left_join(TextSignals  , by=c("CPAPPatientID"="cpap_patient_id")) 

Signals %<>% rename(CAD=`Coronary artery disease`, CKD=`Chronic kidney disease`)



## included procedure lists and categories
# included_proc_codes <- c("0FT44ZZ","0DTN0ZZ", "0DB64Z3", "0FBG0ZZ", "0FBG3ZZ","0UT90ZZ", "0UB74ZZ", "0SG00A0", "0SRC0J9","0SRB04Z","0RRJ00Z","0DQ53ZZ","0FT20ZZ","0TT10ZZ","0VT08ZZ","0TTB0ZZ","B50W","51.23","45.8","43.82","52.7","52.0","68.4","68.5","68.9","68.49","81.06","81.54","81.51","81.80","53.9","32.41","32.49","55.4","60.5","57.71","39.52","39.53")

# make_surg_categories <- function(ICD_PROCEDURE_CODE) {
# case_when(
#   ICD_PROCEDURE_CODE == "51.23" | ICD_PROCEDURE_CODE == "0FT44ZZ" ~ 'cholecystectomy', 
#   ICD_PROCEDURE_CODE == "45.8" | ICD_PROCEDURE_CODE == "0DTN0ZZ" ~ 'Colectomy', 
#   ICD_PROCEDURE_CODE == "43.82" | ICD_PROCEDURE_CODE == "0DB64Z3" ~ 'Gastrectomy',
#   ICD_PROCEDURE_CODE == "52.7" | ICD_PROCEDURE_CODE == "0FBG0ZZ" ~ 'whipple',
#   ICD_PROCEDURE_CODE == "68.4" | ICD_PROCEDURE_CODE == "0UT90ZZ" |ICD_PROCEDURE_CODE == "68.5"|ICD_PROCEDURE_CODE =="68.9" ~ 'Hysterectomy',
#   ICD_PROCEDURE_CODE == "68.49" | ICD_PROCEDURE_CODE == "0UB74ZZ" ~ 'Hysterectomy',
#   ICD_PROCEDURE_CODE == "52.0" | ICD_PROCEDURE_CODE ==  "0FBG3ZZ" ~ 'pancreatectomy',
#   ICD_PROCEDURE_CODE == "81.06" | ICD_PROCEDURE_CODE == "0SG00A0" ~ 'lumbarFusion',
#   ICD_PROCEDURE_CODE == "81.54" | ICD_PROCEDURE_CODE == "0SRC0J9" ~ 'totalKneeArthropathy',
#   ICD_PROCEDURE_CODE == "81.51" | ICD_PROCEDURE_CODE == "0SRB04Z" ~ 'totalHip',
#   ICD_PROCEDURE_CODE == "81.80" | ICD_PROCEDURE_CODE == "0RRJ00Z" ~ 'totalShoulder',
#   ICD_PROCEDURE_CODE == "53.9" | ICD_PROCEDURE_CODE ==  "0DQ53ZZ" ~ 'laproscopicHiatalHernia',
#   ICD_PROCEDURE_CODE == "32.49" | ICD_PROCEDURE_CODE == "32.41" ~ 'lobectomy',
#   ICD_PROCEDURE_CODE == "55.4" | ICD_PROCEDURE_CODE == "0TT10ZZ" ~ 'Nephrectomy',
#   ICD_PROCEDURE_CODE == "60.5" | ICD_PROCEDURE_CODE == "0VT08ZZ" ~ 'Prostatectomy',
#   ICD_PROCEDURE_CODE == "57.71" | ICD_PROCEDURE_CODE == "0TTBOZZ" ~ 'Cystectomy',
#   ICD_PROCEDURE_CODE == "39.52" | ICD_PROCEDURE_CODE == "39.53" ~ 'Arteriovenous')
# }



## categorize some discharge locations
facility_list <- c(
  "Discharged/transferred to an inpatient rehabilitation facility including distinct part units of a hospital" , 
  "DSCH/TRNF TO SNF" , 
  "DSCH/TRNF TO AN IP REHAB FACILITY" , 
  "DSCH/TRNF TO A LONG TERM CARE HOSPITAL" , 
  "Discharged/transferred to long term care hospitals" , 
  "Discharged/transferred to an Intermediate Care Facility" , 
  "Discharged/transferred another short-term general hospital" , 
  "DSCH/TRNF TO A CUSTODIAL FACILITY" , 
  "Discharged/transferred to a Federal hospital (effective for discharges after October 1, 2003)" , 
  "DSCH/TRNF TO A SHORT TERM HOSPITAL FOR IP" , 
  "DSCH/TRNF TO A NURSING FACILITY MEDICAID CERTIFIED" , 
  "DSCH/TRNF TO OTHER REHAB WITH PLAN READMIT" , 
  "DSCH/TRNF TO A FED HEATLH CARE FACILITY" , 
  "Discharged/transferred to another type of health care institution not defined elsewhere in the code list.",
  "Discharged/transferred to Skilled Nursing Facility",
  "Discharged/transferred to a federal health care facility with a planned acute care hospital inpatient readmission",
  "Discharged/Transfer to Skilled Nursing Facility" ,
  "Discharge to Rehab Facility/Unit"  ,
  "Discharge to Psychiatric Hospital/Unit" ,
"Discharged/Transfer to Other Facility" , 
"Discharged/Transfer to Short Term Hospital" , 
"Discharged/transferred to an inpatient rehabilitation facility (IRF) including rehabilitation distinct part units of a hospital wi" , 
"Discharged/transferred to a nursing facility certified under Medicaid but not certified under Medicare with a planned acute care h" , 
"Discharge to Nursing Facility Certified by Medicaid" , 
"Discharged/Transfer to Federal Hospital" , 
"Discharged/transferred to a short term general hospital for inpatient care with a planned acute care hospital inpatient readmissio" , 
"Discharged/Transfer to Intermediate Care Facility" , 
"Discharged/transferred to a Medicare certified long term care hospital (LTCH) with a planned acute care hospital inpatient readmis" , 
"Discharged from Acute Care to Inpt Long Term Care" 
  )

home_list <-c(
  "Discharge to home or self care (routine discharge)",
  "Discharged/transferred to home - organized home health service",
  "DSCH/TRNF TO HOME-HOME HEALTH SKILLED CARE",
  "DSCH/TRNF TO HOME W/ HOME HEALTH CARE WITH PLAN READMIT",
  "Discharge to home or self care",
  "DISCHARGE TO HOME OR SELF CARE" ,
  "Discharged/Transfer with Home Health Services",  
  "Discharge to Home (Includes Boarding Home)" ,
  "Discharge AMA" ,
 "Left Without Being Seen" , 
 "Discharged to home or self-care with a planned acute care hospital inpatient" , 
 "Discharged/transferred to home under care of organized home health service organization with a planned acute care hospital inpatie" , 
 "23-HOUR ADMISSION"
)  

all_dc_locations <- admission_log$DISCHARGE_DISPOSITION %>% unique

death_list <- all_dc_locations %>% grep( pattern="hospice", ignore.case=T, value=T) 
death_list <- all_dc_locations %>% grep( pattern="expired", ignore.case=T, value=T) %>% c(death_list)

# setdiff(all_dc_locations , c(home_list, facility_list, death_list) )



## cross-walk CPAP patient IDs to surgery patient IDs
CPAP_mapping<- read_csv( file="CPAP_to_surg.csv", col_types="cccccT"  )

## because I will be linking some CPAP-ID based data, attach the surgery data to a CPAP assessment to later validate
CPAP_mapping %<>% group_by(Surg_PatientID) %>% mutate(AnestStart = max(DoS[Data_PatientID==Surg_PatientID]) ) %>% ungroup

## repair some missing PAN - first match EMPI formatting
CPAP_mapping$EMPI %<>% stringr::str_pad(string=., width=24, side="left", pad="0")
## uncommonly EMPI (and MRN) are "made up" at CPAP clinic if a patient has never been seen before. However, the billing number link seems to work
## MRN is at least not missing
# CPAP_mapping %<>% group_by(MRN) %>% arrange(DoS) %>% mutate(EMPI = last(EMPI)) %>% ungroup
CPAP_mapping %<>% group_by(PAN) %>% arrange(DoS) %>% mutate(EMPI = if_else(!is.na(PAN) , last(EMPI), EMPI) )%>% ungroup

## the visit log has a lot of cancelled / erronious inputs
admission_log %<>% filter(difftime(ADMIT_DATE, date(ADMIT_DATE), units="mins") %>% as.numeric %>% abs %>% dplyr::between(1.5, 1438.5)  )
admission_log %<>% filter(difftime(DISCHARGE_DATE, date(DISCHARGE_DATE), units="mins") %>% as.numeric %>% abs %>% dplyr::between(1.5, 1438.5)  )
admission_log %<>% filter(DISCHARGE_DISPOSITION!="None recorded")
admission_log %<>% filter(FACILITY_INITIALS=="BJH")

CPAP_mapping %>% filter(PAN %>% is.na) %>%filter(Data_PatientID==Surg_PatientID) %>% inner_join(admission_log, by=c("EMPI"="REFERENCE_NO") , na_matches = "never") %>% 
  filter(data.table::between(DoS,ADMIT_DATE, DISCHARGE_DATE ,NAbounds=NA) ) ->rematched_data

## there are occasional overlapping visits where someone corrects an input by creating a new visit - take the shortest
rematched_data %<>% mutate(los = difftime(ADMIT_DATE, DISCHARGE_DATE, units="mins") %>% as.numeric %>% abs ) %>% group_by(Surg_PatientID) %>% slice_min(order_by=los) %>% ungroup
CPAP_mapping %<>% left_join(rematched_data %>% select(PAN2=PAN_AKA_REG_NO, Surg_PatientID), by="Surg_PatientID", na_matches = "never") %>% mutate(PAN = coalesce(PAN, PAN2)) %>% select(-PAN2)
rm(rematched_data)

CPAP_mapping %>% select(-Data_PatientID, -DoS) %>% distinct %>% left_join(CPAP_mapping %>% select(Data_PatientID, EMPI, DoS) %>% distinct , by="EMPI") %>% filter(data.table::between(DoS,AnestStart-lubridate::ddays(90), AnestStart, NAbounds=NA)) ->rematched_data

## this is the output of alternative_procedures.R, which operates in the ACT2 environment and contains Actfast_proc2, actfast_proc_late, procedure_data are all PAN based linked to Surg_PatientID
load(file="matched_proc_codes.rdata") 
all_codes <- unique(c(unique(actfast_proc_late$ICDX_PROCEDURE_CODE) , unique(Actfast_proc2$ICDX_PROCEDURE_CODE)  ) )

code_patterns <- list(
gut_codes = c("^0D[BTVD5][89ABEFGHKLMNPQ]" , "45\\.[6789]", "^46\\.9[34]", "^48\\.[456]" )
, stomach_codes = c("^0D[1BVTD5]6" , "^43\\.[56789]")
, chole_codes = c("^0F[DT]44" , "51\\.2[34]")
, panc_codes = c("^0F[BT]G", "^52\\.[256]", "52\\.7")
, hyster_codes = c("^0U[TD][4579]" , "^68\\.[345679]")
, lumbar_codes = c("^0SG[01]" ,"^81\\.0[678]" , "81\\.3[678]")
, shoulder_codes = c("^0RR[JK]" , "81\\.8[08]")
, hiatalHernia_codes = c("^0BU[RST][34]" , "53\\.71")
, knee_codes =  c("^0S[UR][CD]", "81\\.54")
, totalHip_codes = c("^0SR[9B]" ,"81\\.51")
, neph_codes = c( "^0TT[012]", "^55\\.[45]")
, prost_codes = c("^0VT0" ,"^60\\.[2346]")
, bladder_codes = c("^0TTB", "^57\\.[67]")
# , ueavfist_codes = c("39\\.42" ,  "39\\.53" , "39\\.27" , "39\\.29" , "^031[345678569ABC]0[A-Z0-9][DF]")
, vats_codes = c("0BTC4ZZ" , "0BTD4ZZ" , "0BTF4ZZ" , "0BTG4ZZ" , "0BTH4ZZ" , "0BTJ4ZZ" , "0BTK4ZZ" , "0BTL4ZZ" , "32\\.20" , "32\\.24" , "32\\.25" , "32\\.41", "0BB[CDFGHJKLNP][348]Z[XZ]","0BT[CDFGHJK]4Z[XZ]" )
)

pretty_names <- c("intestinal", "gastric", "cholecystectomy", "pancreatic", "hysterectomy", "lumbar fusion", "total shoulder", "lap hiatal hernia", "total knee", "total hip", "nephrectomy", "prostatectomy", "cystectomy", "VATS" )

pretty_names <- cbind(pretty_names , names(code_patterns) %>% sub(pattern="_codes", replacement="") %>%paste0("SType_", . )  ) %>% set_colnames(c("pretty_name", "SurgeryType"))

swap_pretty_names <- . %>% left_join(pretty_names%>% as_tibble, by="SurgeryType") %>% select(-SurgeryType) %>% rename(SurgeryType=pretty_name) %>% select(SurgeryType, everything() )

observed_codes <- lapply(code_patterns, function(y) unique(unname(unlist(sapply(y, function(x) grep(pattern=x, all_codes, value=T)) )  ) ))
included_proc_codes <- unlist(observed_codes)

## to avoid ugly loops in R, match procedure codes to the total list of codes [chmatch is an optimized version of match() for characters], then compare that position to the length of the sets
make_surg_categories <- function(ICD_PROCEDURE_CODE) { data.table::chmatch(ICD_PROCEDURE_CODE, included_proc_codes ) %>% cut( breaks = c(0L,cumsum(sapply(observed_codes, length) ) )) %>% lvls_revalue( c(names(observed_codes) %>% sub(pattern="_codes", replacement="") ) ) }


## for each set, scan the matching procedures within 45 days of the signals eval

## candidates for figure 1 "surgeries age > 65"
figure1 <- data.frame(Stage="surgeries age > 65", N=Signals %>% nrow, deltaN=NA_real_)

## age filter is a non-op, but many don't have AD8 or ABT
# Signals %<>% filter(Age_at_CPAP >= 65)   %>%  filter(!(is.na(AD8) & is.na(SBT) ) )
  
######## oldest (CLORE) procedure data
## 585 within 90 days
actfast_proc_late_filtered <- actfast_proc_late  %>% filter( ICDX_PROCEDURE_CODE %in% included_proc_codes) %>% select(PatientID, ICDX_PROCEDURE_CODE) %>% inner_join(rematched_data, by =  c("PatientID" = "Surg_PatientID" ), na_matches = "never")
actfast_proc_late_filtered %<>% inner_join(Signals, by =  c("Data_PatientID"   = "CPAPPatientID"), na_matches = "never")

# actfast_proc_late_filtered$PatientID %>% n_distinct

## other "half" of the data set: 2577 so total 3162
## double check direct PAN matching might find more events; the one below was the PAN -> PatientID done by I2
## it turns out to make no practical difference - there are two uniquely detected cases each direction, not worth figuring out
if(FALSE) {
actfast_proc_early_filtered <- Actfast_proc2 %>% filter( ICDX_PROCEDURE_CODE %in% included_proc_codes) %>% select(PAN, ICDX_PROCEDURE_CODE) %>% left_join(rematched_data , by="PAN", na_matches = "never" )
actfast_proc_early_filtered %<>% inner_join(Signals, by =  c("Data_PatientID"   = "CPAPPatientID"), na_matches = "never")
actfast_proc_early_filtered %<>% filter(CPAP_Date > AnestStart - lubridate::ddays(90))
actfast_proc_early_filtered %<>% filter( difftime(CPAP_Date, DoS, units="days") %>% as.numeric %>% abs %>% is_less_than(1) )
actfast_proc_early_filtered$Surg_PatientID %>% unique -> temp ##setdiff vs below
}

actfast_proc_early_filtered <- Actfast_proc2 %>% filter( ICDX_PROCEDURE_CODE %in% included_proc_codes) %>% select(PatientID, ICDX_PROCEDURE_CODE) %>% inner_join(CPAP_mapping, by =  c("PatientID" = "Surg_PatientID" ), na_matches = "never")
actfast_proc_early_filtered %<>% inner_join(Signals, by =  c("Data_PatientID"   = "CPAPPatientID"), na_matches = "never")



## there are a negligible number of failed PANs, just delete them
actfast_proc_early_filtered %<>% filter(!is.na(PAN))
actfast_proc_late_filtered %<>% filter(!is.na(PAN))

actfast_proc_filtered <- bind_rows(actfast_proc_late_filtered,actfast_proc_early_filtered)


figure1 <- bind_rows(figure1 , data.frame(Stage="distinct hospitalizations w / qualifying procedures", N=actfast_proc_filtered$PAN %>% n_distinct, deltaN=NA_real_) )


actfast_proc_filtered %<>% filter(CPAP_Date > AnestStart - lubridate::ddays(90))

actfast_proc_filtered %<>% filter( difftime(CPAP_Date, DoS, units="days") %>% as.numeric %>% abs %>% is_less_than(1) )


actfast_proc_filtered %>% select(PatientID, EMPI, AD8, SBT, Barthel, ethnicity ) %>% write_csv("/research/re_export_mv_asses.csv")


figure1 <- bind_rows(figure1 , data.frame(Stage="preop eval w/i 90 days", N=actfast_proc_filtered$PAN %>% n_distinct, deltaN=NA_real_) )

actfast_proc_filtered %<>% filter( is.na(AD8) + is.na(SBT) < 2)

figure1 <- bind_rows(figure1 , data.frame(Stage="cognitive screen present", N=actfast_proc_filtered$PAN %>% n_distinct, deltaN=NA_real_) )



######## newer (ACT3) data -> 1811 cases
## not used, this set is smaller because it did not include patients who were not eligible for ACT3 by virtue of the tower not being staffed
if(FALSE) {
  procedure_data_filtered <- procedure_data  %>% filter( ICDX_PROCEDURE_CODE %in% included_proc_codes) %>% select(REG_NO, PatientID, ICDX_PROCEDURE_CODE) %>%  inner_join(CPAP_mapping, by =  c("PatientID" = "Surg_PatientID" ))
  procedure_data_filtered %<>% inner_join(Signals, by =  c("Data_PatientID"   = "CPAPPatientID"))
  procedure_data_filtered %<>% filter(CPAP_Date > AnestStart - lubridate::ddays(90))
}

## note that these are not completely distinct but look to be non-unique in acceptable ways
## - single surgery with multiple qualifying billing codes (e.g. hysterectomy + colon rsxn)
## - 2x CPAP within the window for a qualifying surgery with a NON qualifying surgery in between
## - multiple surgeries within the same hospitalization (which share the same billing code set)
## Resolution: 
## (1) take last CPAP values with same PID 
## (2) make hospitalization the unit of analysis, since that is the thing capable of having a readmit / dischage
## (3) include all PID and code combos -> note limitation

## try to subset to unique admissions


# setequal(actfast_proc_late_filtered %>% colnames,  actfast_proc_early_filtered %>% colnames) # TRUE
# actfast_proc_filtered %<>%  group_by(PatientID) %>% mutate( anymiss=all(is.na(AD8)) | all(is.na(SBT)) ) %>% ungroup %>% filter(anymiss==FALSE) %>% select(-anymiss)
actfast_proc_filtered %<>% group_by(PatientID) %>% arrange(DoS) %>% mutate(AD8= AD8 %>% na.omit %>% last, SBT=SBT%>% na.omit %>% last ) %>% slice_tail(n=1) %>% ungroup

# ## old way of doing slice instead of coalesce, only matters a few times
# actfast_proc_filtered %<>% group_by(PatientID) %>% slice_max(order_by=DoS, n=1, with_ties=FALSE) %>% ungroup

########### POD 4 patients
## load them separately since the outcome set will be smaller
## because they are not in my other dataset (or rather it is much more complicated to link them), don't bother coalesce
## this turns out to never be useful - for pod 4 cases only 6 qualify
if(FALSE) {
new_proc_codes <- read_xlsx("2020_01_15_Gregory_Cognative_Dysfunction_Data.xlsx", 
                             sheet = "ProcedureCodes",na = "NULL", col_types = c("text", "text", "text", "numeric", "text"))


new_discharge <- read_xlsx("2020_01_15_Gregory_Cognative_Dysfunction_Data.xlsx", 
                             sheet = "Other", na = "NULL", col_types = c("text", "text", "numeric"))

signals_procedure <- Signals %>%  filter(is.na(AD8) | is.na(SBT)  ) %>% inner_join(new_proc_codes %>% filter(ICD_CODE_VERSION == "9-CM" | ICD_CODE_VERSION =="10-PCS") %>% filter( ICD_PROCEDURE_CODE %in% included_proc_codes) ) 
# %>% distinct(CPAPPatientID, .keep_all=T), by="CPAPPatientID") ## want to keep multiple surgery types for now

## eliminate overlap - annoying because the new pull includes only the CPAP event ID, but does have the surgery date
## is overlap iff CPAP even appears in linker file (meaning that a surgery was included) AND the surgery date is the same
signals_procedure %>% inner_join(CPAP_mapping , by=c("CPAPPatientID"="Data_PatientID") ) %>% filter( lubridate::date(AnestStart) == lubridate::date(Surgery_Date) ) -> overlap_temp
signals_procedure %<>% anti_join(overlap_temp, by="CPAPPatientID" )  
rm(overlap_temp)

signals_procedure %<>%  mutate(SurgeryType = make_surg_categories(ICD_PROCEDURE_CODE) )

pod4_discharge <- read_xlsx("2020_01_15_Gregory_Cognative_Dysfunction_Data.xlsx", 
                    sheet = "Other", na = "NULL", col_types = c("text", 
                                                                "text", "numeric"))

pod4_discharge %<>% mutate( dc_status = case_when( 
  is.na(UB_DISCHARGE_DISPOSITION_DESCRIPTION ) ~ "missing",
  UB_DISCHARGE_DISPOSITION_DESCRIPTION %in% death_list ~ "hospice or death",
  UB_DISCHARGE_DISPOSITION_DESCRIPTION %in% home_list  ~ "home" ,
  UB_DISCHARGE_DISPOSITION_DESCRIPTION %in% facility_list ~ "facility",
  TRUE ~ "other" ) )

other_than_home_pod4<- left_join(signals_procedure, pod4_discharge, by = "CPAPPatientID")

}
## there are only 47 such cases - just ignore them


actfast_proc_filtered %<>%  mutate(SurgeryType = make_surg_categories(ICDX_PROCEDURE_CODE) )

# ## these surgeries are too uncommon to do anything with
# actfast_proc_filtered %<>% filter(!(SurgeryType %in% c("Gastrectomy" , "laproscopicHiatalHernia" , "Arteriovenous") ) ) 


## replace NA with 0 in comorbidities and transform them to binary
na_zero <- function(x) {  replace_na(x,0) }
na_false <- function(x) {  replace_na(x,FALSE) }
# comborbid_vars <- c("Coronary artery disease", "Congestive heart failure", "Atrial fibrillation or flutter history" , "COPD" , "Asthma" , "Peripheral artery disease" , "Diabetes mellitus" , "Current cancer", "Cerebrovascular disease" , "Cerebrovascular disease, stroke, or TIA" , "CVA" , "TIA" ,"Hypertension")


comborbid_vars <- c("COPD" , "Congestive heart failure" , "Diabetes mellitus" , "Current cancer", "Cerebrovascular disease" , "Cerebrovascular disease, stroke, or TIA" , "CVA" , "TIA", "CAD", "CKD")

actfast_proc_filtered %<>%  mutate_at(vars(one_of(comborbid_vars)) , as.logical) %>%  mutate_at(vars(one_of(comborbid_vars)) , na_false) %>% mutate(CVA = `Cerebrovascular disease` | `Cerebrovascular disease, stroke, or TIA` | CVA | TIA) %>% mutate_at(vars(one_of( c("Hypertension", "CAD", "Atrial fibrillation or flutter history","CKD") )), na_false )

## these leave as categories
factor_vars <- c( "sex", "low_functional_capacity" )

## transform each hospitalization - 2733 hospitalizations

actfast_proc_filtered %<>% group_by(PAN) %>% mutate( AD8 = max(AD8, na.rm=TRUE) , SBT = max(SBT, na.rm=TRUE) ) %>% mutate_at(vars(one_of(comborbid_vars)), max)%>% mutate_at(vars(one_of(factor_vars)), last) %>% mutate( AbnCog =  case_when(
    is.na(AD8) & is.na(SBT) ~ NA ,
    is.na(AD8) ~ SBT>=5 ,
    is.na(SBT) ~ AD8>=2 ,
    TRUE ~ AD8>=2 | SBT>=5 ) ) %>% ungroup


encode_onehot <- function(x, colname_prefix = "", colname_suffix = "") {
  if (!is.factor(x)) {
      x <- as.factor(x)
  }
  encoding_matrix <- contrasts(x, contrasts = FALSE)
  encoded_data <- encoding_matrix[as.integer(x),]
  colnames(encoded_data) <- paste0(colname_prefix, colnames(encoded_data), colname_suffix)
  encoded_data
}

actfast_proc_filtered <- bind_cols(actfast_proc_filtered, encode_onehot(actfast_proc_filtered$SurgeryType, colname_prefix="SType_") %>% as_tibble )
actfast_proc_filtered$year <- format(actfast_proc_filtered$AnestStart, format= "%Y")
actfast_proc_filtered <- bind_cols(actfast_proc_filtered, encode_onehot(actfast_proc_filtered$year, colname_prefix = "year_") %>% as_tibble)

hosp_proc<- actfast_proc_filtered  %>% group_by(PAN) %>% mutate_at(vars(one_of("Age_at_CPAP"),starts_with("SType_") ), max)  %>%  slice_head( n=1 ) %>% ungroup

hosp_proc %<>% filter(!is.na(SurgeryType))

comborbid_vars <- setdiff(comborbid_vars, c("Cerebrovascular disease, stroke, or TIA", "TIA","Cerebrovascular disease" ))
hosp_proc %<>% rename_with(.fn= . %>% gsub(pattern=" ", replacement="_", fixed=T) %>% gsub(pattern=",", replacement="", fixed=T), .cols=one_of(comborbid_vars))

comborbid_vars %<>% gsub(pattern=" ", replacement="_", fixed=T) %>% gsub(pattern=",", replacement="", fixed=T)




######### Analysis 2
# Comparison of the incidence of the primary outcomes for the study between patients with an abnormal cognitive screen and those with a normal cognitive screen:
#     1. Discharge to a location other than home
#     2. Hospital readmission after discharge within 30 days

#### Discharge other than home
hosp_proc %<>% left_join(admission_log %>% select(PAN_AKA_REG_NO, DISCHARGE_DISPOSITION), by=c("PAN" = "PAN_AKA_REG_NO"), na_matches="never") %>% 
 mutate( dc_status = case_when( 
  is.na(DISCHARGE_DISPOSITION ) ~ "missing",
  DISCHARGE_DISPOSITION %in% death_list ~ "hospice or death",
  DISCHARGE_DISPOSITION %in% home_list  ~ "home" ,
  DISCHARGE_DISPOSITION %in% facility_list ~ "facility",
  TRUE ~ "other" ) )

## summarize each admission: take the worst SBT / AD8

hosp_proc %>% summarize(
  Abnormal_AD8 = mean(AD8>=2, na.rm=T), 
  NAD8 = sum(is.finite(AD8)), 
  Abnormal_SBT = mean(SBT>=5, na.rm=T), NSBT = sum(is.finite(SBT)), 
  AbnCog = mean( case_when(
    is.na(AD8) & is.na(SBT) ~ NA ,
    is.na(AD8) ~ SBT>=5 ,
    is.na(SBT) ~ AD8>=2 ,
    TRUE ~ AD8>=2 | SBT>=5 ) , na.rm=T),
  NAnyCog = sum(is.finite(AD8) | is.finite(SBT) )
)

#   Abnormal_AD8  NAD8 Abnormal_SBT  NSBT AbnCog NAnyCog
#          <dbl> <int>        <dbl> <int>  <dbl>   <int>
# 1       0.0776  5980        0.154  5980  0.201    5980


# hosp_proc %>% filter(!is.na(dc_status)) %>% summarize(n_distinct(PAN))


analysis_pipe <- . %>% mutate(thisout=dc_status!="home") %>% mutate(AbnCog= as.numeric(AbnCog)) %>% 
  glm(data=., formula=myform,  family=binomial() ) %>% 
  summary %>% extract2("coefficients") %>% as_tibble(rownames="rname") %>% filter(grepl(rname, pattern="AbnCog")) %>% select(-`z value`)

## surgery specific effects - build formulas externally because of the non-factor structure
## save these building blocks for various models
## year specific included
base_form <- "thisout ~ 0" %>% formula
surg_vars <- colnames(hosp_proc) %>% grep(pattern="SType_", value=T)
surg_form <- paste0(surg_vars, collapse=" + ")
surg_interact_form <- paste0(surg_vars,":AbnCog" ,  collapse=" + ")
comorbid_form <- paste0(c(comborbid_vars, factor_vars) ,  collapse=" + ")
year_vars <- colnames(hosp_proc) %>% grep(pattern = "year_", value=T)
year_form <- paste0(year_vars, collapse = " + ")
year_interact_form <- paste0(year_vars, ":AbnCog" , collapse=" + ")


myform <- base_form %>% 
  update( paste0("~.+", surg_form) ) %>% 
  update( paste0("~.+", surg_interact_form) ) 
  
hosp_proc %>% analysis_pipe 

## and examine the surgery specific offsets - these are all relative to 0 (50%)
myform <- base_form %>% 
  update( paste0("~.+", surg_form) )

hosp_proc %>% mutate(thisout=dc_status!="home") %>% mutate(AbnCog= as.numeric(AbnCog)) %>% glm(data=.,  formula=myform,
      , family=binomial()   ) %>% summary %>% extract2("coefficients") %>% as_tibble(rownames="rname") %>% filter(!grepl(rname, pattern="AbnCog")) %>% select(-`z value`)

## overall
myform <- base_form %>% 
  update( paste0("~.+", surg_form) ) %>%
  update( "~.+AbnCog" ) 

hosp_proc %>% analysis_pipe

## now adjust for age 
myform <- base_form %>% 
  update( paste0("~.+", surg_form) ) %>%
  update( "~.+AbnCog" ) %>%
  update( "~.+bs(Age_at_CPAP, 5)" ) 

  
hosp_proc %>% analysis_pipe

## adjusting for some common diseases 
myform <- base_form %>% 
  update( paste0("~.+", surg_form) ) %>%
  update( paste0("~.+", comorbid_form) ) %>%
  update( "~.+AbnCog" ) %>%
  update( "~.+bs(Age_at_CPAP, 5)" ) 

hosp_proc %>% analysis_pipe


## surgery-specific rates with adjustment
myform <- base_form %>% 
  update( paste0("~.+", surg_form) ) %>%
  update( paste0("~.+", surg_interact_form) ) %>%
  update( paste0("~.+", comorbid_form) ) %>%
  update( "~.+bs(Age_at_CPAP, 5)" ) 

hosp_proc %>% analysis_pipe

############
#### Readmission in 30 days
############
# readmit_set <- admission_log %>% select(PAN_AKA_REG_NO, DISCHARGE_DATE, REFERENCE_NO) %>% left_join(admission_log %>% select(REFERENCE_NO, ADMIT_DATE), by=c("REFERENCE_NO"), na_matches="never") %>%  filter( data.table::between(ADMIT_DATE, DISCHARGE_DATE, DISCHARGE_DATE+lubridate::ddays(30) ) ) 

room_transfer$ROOM_START_DATE %<>% ymd_hms

readmit_set <- admission_log %>% select(PAN_AKA_REG_NO, DISCHARGE_DATE, REFERENCE_NO) %>% left_join(room_transfer %>% select(REFERENCE_NO, ROOM_START_DATE), by=c("REFERENCE_NO"), na_matches="never") %>%  filter( data.table::between(ROOM_START_DATE, DISCHARGE_DATE, DISCHARGE_DATE+lubridate::ddays(360) , NAbounds=NA) ) 

## the bigger set doesn't help much
if(FALSE) {
plain_admission_log <- read_csv("CDS Visits.csv" , col_types=cols( REFERENCE_NO = col_character(), PAN_AKA_REG_NO = col_character(), ADMIT_DATE=col_datetime(format="%Y-%m-%d-%H.%M.%S" ) , DISCHARGE_DATE=col_datetime("%Y-%m-%d-%H.%M.%S"), DISCHARGE_DISPOSITION = col_character()  ) )

readmit_set <- plain_admission_log %>% select(PAN_AKA_REG_NO, DISCHARGE_DATE, REFERENCE_NO) %>% left_join(room_transfer %>% select(REFERENCE_NO, ROOM_START_DATE), by=c("REFERENCE_NO"), na_matches="never") %>%  filter( data.table::between(ROOM_START_DATE, DISCHARGE_DATE, DISCHARGE_DATE+lubridate::ddays(30) , NAbounds=NA) ) 
}

## ICU status while I am here
ICU_set <- room_transfer %>% filter(grepl(WARD, pattern="ICU" ) ) %>% pull("REG_NO") %>% unique


hosp_proc %<>% mutate( readmit = PAN %in%  unique(readmit_set$PAN_AKA_REG_NO ), ICU = PAN %in% ICU_set  )

## only 12%
hosp_proc %>%  pull("readmit") %>% table

# length(intersect(hosp_proc$EMPI,  admission_log$REFERENCE_NO ))
# length(intersect(hosp_proc$EMPI,  room_transfer$REFERENCE_NO ))

analysis_pipe <- . %>%filter(dc_status=="home") %>% mutate(thisout=readmit) %>% mutate(AbnCog= as.numeric(AbnCog)) %>% glm(data=., formula=myform,  family=binomial() ) %>% summary %>% extract2("coefficients") %>% as_tibble(rownames="rname") %>% filter(grepl(rname, pattern="AbnCog")) %>% select(-`z value`)

## overall - weak negative association
myform <- paste0( "thisout ~ 0 + ", paste0(surg_vars, collapse=" + "), " + AbnCog" ) %>% as.formula
hosp_proc %>% analysis_pipe 


## now adjust for age 
myform <- paste0( "thisout ~ 0 + ", paste0(surg_vars, collapse=" + "), " + AbnCog+ bs(Age_at_CPAP, 5)" ) %>% as.formula
hosp_proc %>% analysis_pipe

## and comorbid
myform <- base_form %>% 
  update( paste0("~.+", surg_form) ) %>%
  update( paste0("~.+", comorbid_form) ) %>%
  update( "~.+AbnCog" ) %>%
  update( "~.+bs(Age_at_CPAP, 5)" ) 

hosp_proc %>% analysis_pipe

## surgery specific rates - a few singularities, nothing really stands out
myform <- base_form %>% 
  update( paste0("~.+", surg_form) ) %>% 
  update( paste0("~.+", surg_interact_form) ) 
  
hosp_proc %>% analysis_pipe 


# Secondary outcomes: 
#     1. Hospital length-of-stay (time in days to discharge from time of index surgery)
#     2. In-hospital mortality (defined as death prior to discharge from the hospital)

## LOS
hosp_proc %<>% left_join(admission_log %>% select(PAN_AKA_REG_NO, DISCHARGE_DATE) , by= c("PAN"="PAN_AKA_REG_NO")) %>% mutate(LoS = difftime(DISCHARGE_DATE, Surgery_Date, units="days" )%>% as.numeric ) %>% mutate(LoS = if_else(LoS<.01, NA_real_, LoS  ))

analysis_pipe <- . %>% filter(dc_status=="home") %>% mutate(thisout=LoS) %>% mutate(AbnCog= as.numeric(AbnCog)) %>% glm(data=., formula=myform,  family=quasipoisson() ) %>% summary %>% extract2("coefficients") %>% as_tibble(rownames="rname") %>% filter(grepl(rname, pattern="AbnCog")) %>% select(-`t value`)

## overall - weak positive association
myform <- base_form %>% 
  update( paste0("~.+", surg_form) ) %>%
  update( "~.+AbnCog" ) 
hosp_proc  %>% analysis_pipe

## now adjust for age 
myform <- base_form %>% 
  update( paste0("~.+", surg_form) ) %>%
  update( "~.+AbnCog" ) %>%
  update( "~.+bs(Age_at_CPAP, 5)" ) 

hosp_proc %>% analysis_pipe

## adjust for comorbid
myform <- base_form %>% 
  update( paste0("~.+", surg_form) ) %>%
  update( paste0("~.+", comorbid_form) ) %>%
  update( "~.+AbnCog" ) %>%
  update( "~.+bs(Age_at_CPAP, 5)" ) 
hosp_proc %>% analysis_pipe


## adjusted surg-specific - a few are significant, but it's just multiple testing
myform <- base_form %>% 
  update( paste0("~.+", surg_form) ) %>%
  update( paste0("~.+", surg_interact_form) ) %>%
  update( paste0("~.+", comorbid_form) ) %>%
  update( "~.+bs(Age_at_CPAP, 5)" ) 
hosp_proc %>% analysis_pipe

## mortality - only 38
mortality_data <- read_csv("metav.csv" , col_types=cols( MPI = col_character(), DATE_OF_DEATH=col_datetime(format="%Y-%m-%d-%H.%M.%S" )   ) )

hosp_proc %<>% left_join( mortality_data , by = c("EMPI" = "MPI") )
## add a window for death for unrecognized hospice
## note that in R NA | TRUE is correctly TRUE
hosp_proc %<>% mutate(death = DATE_OF_DEATH < DISCHARGE_DATE + ddays(2) | DATE_OF_DEATH < Surgery_Date + ddays(30) | dc_status=="hospice or death")
hosp_proc %<>% mutate(death = if_else(is.na(death), FALSE, death ) )


analysis_pipe <- . %>% mutate(thisout=death) %>% mutate(AbnCog= as.numeric(AbnCog)) %>% glm(data=., formula=myform,  family=binomial() ) %>% summary %>% extract2("coefficients") %>% as_tibble(rownames="rname") %>% filter(grepl(rname, pattern="AbnCog")) %>% select(-`z value`)

## overall - no association
myform <- base_form %>% 
  update( paste0("~.+", surg_form) ) %>%
  update( "~.+AbnCog" ) 
hosp_proc  %>% analysis_pipe

## now adjust for age 
myform <- base_form %>% 
  update( paste0("~.+", surg_form) ) %>%
  update( "~.+AbnCog" ) %>%
  update( "~.+bs(Age_at_CPAP, 5)" ) 
hosp_proc %>% analysis_pipe

## adjust for comorbid
myform <- base_form %>% 
  update( paste0("~.+", surg_form) ) %>%
  update( paste0("~.+", comorbid_form) ) %>%
  update( "~.+AbnCog" ) %>%
  update( "~.+bs(Age_at_CPAP, 5)" ) 
hosp_proc %>% analysis_pipe


## adjusted surg-specific - blows up due to singularities
myform <- base_form %>% 
  update( paste0("~.+", surg_form) ) %>%
  update( paste0("~.+", surg_interact_form) ) %>%
  update( paste0("~.+", comorbid_form) ) %>%
  update( "~.+bs(Age_at_CPAP, 5)" ) 
hosp_proc %>% analysis_pipe

# Analysis 3:
#     1. Etiology of readmission in patients readmitted with abnormal cognitive screen vs those with a normal preoperative cognitive screen

##  I don't have an easy way to do analysis 3 right now

# Analysis 4:
#     1. Comparative predictive value of AD8 and SBT 
# some odd renaming to avoid having to modify other code
# there are two common ways to compare non-nested models for predictive value: vuong / clarke type tests and hold out samples

analysis_pipe_vu <- function(x) {
g1 <- x %>% mutate(thisout=dc_status!="home") %>% mutate(AbnCog= as.numeric(SBT >= 5)) %>% glm(data=., formula=myform,  family=binomial() ) 
g2 <- x %>% mutate(thisout=dc_status!="home") %>% mutate(AbnCog= as.numeric(AD8 >= 2)) %>% glm(data=., formula=myform,  family=binomial() ) 
vuongtest(g1, g2)
}

analysis_pipe_cv <- function(x) {

  x2 <- x %>% mutate(thisout=dc_status!="home")
    ## produce a shared set of cross-validation folds - note that dplyr / magrittr do not automatically know how to turn the fold-by-ref into a dataframe for manipulation, but they seems to have added a glm method
  rs <- modelr::crossv_kfold(x2, k=100)
  ## train the first model on all folds, then evaluate it on the hold out data using AUROC as a critereon - note the wrapper since at low frequency / sample size an evaluation set can have no events
  r1 <- map(rs$train, . %>% as.data.frame %>% mutate(AbnCog= as.numeric(SBT >= 5)) %>% glm(data=., formula=myform,  family=binomial() ) ) %>% 
    map2_dbl(rs$test, function(.x, .y){
      response <- .y %>% as.data.frame %>% pull("thisout")
      if(n_distinct(response) > 1 ) {
        pROC::roc(direction = "<" , response=response, levels=c(FALSE,TRUE), predictor=predict(.x , newdata=.y %>% as.data.frame %>% mutate(AbnCog= as.numeric(SBT >= 5)) )  ) %>% auc } else {NA_real_}
    } )

  r2 <- map(rs$train, . %>% as.data.frame %>% mutate(AbnCog= as.numeric(AD8 >= 2)) %>% glm(data=., formula=myform,  family=binomial() ) ) %>% map2_dbl(rs$test, function(.x, .y){
    response <- .y %>% as.data.frame %>% pull("thisout")
    if(n_distinct(response) > 1 ) {
      pROC::roc(direction = "<" , response=response, levels=c(FALSE,TRUE), predictor=predict(.x , newdata=.y %>% as.data.frame %>% mutate(AbnCog= as.numeric(AD8 >= 2)) )  ) %>% auc } else {NA_real_}
  } )
  t.test(na.omit(r1), na.omit(r2) )
}

## subsampling will produce inconsistent age parameterizations and OOB errors
global_age_spline <- bs(hosp_proc$Age_at_CPAP, 3)

## overall - no difference either way
myform <- base_form %>% 
  update( paste0("~.+", surg_form) ) %>%
  update( "~.+AbnCog" ) 
hosp_proc  %>% analysis_pipe_vu
hosp_proc  %>% analysis_pipe_cv


## now adjust for age 
myform <- base_form %>% 
  update( paste0("~.+", surg_form) ) %>%
  update( "~.+AbnCog" ) %>%
  update( "~.+predict(global_age_spline,Age_at_CPAP)" ) 
hosp_proc  %>% analysis_pipe_vu
hosp_proc  %>% analysis_pipe_cv

## adjust for comorbid
myform <- base_form %>% 
  update( paste0("~.+", surg_form) ) %>%
  update( paste0("~.+", comorbid_form) ) %>%
  update( "~.+AbnCog" ) %>%
  update( "~.+predict(global_age_spline,Age_at_CPAP)" ) 
analysis_pipe_vu_output <- hosp_proc  %>% analysis_pipe_vu
analysis_pipe_cv_output <- hosp_proc  %>% analysis_pipe_cv


######### Analysis 1


## this analysis isn't repeated, so there is no need to save the underlying changes
# Analysis1table <- hosp_proc %>% 
# mutate(bmi_cats = cut(BMI, breaks=c(14, 18.5, 25, 30, 35, 40, 61), right=FALSE ), male_sex = sex==1) %>% 
# mutate(low_barthel = Barthel %>% is_less_than(100) %>% as.factor %>% fct_explicit_na ) %>%
# mutate(abn_cog = AbnCog %>% as.factor %>% fct_recode(abnl="TRUE",nl="FALSE")) %>%
# mutate_at(vars(one_of("Dialysis") ) , na_zero ) %>%
#   table1::table1(~Age_at_CPAP + race + male_sex + bmi_cats  + Coronary_artery_disease + low_barthel + Congestive_heart_failure + Atrial_fibrillation_or_flutter_history + CVA + COPD + Asthma + Peripheral_artery_disease + Diabetes_mellitus + Current_cancer+current_heavy+prior_heavy_alcohol+low_functional_capacity+cirrhosis+Dialysis|abn_cog, data=.)


myform <- base_form %>% 
  update( paste0("~.+", surg_form) ) %>%
  update( paste0("~.+", comorbid_form) ) %>%
  update( "~.+AbnCog" ) %>%
  update( "~.+bs(Age_at_CPAP, 5)" ) 

dc_home_glm <- hosp_proc %>% mutate(thisout=dc_status!="home") %>% mutate(AbnCog= as.numeric(AbnCog)) %>% 
  glm(data=., formula=myform,  family=binomial() ) 
  
readmit_glm  <- hosp_proc %>%filter(dc_status=="home") %>% mutate(thisout=readmit) %>% mutate(AbnCog= as.numeric(AbnCog)) %>% 
  glm(data=., formula=myform,  family=binomial() ) 

death_glm <- hosp_proc %>% mutate(thisout=death) %>% mutate(AbnCog= as.numeric(AbnCog)) %>% 
  glm(data=., formula=myform,  family=binomial() ) 
  
los_glm <- hosp_proc %>%filter %>% filter(dc_status=="home") %>% mutate(thisout=LoS) %>% mutate(AbnCog= as.numeric(AbnCog)) %>% glm(data=., formula=myform,  family=quasipoisson() )


coef_home <- dc_home_glm  %>% summary %>% extract2("coefficients") %>% as_tibble(rownames="rname") %>% filter(grepl(rname, pattern="AbnCog")) %>% select(-`z value`)

coef_readmit <-  readmit_glm %>% summary %>% extract2("coefficients") %>% as_tibble(rownames="rname") %>% filter(grepl(rname, pattern="AbnCog")) %>% select(-`z value`)

coef_death <- death_glm %>% summary %>% extract2("coefficients") %>% as_tibble(rownames="rname") %>% filter(grepl(rname, pattern="AbnCog")) %>% select(-`z value`)

coef_los <- los_glm %>% summary %>% extract2("coefficients") %>% as_tibble(rownames="rname") %>% filter(grepl(rname, pattern="AbnCog")) %>% select(-`t value`)

ci_pipe <- . %>%  confint %>% as_tibble(rownames="rname") %>% filter(grepl(rname, pattern="AbnCog")) %>% select(-rname) %>% as.vector %>% exp %>% round(2) %>% sprintf(fmt="%.2f") %>% paste(collapse=" to ")

ci_home <- dc_home_glm %>% ci_pipe
ci_readmit <- readmit_glm %>% ci_pipe
ci_death <- death_glm %>% ci_pipe
ci_los <- los_glm %>% ci_pipe

myform <- base_form %>% 
  update( paste0("~.+", surg_form) ) %>% 
  update( paste0("~.+", surg_interact_form) ) %>%
  update( paste0("~.+", comorbid_form) ) %>%
  update( "~.+bs(Age_at_CPAP, 5)" ) 
  

inter_glm <- hosp_proc %>% mutate(thisout=dc_status!="home") %>% mutate(AbnCog= as.numeric(AbnCog)) %>% 
  glm(data=., formula=myform,  family=binomial() ) 

point_inter <-   inter_glm %>% extract2("coefficients") %>% as_tibble(rownames="rname") %>% filter(grepl(rname, pattern="AbnCog")) %>% select(rname, value)
  
cis_inter <-inter_glm  %>%  confint %>% as_tibble(rownames="rname") %>% filter(grepl(rname, pattern="AbnCog"))

cis_inter %<>% mutate(SurgeryType =rname %>% sub(pattern=":.*", replacement="") ) %>% swap_pretty_names

point_inter <- point_inter[cis_inter%>% transmute(width=`97.5 %` - `2.5 %`) %>% unlist %>%order(decreasing=TRUE),]
cis_inter %<>% arrange( desc(`97.5 %` - `2.5 %` ) )
cis_inter$value <-  point_inter$value

temp <- dc_home_glm %>% confint %>% as_tibble(rownames="rname") %>% filter(grepl(rname, pattern="AbnCog")) %>% select(-rname) %>% as.vector

cis_inter %<>% bind_rows( tibble(SurgeryType = "Overall", `2.5 %` =temp[["2.5 %"]], `97.5 %`=temp[["97.5 %"]], value=coef_home[[2]] ))


png(file="forest_home_surgery.png", width=5, height=5, units="in", res=300)
par(mar=c(3,0,0,0))
plot(x=0, y=0, xlim=c(-6,3), ylim=c(-15, 0), type='n', axes=FALSE, ylab="", xlab="")

text(x=-5.9, y=-seq.int(nrow(cis_inter)) , labels = cis_inter$SurgeryType , pos=4)
# text(x=-15, y=0, labels="Months", pos=4)
# text(x=seq(from=0, to=60, by=6), y=0, labels=seq(from=0, to=60, by=6), pos=4)
abline(v=0)
abline(h=-.1)
text(x=-6, y=0.2, labels="Surgery type", pos=4)
text(x=-3, y=0.2, labels="more dc home", pos=4)
text(x=-0, y=0.2, labels="less dc home", pos=4)

points(x=cis_inter$value, y=-seq.int(nrow(cis_inter)), pch=19  )
arrows(y0=-seq.int(nrow(cis_inter)), y1=-seq.int(nrow(cis_inter)), x0=cis_inter[["2.5 %"]], x1=cis_inter[["97.5 %"]]  , length=0)

# text(x=-5.9, y=-(nrow(cis_inter)+1) , labels = "overall" , pos=4)
# points(x=coef_home[2], y=-(nrow(cis_inter)+1), pch=19 , col='red')
# arrows(y0=-(nrow(cis_inter)+1), y1=-(nrow(cis_inter)+1), x0=temp[["2.5 %"]], x1=temp[["97.5 %"]]  , length=0, col='red')
axis(1, at=log(c(.125, .25, .5, 1, 2, 4, 8 )), labels=c("1/8", "1/4", "1/2", "1", "2", "4", "8" )  , cex.axis=.9)
axis(1, at=-4, labels="odds-ratio", lwd=0)
dev.off()

anova(inter_glm, dc_home_glm, test="Rao") 

## exploratory clinical outcomes

clinical_outcomes <- read_csv("outcomes.csv")
id_links <- read_csv("mv_era_all_ids.csv", col_types=cols(PatientID=col_character(), DoS = col_datetime(),  DoB = col_datetime() , EMPI = col_character(), IDCode= col_character(), VisitIDCode = col_character(),AnestStop = col_datetime()  ))
clinical_outcomes %<>% inner_join(id_links, by="caseid", na_matches="never" ) %>% select(VisitIDCode, icu_status, Stroke, AKI_v2, AbnormalHeartRythmn, DoS, Pneumonia )

hosp_proc %<>% left_join(clinical_outcomes , by=c("PAN"="VisitIDCode" ) , na_matches="never" ) %>% group_by(PAN) %>% slice_min(order_by=DoS.y, n=1, with_ties=FALSE) %>% ungroup %>% rename(DoS=DoS.x) 

hosp_proc$icu_status  %>% table(useNA='a')
hosp_proc$Stroke  %>% table(useNA='a')
# hosp_proc$Pneumonia  %>% table(useNA='a')
hosp_proc$AbnormalHeartRythmn  %>% table(useNA='a')
hosp_proc$AKI_v2  %>% table(useNA='a')
# hosp_proc$ARF  %>% table(useNA='a')
# hosp_proc$SurgicalWoundInfection  %>% table(useNA='a')

myform <- base_form %>% 
  update( paste0("~.+", surg_form) ) %>%
  update( paste0("~.+", comorbid_form) ) %>%
  update( "~.+AbnCog" ) %>%
  update( "~.+predict(global_age_spline,Age_at_CPAP)" ) 

## ICU admission
analysis_pipe <- . %>% mutate(thisout=icu_status) %>% mutate(AbnCog= as.numeric(AbnCog)) %>% glm(data=., formula=myform,  family=binomial() ) %>% summary %>% extract2("coefficients") %>% as_tibble(rownames="rname") %>% filter(grepl(rname, pattern="AbnCog")) %>% select(-`z value`)
coef_ICU <- hosp_proc %>% analysis_pipe
coef_ICU <- coef_ICU %>% add_column(exploratory_outcomes= "ICU")                                                                                

## AKI
analysis_pipe <- . %>% mutate(thisout=AKI_v2) %>% mutate(AbnCog= as.numeric(AbnCog)) %>% glm(data=., formula=myform,  family=binomial() ) %>% summary %>% extract2("coefficients") %>% as_tibble(rownames="rname") %>% filter(grepl(rname, pattern="AbnCog")) %>% select(-`z value`)
coef_AKI <- hosp_proc %>% analysis_pipe
coef_AKI <- coef_AKI %>% add_column(exploratory_outcomes= "AKI")                                                                                 

## arrythmia
analysis_pipe <- . %>% mutate(thisout=AbnormalHeartRythmn) %>% mutate(AbnCog= as.numeric(AbnCog)) %>% glm(data=., formula=myform,  family=binomial() ) %>% summary %>% extract2("coefficients") %>% as_tibble(rownames="rname") %>% filter(grepl(rname, pattern="AbnCog")) %>% select(-`z value`)
coef_arrythmia <- hosp_proc %>% analysis_pipe
coef_arrythmia <- coef_arrythmia %>% add_column(exploratory_outcomes= "Arrythmia")                                                                                  

## stroke
analysis_pipe <- . %>% mutate(thisout=Stroke) %>% mutate(AbnCog= as.numeric(AbnCog)) %>% glm(data=., formula=myform,  family=binomial() ) %>% summary %>% extract2("coefficients") %>% as_tibble(rownames="rname") %>% filter(grepl(rname, pattern="AbnCog")) %>% select(-`z value`)
coef_stroke<- hosp_proc %>% analysis_pipe
coef_stroke <- coef_stroke %>% add_column(exploratory_outcomes= "Stroke")   
                                                                                
## PNA
analysis_pipe <- . %>% mutate(thisout=Pneumonia) %>% mutate(AbnCog= as.numeric(AbnCog)) %>% glm(data=., formula=myform,  family=binomial() ) %>% summary %>% extract2("coefficients") %>% as_tibble(rownames="rname") %>% filter(grepl(rname, pattern="AbnCog")) %>% select(-`z value`)
coef_Pna<- hosp_proc %>% analysis_pipe
coef_Pna <- coef_Pna %>% add_column(exploratory_outcomes= "Pneumonia")   
   
   
   # conference Intervel
ci_pipe <- . %>%  confint.default %>% as_tibble(rownames="rname") %>% filter(grepl(rname, pattern="AbnCog")) 
                                                                                
ICU_glm <- hosp_proc %>% mutate(thisout= icu_status) %>% mutate(AbnCog= as.numeric(AbnCog)) %>% glm(data=., formula=myform,  family=binomial() )
ci_ICU <- ICU_glm  %>% ci_pipe %>% mutate_if(is.numeric, round, digits = 3) %>% select(-"rname")
coef_ICU <- bind_cols(coef_ICU, ci_ICU) %>% relocate(exploratory_outcomes, .before = Estimate) 

AKI_glm <- hosp_proc %>% mutate(thisout= AKI_v2) %>% mutate(AbnCog= as.numeric(AbnCog)) %>% glm(data=., formula=myform,  family=binomial() )                                                                                
ci_AKI <- AKI_glm %>% ci_pipe %>% mutate_if(is.numeric, round, digits = 3)  %>% select(-"rname")
coef_AKI <- bind_cols(coef_AKI, ci_AKI) %>% relocate(exploratory_outcomes, .before = Estimate) 

AHR_glm <- hosp_proc %>% mutate(thisout= AbnormalHeartRythmn) %>% mutate(AbnCog= as.numeric(AbnCog)) %>% glm(data=., formula=myform,  family=binomial() )                                                                                
ci_AHR <- AHR_glm %>% ci_pipe %>% mutate_if(is.numeric, round, digits = 3)  %>% select(-"rname")
coef_arrythmia  <- bind_cols(coef_arrythmia, ci_AHR) %>% relocate(exploratory_outcomes, .before = Estimate) 

Stroke_glm <- hosp_proc %>% mutate(thisout= Stroke) %>% mutate(AbnCog= as.numeric(AbnCog)) %>% glm(data=., formula=myform,  family=binomial() )                                                                                
ci_Stroke <- Stroke_glm %>% ci_pipe %>% mutate_if(is.numeric, round, digits = 3)  %>% select(-"rname")
coef_stroke<- bind_cols(coef_stroke, ci_Stroke) %>% relocate(exploratory_outcomes, .before = Estimate) 

Pna_glm <- hosp_proc %>% mutate(thisout= Pneumonia) %>% mutate(AbnCog= as.numeric(AbnCog)) %>% glm(data=., formula=myform,  family=binomial() )                                                                                
ci_Pna <- Pna_glm %>% ci_pipe %>% mutate_if(is.numeric, round, digits = 3)  %>% select(-"rname")
coef_Pna<- bind_cols(coef_Pna, ci_Pna) %>% relocate(exploratory_outcomes, .before = Estimate) 

exploratory_outcomes_glm <- bind_rows(coef_ICU, coef_AKI, coef_arrythmia , coef_stroke , coef_Pna )
exploratory_outcomes_glm <- exploratory_outcomes_glm %>% select(-c("rname", "Std. Error"))  
exploratory_outcomes_glm[["Estimate"]] %<>% exp %>% round(2)
exploratory_outcomes_glm[["2.5 %"]] %<>% exp %>% round(2)
exploratory_outcomes_glm[["97.5 %"]] %<>% exp %>% round(2)
exploratory_outcomes_glm %<>% rename(`p val`= `Pr(>|z|)`)
exploratory_outcomes_glm[["p val"]] %<>%  round(3) %>% format.pval(eps=.001)                                                                                 




myform <- base_form %>% 
  update( paste0("~.+", year_form) ) %>%
  update( paste0("~.+", comorbid_form) ) %>%
  update( "~.+AbnCog" )


dc_home_glm_year <- hosp_proc %>% mutate(thisout=dc_status!="home") %>% mutate(AbnCog= as.numeric(AbnCog)) %>% glm(data=., formula=myform,  family=binomial() ) 
readmit_glm_year <- hosp_proc %>%filter(dc_status=="home") %>% mutate(thisout=readmit) %>% mutate(AbnCog= as.numeric(AbnCog)) %>% glm(data=., formula=myform,  family=binomial() ) 
death_glm_year <- hosp_proc %>% mutate(thisout=death) %>% mutate(AbnCog= as.numeric(AbnCog)) %>% glm(data=., formula=myform,  family=binomial() ) 
los_glm_year <- hosp_proc %>%filter %>% filter(dc_status=="home") %>% mutate(thisout=LoS) %>% mutate(AbnCog= as.numeric(AbnCog)) %>% glm(data=., formula=myform,  family=quasipoisson() )


coef_home_year <- dc_home_glm_year  %>% summary %>% extract2("coefficients") %>% as_tibble(rownames="rname") %>% filter(grepl(rname, pattern="AbnCog")) %>% select(-`z value`)
coef_readmit_year <-  readmit_glm_year %>% summary %>% extract2("coefficients") %>% as_tibble(rownames="rname") %>% filter(grepl(rname, pattern="AbnCog")) %>% select(-`z value`)
coef_death_year <- death_glm_year %>% summary %>% extract2("coefficients") %>% as_tibble(rownames="rname") %>% filter(grepl(rname, pattern="AbnCog")) %>% select(-`z value`)
coef_los_year <- los_glm_year %>% summary %>% extract2("coefficients") %>% as_tibble(rownames="rname") %>% filter(grepl(rname, pattern="AbnCog")) %>% select(-`t value`)

ci_pipe <- . %>%  confint.default %>% as_tibble(rownames="rname") %>% filter(grepl(rname, pattern="AbnCog")) %>% select(-rname) %>% as.vector %>% exp %>% round(2) %>% sprintf(fmt="%.2f") %>% paste(collapse=" to ")

ci_home_year <- dc_home_glm_year %>% ci_pipe
ci_readmit_year <- readmit_glm_year %>% ci_pipe
ci_death_year <- death_glm_year %>% ci_pipe
ci_los_year <- los_glm_year %>% ci_pipe




myform <- base_form %>% 
  update( paste0("~.+", year_form) ) %>% 
  update( paste0("~.+", year_interact_form) ) %>%
  update( paste0("~.+", comorbid_form) ) 
  

inter_glm_year <- hosp_proc %>% mutate(thisout=dc_status!="home") %>% mutate(AbnCog= as.numeric(AbnCog)) %>% glm(data=., formula=myform,  family=binomial() ) 

point_inter_year <-   inter_glm_year %>% extract2("coefficients") %>% as_tibble(rownames="rname") %>% filter(grepl(rname, pattern="AbnCog")) %>% select(rname, value)
  
cis_inter_year <- inter_glm_year  %>%  confint.default %>% as_tibble(rownames="rname") %>% filter(grepl(rname, pattern="AbnCog"))
cis_inter_year <- inner_join(cis_inter_year, point_inter_year %>% select(rname, value), by="rname")
cis_inter_year <- cis_inter_year %>% mutate( YEAR =rname %>% sub(pattern=":.*", replacement="") )
                                                                                

#point_inter_year <- point_inter_year[cis_inter_year%>% transmute(width=`97.5 %` - `2.5 %`) %>% unlist %>%order(decreasing=TRUE),]
#cis_inter_year %<>% arrange( desc(`97.5 %` - `2.5 %` ) )
cis_inter_year %<>% arrange(YEAR)                                                                              
                                                                           
temp1 <- dc_home_glm_year %>% confint.default %>% as_tibble(rownames="rname") %>% filter(grepl(rname, pattern="AbnCog")) %>% select(-rname) %>% as.vector

# cis_inter_year %<>%  bind_rows( data.frame(value=coef_home_year[2], `97.5 %`=temp1[["97.5 %"]], `2.5 %`=temp1[["2.5 %"]], YEAR="All Pre-Epic"  ) )
cis_inter_year %<>%  bind_rows( tibble(value=unlist(coef_home_year[2]), "97.5 %"=temp1[["97.5 %"]], "2.5 %"=temp1[["2.5 %"]], YEAR="All Pre-Epic"  ) )
                                                          
png(file="forest_home_year.png", width=5, height=5, units="in", res=300)
par(mar=c(3,0,0,0))
plot(x=0, y=0, xlim=c(-6,3), ylim=c(-7, 0.3), type='n', axes=FALSE, ylab="", xlab="")

text(x=-5.9, y=-seq.int(nrow(cis_inter_year)) , labels = cis_inter_year$YEAR %>% sub(pattern="year_", replacement="") , pos=4)
# text(x=-15, y=0, labels="Months", pos=4)
# text(x=seq(from=0, to=60, by=6), y=0, labels=seq(from=0, to=60, by=6), pos=4)
abline(v=0)
abline(h=-.1)
text(x=-6, y=0.2, labels="YEAR", pos=4)
text(x=-3, y=0.2, labels="more dc home", pos=4)
text(x=-0, y=0.2, labels="less dc home", pos=4)

points(x=cis_inter_year$value, y=-seq.int(nrow(cis_inter_year)), pch=19  )
arrows(y0=-seq.int(nrow(cis_inter_year)), y1=-seq.int(nrow(cis_inter_year)), x0=cis_inter_year[["2.5 %"]], x1=cis_inter_year[["97.5 %"]]  , length=0)

text(x=-5.9, y=-(nrow(cis_inter_year)+1) , labels = "overall" , pos=4)
points(x=coef_home_year[2], y=-(nrow(cis_inter_year)+1), pch=19 , col='red')
arrows(y0=-(nrow(cis_inter_year)+1), y1=-(nrow(cis_inter_year)+1), x0=temp1[["2.5 %"]], x1=temp1[["97.5 %"]]  , length=0, col='red')
axis(1, at=log(c(.125, .25, .5, 1, 2, 4, 8 )), labels=c("1/8", "1/4", "1/2", "1", "2", "4", "8" )  , cex.axis=.9)
axis(1, at=-4, labels="odds-ratio", lwd=0)
dev.off()




saveRDS(hosp_proc, "merged_data.RDS" )
save( file="cognition_cache.rda" ,
  figure1,
  global_age_spline ,
  dc_home_glm ,
  readmit_glm ,
  death_glm,
  los_glm,
  inter_glm, 
  coef_home,
  coef_readmit,
  coef_death,
  coef_los,
  ci_home ,
  ci_readmit ,
  ci_death ,
  ci_los ,
  comborbid_vars ,
  base_form ,
  surg_vars ,
  surg_form ,
  surg_interact_form ,
  comorbid_form ,
  pretty_names, 
  analysis_pipe_vu_output ,
  analysis_pipe_cv_output ,
  exploratory_outcomes_glm ,
  cis_inter, cis_inter_year
)


# > figure1
#                                                 Stage     N deltaN
# 1                                  surgeries age > 65 38005     NA
# 2 distinct hospitalizations w / qualifying procedures  8663     NA
# 3                              preop eval w/i 90 days  8544     NA
# 4                            cognitive screen present  6665     NA


