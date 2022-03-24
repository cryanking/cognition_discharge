library(lubridate) 
library(readxl)
library(magrittr)
library(dplyr) 
library(forcats)
library(readr)
library(data.table)
library(splines)

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


## included procedure lists and categories
included_proc_codes <- c("0FT44ZZ","0DTN0ZZ", "0DB64Z3", "0FBG0ZZ", "0FBG3ZZ","0UT90ZZ", "0UB74ZZ", "0SG00A0", "0SRC0J9","0SRB04Z","0RRJ00Z","0DQ53ZZ","0FT20ZZ","0TT10ZZ","0VT08ZZ","0TTB0ZZ","B50W","51.23","45.8","43.82","52.7","52.0","68.4","68.5","68.9","68.49","81.06","81.54","81.51","81.80","53.9","32.41","32.49","55.4","60.5","57.71","39.52","39.53")

make_surg_categories <- function(ICD_PROCEDURE_CODE) {
case_when(
  ICD_PROCEDURE_CODE == "51.23" | ICD_PROCEDURE_CODE == "0FT44ZZ" ~ 'cholecystectomy', 
  ICD_PROCEDURE_CODE == "45.8" | ICD_PROCEDURE_CODE == "0DTN0ZZ" ~ 'Colectomy', 
  ICD_PROCEDURE_CODE == "43.82" | ICD_PROCEDURE_CODE == "0DB64Z3" ~ 'Gastrectomy',
  ICD_PROCEDURE_CODE == "52.7" | ICD_PROCEDURE_CODE == "0FBG0ZZ" ~ 'whipple',
  ICD_PROCEDURE_CODE == "68.4" | ICD_PROCEDURE_CODE == "0UT90ZZ" |ICD_PROCEDURE_CODE == "68.5"|ICD_PROCEDURE_CODE =="68.9" ~ 'Hysterectomy',
  ICD_PROCEDURE_CODE == "68.49" | ICD_PROCEDURE_CODE == "0UB74ZZ" ~ 'Hysterectomy',
  ICD_PROCEDURE_CODE == "52.0" | ICD_PROCEDURE_CODE ==  "0FBG3ZZ" ~ 'pancreatectomy',
  ICD_PROCEDURE_CODE == "81.06" | ICD_PROCEDURE_CODE == "0SG00A0" ~ 'lumbarFusion',
  ICD_PROCEDURE_CODE == "81.54" | ICD_PROCEDURE_CODE == "0SRC0J9" ~ 'totalKneeArthropathy',
  ICD_PROCEDURE_CODE == "81.51" | ICD_PROCEDURE_CODE == "0SRB04Z" ~ 'totalHip',
  ICD_PROCEDURE_CODE == "81.80" | ICD_PROCEDURE_CODE == "0RRJ00Z" ~ 'totalShoulder',
  ICD_PROCEDURE_CODE == "53.9" | ICD_PROCEDURE_CODE ==  "0DQ53ZZ" ~ 'laproscopicHiatalHernia',
  ICD_PROCEDURE_CODE == "32.49" | ICD_PROCEDURE_CODE == "32.41" ~ 'lobectomy',
  ICD_PROCEDURE_CODE == "55.4" | ICD_PROCEDURE_CODE == "0TT10ZZ" ~ 'Nephrectomy',
  ICD_PROCEDURE_CODE == "60.5" | ICD_PROCEDURE_CODE == "0VT08ZZ" ~ 'Prostatectomy',
  ICD_PROCEDURE_CODE == "57.71" | ICD_PROCEDURE_CODE == "0TTBOZZ" ~ 'Cystectomy',
  ICD_PROCEDURE_CODE == "39.52" | ICD_PROCEDURE_CODE == "39.53" ~ 'Arteriovenous')
}

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


## age filter is a non-op, but many don't have AD8 or ABT
Signals %<>% filter(Age_at_CPAP >= 65)   %>%  filter(!(is.na(AD8) & is.na(SBT) ) )
  
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



## for each set, scan the matching procedures within 45 days of the signals eval

######## oldest (CLORE) procedure data
## 585 within 90 days
actfast_proc_late_filtered <- actfast_proc_late  %>% filter( ICDX_PROCEDURE_CODE %in% included_proc_codes) %>% select(PatientID, ICDX_PROCEDURE_CODE) %>% inner_join(rematched_data, by =  c("PatientID" = "Surg_PatientID" ), na_matches = "never")
actfast_proc_late_filtered %<>% inner_join(Signals, by =  c("Data_PatientID"   = "CPAPPatientID"), na_matches = "never")
actfast_proc_late_filtered %<>% filter(CPAP_Date > AnestStart - lubridate::ddays(90))
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
actfast_proc_early_filtered %<>% filter(CPAP_Date > AnestStart - lubridate::ddays(90))
actfast_proc_early_filtered %<>% filter( difftime(CPAP_Date, DoS, units="days") %>% as.numeric %>% abs %>% is_less_than(1) )

## there are a negligible number of failed PANs, just delete them
actfast_proc_early_filtered %<>% filter(!is.na(PAN))
actfast_proc_late_filtered %<>% filter(!is.na(PAN))

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
actfast_proc_filtered <- bind_rows(actfast_proc_late_filtered,actfast_proc_early_filtered)
## 3145 PatientID
## coalesce to require at least one measurement of both traits in a window
actfast_proc_filtered %<>%  group_by(PatientID) %>% mutate( anymiss=all(is.na(AD8)) | all(is.na(SBT)) ) %>% ungroup %>% filter(anymiss==FALSE) %>% select(-anymiss)
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
## there are only 6 such cases - just ignore them


actfast_proc_filtered %<>%  mutate(SurgeryType = make_surg_categories(ICDX_PROCEDURE_CODE) )

## these surgeries are too uncommon to do anything with
actfast_proc_filtered %<>% filter(!(SurgeryType %in% c("Gastrectomy" , "laproscopicHiatalHernia" , "Arteriovenous") ) ) 


## replace NA with 0 in comorbidities and transform them to binary
na_zero <- function(x) {  if_else(is.na(x), 0, x) }
comborbid_vars <- c("Coronary artery disease", "Congestive heart failure", "Atrial fibrillation or flutter history" , "COPD" , "Asthma" , "Peripheral artery disease" , "Diabetes mellitus" , "Current cancer", "Cerebrovascular disease" , "Cerebrovascular disease, stroke, or TIA" , "CVA" , "TIA" ,"Hypertension")

actfast_proc_filtered %<>%  mutate_at(vars(one_of(comborbid_vars)) , na_zero) %>% mutate_at(vars(one_of(comborbid_vars)) , as.logical) %>% mutate(CVA = `Cerebrovascular disease` | `Cerebrovascular disease, stroke, or TIA` | CVA | TIA) 


## transform each hospitalization - 2733 hospitalizations

actfast_proc_filtered %<>% group_by(PAN) %>% mutate( AD8 = max(AD8, na.rm=TRUE) , SBT = max(SBT, na.rm=TRUE) ) %>% mutate_at(vars(one_of(comborbid_vars)), max) %>% mutate( AbnCog =  case_when(
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

hosp_proc<- actfast_proc_filtered  %>% group_by(PAN) %>% mutate_at(vars(one_of("Age_at_CPAP"),starts_with("SType_") ), max)  %>%  slice_head( n=1 ) %>% ungroup

comborbid_vars <- setdiff(comborbid_vars, c("Cerebrovascular disease, stroke, or TIA", "TIA","Cerebrovascular disease" ))
hosp_proc %<>% rename_with(.fn= . %>% gsub(pattern=" ", replacement="_", fixed=T) %>% gsub(pattern=",", replacement="", fixed=T), .cols=one_of(comborbid_vars))

comborbid_vars %<>% gsub(pattern=" ", replacement="_", fixed=T) %>% gsub(pattern=",", replacement="", fixed=T)


######### Analysis 1


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
# 1       0.0690  2768        0.140  2768  0.184    2768


analysis_pipe <- . %>% mutate(thisout=dc_status=="home") %>% mutate(AbnCog= as.numeric(AbnCog)) %>% glm(data=., formula=myform,  family=binomial() ) %>% summary %>% extract2("coefficients") %>% as_tibble(rownames="rname") %>% filter(grepl(rname, pattern="AbnCog")) %>% select(-`z value`)
## surgery specific effects - build formulas externally because of the non-factor structure
surg_vars <- colnames(hosp_proc) %>% grep(pattern="SType_", value=T)
myform <- paste0( "thisout ~ 0 + ", paste0(surg_vars, collapse=" + "), "+" , paste0(surg_vars,":AbnCog" ,  collapse=" + ") ) %>% as.formula
hosp_proc %>% analysis_pipe 

# actfast_proc_filtered %>% exclude_rare %>% glm(data=.,  dc_status=="home" ~ 0 + SurgeryType + SurgeryType:AbnCog
#       , family=binomial() ) %>% summary %>% extract2("coefficients") %>% as_tibble(rownames="rname") %>% filter(grepl(rname, pattern="AbnCog")) %>% select(-`z value`)

## and examine the surgery specific offsets - these are all relative to 0 (50%)
myform <- paste0( "thisout ~ 0 + ", paste0(surg_vars, collapse=" + ") ) %>% as.formula

hosp_proc %>% mutate(thisout=dc_status=="home") %>% mutate(AbnCog= as.numeric(AbnCog)) %>% glm(data=.,  formula=myform,
      , family=binomial()   ) %>% summary %>% extract2("coefficients") %>% as_tibble(rownames="rname") %>% filter(!grepl(rname, pattern="AbnCog")) %>% select(-`z value`)

# actfast_proc_filtered %>% exclude_rare %>% glm(data=.,  dc_status=="home" ~ 0 + SurgeryType 
#       , family=binomial() , contrasts=list(SurgeryType="contr.sum") ) %>% summary %>% extract2("coefficients") %>% as_tibble(rownames="rname") %>% filter(!grepl(rname, pattern="AbnCog")) %>% select(-`z value`)

## overall
myform <- paste0( "thisout ~ 0 + ", paste0(surg_vars, collapse=" + "), " + AbnCog" ) %>% as.formula
hosp_proc %>% analysis_pipe

# actfast_proc_filtered %>% exclude_rare %>% glm(data=.,  dc_status=="home" ~ 0 + SurgeryType + AbnCog
#       , family=binomial() , contrasts=list(SurgeryType="contr.sum") ) %>% summary %>% extract2("coefficients") %>% as_tibble(rownames="rname") %>% filter(grepl(rname, pattern="AbnCog")) %>% select(-`z value`)

## now adjust for age 
myform <- paste0( "thisout ~ 0 + ", paste0(surg_vars, collapse=" + "), " + AbnCog+ bs(Age_at_CPAP, 3)" ) %>% as.formula
hosp_proc %>% analysis_pipe

# actfast_proc_filtered %>% exclude_rare %>% glm(data=.,  dc_status=="home" ~ 0 + SurgeryType + AbnCog + bs(Age_at_CPAP, 3) 
#       , family=binomial() , contrasts=list(SurgeryType="contr.sum") ) %>% summary %>% extract2("coefficients") %>% as_tibble(rownames="rname") %>% filter(grepl(rname, pattern="AbnCog")) %>% select(-`z value`)

## adjusting for some common diseases - r complained about the length of the input to formula()
myform <- paste0( "thisout ~ 0 + ", paste0(surg_vars, collapse=" + ")) %>% as.formula
myform %<>% update(paste0( "~ . + AbnCog" )) 
myform %<>% update(paste0( "~ . + bs(Age_at_CPAP, 3) "))
myform %<>% update(paste0( "~ . + " , paste0(comborbid_vars ,  collapse=" + ") ) )

hosp_proc %>% analysis_pipe


# actfast_proc_filtered %>% exclude_rare %>% glm(data=.,  dc_status=="home" ~ 0 + SurgeryType + AbnCog + bs(Age_at_CPAP, 3) + Hypertension + `Coronary artery disease` + `Atrial fibrillation or flutter history` +  CVA + `Diabetes mellitus`
#       , family=binomial() , contrasts=list(SurgeryType="contr.sum") ) %>% summary %>% extract2("coefficients") %>% as_tibble(rownames="rname") %>% filter(grepl(rname, pattern="AbnCog")) %>% select(-`z value`)

## surgery-specific rates with adjustment
myform <- paste0( "thisout ~ 0 + ", paste0(surg_vars, collapse=" + ")) %>% as.formula
myform %<>% update(paste0( "~ . +" , paste0(surg_vars,":AbnCog" ,  collapse=" + ")) )
myform %<>% update(paste0( "~ . + bs(Age_at_CPAP, 3) "))
myform %<>% update(paste0( "~ . + " , paste0(comborbid_vars ,  collapse=" + ") ) )

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

readmit_set <- plain_admission_log %>% select(PAN_AKA_REG_NO, DISCHARGE_DATE, REFERENCE_NO) %>% left_join(room_transfer %>% select(REFERENCE_NO, ROOM_START_DATE), by=c("REFERENCE_NO"), na_matches="never") %>%  filter( data.table::between(ROOM_START_DATE, DISCHARGE_DATE, DISCHARGE_DATE+lubridate::ddays(0) , NAbounds=NA) ) 
}


hosp_proc %<>% mutate( readmit = PAN %in%  unique(readmit_set$PAN_AKA_REG_NO ) )

## only 42 (1.5%), is implausibly small compared to the global 7.5%. this dataset must not contain all readmissions
hosp_proc %>%  pull("readmit") %>% table

# length(intersect(hosp_proc$EMPI,  admission_log$REFERENCE_NO ))
# length(intersect(hosp_proc$EMPI,  room_transfer$REFERENCE_NO ))

analysis_pipe <- . %>%filter(dc_status=="home") %>% mutate(thisout=readmit) %>% mutate(AbnCog= as.numeric(AbnCog)) %>% glm(data=., formula=myform,  family=binomial() ) %>% summary %>% extract2("coefficients") %>% as_tibble(rownames="rname") %>% filter(grepl(rname, pattern="AbnCog")) %>% select(-`z value`)

## overall - negative association!
myform <- paste0( "thisout ~ 0 + ", paste0(surg_vars, collapse=" + "), " + AbnCog" ) %>% as.formula
hosp_proc %>% analysis_pipe 


## now adjust for age 
myform <- paste0( "thisout ~ 0 + ", paste0(surg_vars, collapse=" + "), " + AbnCog+ bs(Age_at_CPAP, 3)" ) %>% as.formula
hosp_proc %>% analysis_pipe

# Secondary outcomes: 
#     1. Hospital length-of-stay (time in days to discharge from time of index surgery)
#     2. In-hospital mortality (defined as death prior to discharge from the hospital)

## LOS
hosp_proc %<>% left_join(admission_log %>% select(PAN_AKA_REG_NO, DISCHARGE_DATE) , by= c("PAN"="PAN_AKA_REG_NO")) %>% mutate(LoS = difftime(DISCHARGE_DATE, Surgery_Date, units="days" )%>% as.numeric )

analysis_pipe <- . %>% filter(dc_status=="home") %>% mutate(thisout=LoS) %>% mutate(AbnCog= as.numeric(AbnCog)) %>% glm(data=., formula=myform,  family=quasipoisson() ) %>% summary %>% extract2("coefficients") %>% as_tibble(rownames="rname") %>% filter(grepl(rname, pattern="AbnCog")) %>% select(-`t value`)

## overall - no association
myform <- paste0( "thisout ~ 0 + ", paste0(surg_vars, collapse=" + "), " + AbnCog" ) %>% as.formula
hosp_proc  %>% analysis_pipe

## now adjust for age 
myform <- paste0( "thisout ~ 0 + ", paste0(surg_vars, collapse=" + "), " + AbnCog+ bs(Age_at_CPAP, 3)" ) %>% as.formula
hosp_proc %>% analysis_pipe

## adjust for comorbid
myform <- paste0( "thisout ~ 0 + ", paste0(surg_vars, collapse=" + ")) %>% as.formula
myform %<>% update(paste0( "~ . + AbnCog" )) 
myform %<>% update(paste0( "~ . + bs(Age_at_CPAP, 3) "))
myform %<>% update(paste0( "~ . + " , paste0(comborbid_vars ,  collapse=" + ") ) )
hosp_proc %>% analysis_pipe


## adjusted surg-specific - a few are significant, but it's just multiple testing
myform <- paste0( "thisout ~ 0 + ", paste0(surg_vars, collapse=" + ")) %>% as.formula
myform %<>% update(paste0( "~ . +" , paste0(surg_vars,":AbnCog" ,  collapse=" + ")) )
myform %<>% update(paste0( "~ . + bs(Age_at_CPAP, 3) "))
myform %<>% update(paste0( "~ . + " , paste0(comborbid_vars ,  collapse=" + ") ) )
hosp_proc %>% analysis_pipe

## mortality - only 16
mortality_data <- read_csv("metav.csv" , col_types=cols( MPI = col_character(), DATE_OF_DEATH=col_datetime(format="%Y-%m-%d-%H.%M.%S" )   ) )
)
hosp_proc %<>% left_join( mortality_data , by = c("EMPI" = "MPI") )
hosp_proc %<>% mutate(death = DATE_OF_DEATH < DISCHARGE_DATE + ddays(2) | DATE_OF_DEATH < Surgery_Date + ddays(30))
hosp_proc %<>% mutate(death = if_else(is.na(death), FALSE, death ) )


analysis_pipe <- . %>% mutate(thisout=death) %>% mutate(AbnCog= as.numeric(AbnCog)) %>% glm(data=., formula=myform,  family=binomial() ) %>% summary %>% extract2("coefficients") %>% as_tibble(rownames="rname") %>% filter(grepl(rname, pattern="AbnCog")) %>% select(-`z value`)

## overall - no association
myform <- paste0( "thisout ~ 0 + ", paste0(surg_vars, collapse=" + "), " + AbnCog" ) %>% as.formula
hosp_proc  %>% analysis_pipe

## now adjust for age 
myform <- paste0( "thisout ~ 0 + ", paste0(surg_vars, collapse=" + "), " + AbnCog+ bs(Age_at_CPAP, 3)" ) %>% as.formula
hosp_proc %>% analysis_pipe

## adjust for comorbid
myform <- paste0( "thisout ~ 0 + ", paste0(surg_vars, collapse=" + ")) %>% as.formula
myform %<>% update(paste0( "~ . + AbnCog" )) 
myform %<>% update(paste0( "~ . + bs(Age_at_CPAP, 3) "))
myform %<>% update(paste0( "~ . + " , paste0(comborbid_vars ,  collapse=" + ") ) )
hosp_proc %>% analysis_pipe


## adjusted surg-specific - blows up due to singularities
myform <- paste0( "thisout ~ 0 + ", paste0(surg_vars, collapse=" + ")) %>% as.formula
myform %<>% update(paste0( "~ . +" , paste0(surg_vars,":AbnCog" ,  collapse=" + ")) )
myform %<>% update(paste0( "~ . + bs(Age_at_CPAP, 3) "))
myform %<>% update(paste0( "~ . + " , paste0(comborbid_vars ,  collapse=" + ") ) )
hosp_proc %>% analysis_pipe

# Analysis 3:
#     1. Etiology of readmission in patients readmitted with abnormal cognitive screen vs those with a normal preoperative cognitive screen

## with only 42 readmits detectect, I don't think this is worth the effort

# Analysis 4:
#     1. Comparative predictive value of AD8 and SBT 

