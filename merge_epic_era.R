# docker run -it --rm -v "/mnt/ris/ActFastData/:/research/"  cryanking/verse_plus R  

library(data.table)
library(lubridate)
library(magrittr)
clarity_root <- '/research/ActFast_Epic_Flow/Data 202004/Clarity data/'

## AD8, SBT pre-processed in main actfast
processed_preop_screen <- fread("/research/ActFast_Intermediates/epic_cpap_flows.csv")

## demographics and comorbidities pre-processed in main actfast
preop_covariates <- fread("/research/ActFast_Intermediates/epic_preop_before_labs_text_notes.csv")

## ICU status, length of stay, readmission, pre-processed in main actfast
admit_outcomes <- fread("/research/ActFast_Intermediates/epic_flo_outcomes.csv")

## death is just MRN +  date, need to make survival something relative to surgeries; preop file has already merged orlogid (surgery events) to MRN
death_outcomes <- fread('/research/ActFast_Big/Epic Through 2020_11/Report 2.csv')
preop_covariates <- merge(death_outcomes, preop_covariates, by.y="CurrentMRN", by.x="Patient Primary MRN", all.y=T)

procedure_codes <- fread('/research/ActFast_Big/Epic Through 2020_11/Report 4.csv', sep=";" )

## admission data to get discharge times
epic_admits <- fread(paste0(clarity_root , 'Clarity Hogue Result Set ADT.csv'))
my_visits <- epic_admits[ADT_EVENT %chin% c("Admission","Discharge" , "Hospital Outpatient") , .(CurrentMRN = first(CurrentMRN), admt = min(EFFECTIVE_TIME), dist=max(EFFECTIVE_TIME) ) , by="CSN"]



## filter to just the variables of interest
merged_data <- processed_preop_screen[, .(orlogid, AD8=`AD8 Dementia Score`, SBT=`Short Blessed Total Score` ) ] %>% 
merge(admit_outcomes[ , .(orlogid,ICU,  postop_los, readmit=fcase(is.na(readmission_survival) , FALSE, readmission_survival>30, FALSE, default=TRUE ) ) ] , by="orlogid") %>%
merge(preop_covariates[, .(orlogid, death_date = `Patient Death Date` , RACE , Sex , age, COPD , CAD , CKD , CHF , CVA_Stroke , cancerStatus, Diabetes ) ] , by="orlogid" )
## i merge the discharge dates later, will transform the death dates to an outcome then

merged_data <- merged_data[ !is.na(AD8) ][!is.na(SBT)]
merged_data[ , AbnCog := as.numeric(SBT) >= 5 | as.numeric(AD8) >= 2 ]
merged_data <- merged_data[age>=64.5]

## this ID mapping file is needed to merge the procedure codes
epic_id2<-  fread(paste0(clarity_root , 'Clarity Harper Result Set Anesthesia Identifiers.csv') )
epic_id2 <- epic_id2 [AN_3_ENC_CSN_ID != "NULL"]
epic_id2[, MAR_WINDOW_END := lubridate::ymd_hms(MAR_WINDOW_END)]
epic_id2 <- epic_id2[!is.na(MAR_WINDOW_END)]

procedure_codes <- epic_id2[ , .(orlogid=LOG_ID, CSN=AN_3_ENC_CSN_ID, AN_PROC_NAME, AN_DATE)][procedure_codes[, .(CSN=`Encounter Epic CSN` %>% as.character, codes=`Procedure Code Concat (1-50)`, dist=`Discharge Date`, admt=`Admit Date`, dispo=`Discharge Disposition`) ]  , on="CSN"]

procedure_codes[, dispo:= fcase(
  dispo=="RTN" , "home", ## home or self care
  dispo=="ORH" , "facility", ## inpatient rehab
  dispo=="HHC" , "home", ## with home health
  dispo=="SNF", "facility",
  dispo=="ICF", "facility", ## custodial facility = ward of state?
  dispo=="HSM", "death", ## hospice
  dispo=="SWG", "facility", ## swing bed = SNF/ inpt hybrid
  dispo=="EXP", "death", 
  dispo=="AMA", "home",
  dispo=="COL", "home", ## jail
  dispo=="OIN", "facility", ## transfer to cancer center
  dispo=="LTC", "facility",
  dispo=="HOS", "death", ## hospice
  dispo=="LWB", "home", ## also AMA
  dispo=="FED", "facility", ## VA
  dispo=="PSY", "facility", 
  dispo=="DRR", "facility", ## transfer, maybe make NA?
  dispo=="LRP", NA_character_, ## diverted elsewhere - these are probably errors
  dispo=="OTH", "facility", ## transfer / inpatient rehab
  dispo=="DBR", NA_character_, ## I think an error code
  dispo=="NSF", "facility", ## SNF also
  default=NA_character_) ]

## unfortunately, the codes are a comma separated column
## there are some hospitalizations with no billing
procedure_codes <- procedure_codes[codes != ""]
procedure_codes <- procedure_codes[! is.na(dispo)]


code_patterns <- list(
gut_codes = c("0D[BTVD5][89ABEFGHKLMNPQ]"  )
, stomach_codes = c("0D[1BVTD5]6" )
, chole_codes = c("0F[DT]44" )
, panc_codes = c("0F[BT]G")
, hyster_codes = c("0U[TD][4579]" )
, lumbar_codes = c("0SG[01]")
, shoulder_codes = c("0RR[JK]")
, hiatalHernia_codes = c("0BU[RST][34]" )
, knee_codes =  c("0S[UR][CD]")
, totalHip_codes = c("0SR[9B]" )
, neph_codes = c( "0TT[012]")
, prost_codes = c("0VT0" )
, bladder_codes = c("51570", "51575", "51596", "51590", "51595", "51580", "51585", "51555", "51550", "51565", "51597", "0TTB")
, ueavfist_codes = c("031[345678569ABCH]0[A-Z0-9][DF]","03WY0J","05WY0[JK]Z" , "36830" , "36818", "36818", "36819", "36821", "36833", "36832", "36825")
, vats_codes = c("0BT[CDFGHJK]4ZZ")
)


## add in CPT codes from AMO
cpt_code_pattern <- readxl::read_xlsx("/research/ActFast_BJ_Data/External_Dictionaries/CPT codes.xlsx")
cpt_code_pattern <- cpt_code_pattern[is.na(cpt_code_pattern$drop),]

code_patterns$gut_codes <- c(code_patterns$gut_codes, cpt_code_pattern[cpt_code_pattern$group==2 , "code", drop=TRUE] ) %>% unique

code_patterns$stomach_codes <- c(code_patterns$stomach_codes, cpt_code_pattern[cpt_code_pattern$group==3 , "code", drop=TRUE] ) %>% unique

code_patterns$chole_codes <- c(code_patterns$chole_codes, cpt_code_pattern[cpt_code_pattern$group==1 , "code", drop=TRUE] ) %>% unique

code_patterns$panc_codes <- c(code_patterns$panc_codes, cpt_code_pattern[cpt_code_pattern$group==4 , "code", drop=TRUE] ) %>% unique

code_patterns$hyster_codes <- c(code_patterns$hyster_codes, cpt_code_pattern[cpt_code_pattern$group==5 , "code", drop=TRUE] ) %>% unique

code_patterns$lumbar_codes <- c(code_patterns$lumbar_codes, cpt_code_pattern[cpt_code_pattern$group==6 , "code", drop=TRUE] ) %>% unique

code_patterns$shoulder_codes <- c(code_patterns$shoulder_codes, cpt_code_pattern[cpt_code_pattern$group==9 , "code", drop=TRUE] ) %>% unique

code_patterns$hiatalHernia_codes <- c(code_patterns$hiatalHernia_codes, cpt_code_pattern[cpt_code_pattern$group==10 , "code", drop=TRUE] ) %>% unique

code_patterns$knee_codes <- c(code_patterns$knee_codes, cpt_code_pattern[cpt_code_pattern$group==7 , "code", drop=TRUE] ) %>% unique

code_patterns$totalHip_codes <- c(code_patterns$totalHip_codes, cpt_code_pattern[cpt_code_pattern$group==8 , "code", drop=TRUE] ) %>% unique

code_patterns$neph_codes <- c(code_patterns$neph_codes, cpt_code_pattern[cpt_code_pattern$group==12 , "code", drop=TRUE] ) %>% unique

code_patterns$prost_codes <- c(code_patterns$prost_codes, cpt_code_pattern[cpt_code_pattern$group==13 , "code", drop=TRUE] ) %>% unique

code_patterns$bladder_codes <- c(code_patterns$bladder_codes, cpt_code_pattern[cpt_code_pattern$group==14 , "code", drop=TRUE] ) %>% unique

code_patterns$ueavfist_codes <- c(code_patterns$ueavfist_codes, cpt_code_pattern[cpt_code_pattern$group==15 , "code", drop=TRUE] ) %>% unique

code_patterns$vats_codes <- c(code_patterns$vats_codes, cpt_code_pattern[cpt_code_pattern$group==11 , "code", drop=TRUE] ) %>% unique



## testing
if(FALSE) {
procedure_codes_reduced <- procedure_codes[orlogid %in% merged_data$orlogid ] 
sum(( procedure_codes_reduced$codes %>% strsplit( split=",", fixed=T) %>% unlist %>% unname ) %chin% observed_codes[["ueavfist_codes"]][1:25] )

procedure_codes_reduced
procedure_codes_reduced[ grepl(AN_PROC_NAME, pattern="ARTERIOVENOUS", ignore.case=T) ]
}


## DONE: (1) modify this to iterate over arrays, as done for mv (2) add more cpt codes
## instead of grep-ing each column many times, the strategy is to create a list of the codes included in each procedure set (using the unique codes), then do a match() and collapse each hospitalization's codes into indicator variables for each procedure set
## this is a little painful seeming because it is trying to avoid explicit loops / use R's vectorized utilities where possible and avoid repeated calls to grep (which is slowish)
## it probably would have been easier / cleaner to melt this to long then use a by method in data.table

## first split the code column into a list
all_codes <- procedure_codes$codes %>% strsplit( split=",", fixed=T)  

## save a little time by doing this once
unique_codes <- all_codes %>% unlist %>% unname %>% unique

## per-procedure set list of codes that actually occur
observed_codes <- lapply(code_patterns, function(y) unique(unname(unlist(sapply(y, function(x) grep(pattern=paste0("^", x),unique_codes , value=T)) )  ) ))

## transform a code into the set it belongs to; this requires the assumption that a a specific code doese not overlap into multiple sets
code_categories <- chmatch(all_codes %>% unlist %>% unname , observed_codes %>% unlist ) %>% cut( breaks = c(0L,cumsum(sapply(observed_codes, length) ) )) %>% forcats::lvls_revalue( c(names(observed_codes) ) )

na_false <- function(x) fifelse(is.na(x), FALSE, x )
for( thisset in names(code_patterns) ) {
## create a per-hospitalization list of codes in a given category, then create an indicator for any being present
  set(procedure_codes , j=thisset, value=na_false(code_categories==thisset) %>% relist(all_codes ) %>% sapply(any) )
}

## accumulate if a hospitalization has any matching codes
procedure_codes[ , included := rowSums(.SD, na.rm = TRUE) > 0 , .SDcols=names(code_patterns) ]

## name consistency with older code
# setnames(procedure_codes, names(code_patterns), names(code_patterns)%>% sub(pattern="_codes", replacement="") )

## older way of doing it when there was only one pattern per group
if(FALSE) {
  included_proc_codes <- unlist(observed_codes)

  ## to avoid ugly loops in R, match procedure codes to the total list of codes [chmatch is an optimized version of match() for characters], then compare that position to the length of the sets
  for(thisp in names(code_patterns) ) {
    set(procedure_codes , j=thisp, value=grepl(procedure_codes$codes, pattern=code_patterns[[thisp]]  )  )
  }

  procedure_codes[ , included := rowSums(.SD, na.rm = TRUE) > 0 , .SDcols=names(code_patterns) ]

}



if(TRUE) {
## an aside -- figure 1 type flow
## only 16 k hae an AD8+SBT, 30k one or the other
processed_preop_screen[, .(orlogid, AD8=`AD8 Dementia Score`, SBT=`Short Blessed Total Score` ) ] [ !is.na(AD8) | !is.na(SBT)]$orlogid %>% uniqueN

processed_preop_screen[, .(orlogid, AD8=`AD8 Dementia Score`, SBT=`Short Blessed Total Score` ) ] [ !is.na(AD8) ][!is.na(SBT)]$orlogid %>% uniqueN

## these don't do anything
processed_preop_screen[, .(orlogid, AD8=`AD8 Dementia Score`, SBT=`Short Blessed Total Score` ) ] %>% 
merge(admit_outcomes[ , .(orlogid,ICU,  postop_los, readmit=fcase(is.na(readmission_survival) , FALSE, readmission_survival>30, FALSE, default=TRUE ) ) ] , by="orlogid") %>% pull("orlogid") %>% uniqueN

processed_preop_screen[, .(orlogid, AD8=`AD8 Dementia Score`, SBT=`Short Blessed Total Score` ) ] %>% 
merge(admit_outcomes[ , .(orlogid,ICU,  postop_los, readmit=fcase(is.na(readmission_survival) , FALSE, readmission_survival>30, FALSE, default=TRUE ) ) ] , by="orlogid") %>% merge(preop_covariates[, .(orlogid, death_date = `Patient Death Date` , RACE , Sex , age, COPD , CAD , CKD , CHF , CVA_Stroke , cancerStatus, Diabetes ) ] , by="orlogid" ) %>% pull("orlogid") %>% uniqueN

## 33k age >= 65
processed_preop_screen[, .(orlogid, AD8=`AD8 Dementia Score`, SBT=`Short Blessed Total Score` ) ] %>% 
merge(admit_outcomes[ , .(orlogid,ICU,  postop_los, readmit=fcase(is.na(readmission_survival) , FALSE, readmission_survival>30, FALSE, default=TRUE ) ) ] , by="orlogid") %>% merge(preop_covariates[age >= 65, .(orlogid, death_date = `Patient Death Date` , RACE , Sex , age, COPD , CAD , CKD , CHF , CVA_Stroke , cancerStatus, Diabetes ) ] , by="orlogid" ) %>% pull("orlogid") %>% uniqueN

## only 16 k have an AD8+SBT, 30k one or the other
processed_preop_screen[, .(orlogid, AD8=`AD8 Dementia Score`, SBT=`Short Blessed Total Score` ) ][ !is.na(AD8) ][!is.na(SBT)] %>% 
merge(admit_outcomes[ , .(orlogid,ICU,  postop_los, readmit=fcase(is.na(readmission_survival) , FALSE, readmission_survival>30, FALSE, default=TRUE ) ) ] , by="orlogid") %>% merge(preop_covariates[age >= 65, .(orlogid, death_date = `Patient Death Date` , RACE , Sex , age, COPD , CAD , CKD , CHF , CVA_Stroke , cancerStatus, Diabetes ) ] , by="orlogid" ) %>% pull("orlogid") %>% uniqueN

processed_preop_screen[, .(orlogid, AD8=`AD8 Dementia Score`, SBT=`Short Blessed Total Score` ) ][ !is.na(AD8) | !is.na(SBT)] %>% 
merge(admit_outcomes[ , .(orlogid,ICU,  postop_los, readmit=fcase(is.na(readmission_survival) , FALSE, readmission_survival>30, FALSE, default=TRUE ) ) ] , by="orlogid") %>% merge(preop_covariates[age >= 65, .(orlogid, death_date = `Patient Death Date` , RACE , Sex , age, COPD , CAD , CKD , CHF , CVA_Stroke , cancerStatus, Diabetes ) ] , by="orlogid" ) %>% pull("orlogid") %>% uniqueN

## only 13k matching procedures, some procedures like avf are just uncommon in this data
## supported by manually scanning for text

## 40k procedure code data present
procedure_codes$CSN %>% uniqueN

## 15k age > 65
procedure_codes[preop_covariates[age >= 65], on="orlogid"]$CSN %>% uniqueN

## 4.8k included
procedure_codes[preop_covariates[age >= 65], on="orlogid"][included==TRUE, CSN] %>% uniqueN

procedure_codes[ , .(  gut_codes, stomach_codes, chole_codes, panc_codes, hyster_codes, lumbar_codes,shoulder_codes, hiatalHernia_codes, knee_codes, totalHip_codes, neph_codes,   prost_codes, bladder_codes, ueavfist_codes, vats_codes)] %>% sapply(sum)
#          gut_codes      stomach_codes        chole_codes         panc_codes 
#               3764               1213                609                556 
#       hyster_codes       lumbar_codes     shoulder_codes hiatalHernia_codes 
#               2343               1332               1205                183 
#         knee_codes     totalHip_codes         neph_codes        prost_codes 
#               1393               1432                692                823 
#      bladder_codes     ueavfist_codes         vats_codes 
#                206                267                162 

## with ad8+sbt 3716

processed_preop_screen[, .(orlogid, AD8=`AD8 Dementia Score`, SBT=`Short Blessed Total Score` ) ][ !is.na(AD8) ][!is.na(SBT)] %>% 
merge(admit_outcomes[ , .(orlogid,ICU,  postop_los, readmit=fcase(is.na(readmission_survival) , FALSE, readmission_survival>30, FALSE, default=TRUE ) ) ] , by="orlogid") %>% merge(preop_covariates[age >= 65, .(orlogid, death_date = `Patient Death Date` , RACE , Sex , age, COPD , CAD , CKD , CHF , CVA_Stroke , cancerStatus, Diabetes ) ] , by="orlogid" ) %>% merge(procedure_codes[included==TRUE, .(orlogid, CSN, gut_codes, stomach_codes, chole_codes, panc_codes, hyster_codes, lumbar_codes,shoulder_codes, hiatalHernia_codes, knee_codes, totalHip_codes, neph_codes,   prost_codes, bladder_codes, ueavfist_codes, vats_codes, dispo) ] , by="orlogid") %>% pull("orlogid") %>% uniqueN

}


procedure_codes <- procedure_codes[included==TRUE]

merged_data2 <- merged_data %>% merge(procedure_codes[, .(orlogid, CSN, gut_codes, stomach_codes, chole_codes, panc_codes, hyster_codes, lumbar_codes,shoulder_codes, hiatalHernia_codes, knee_codes, totalHip_codes, neph_codes,   prost_codes, bladder_codes, ueavfist_codes, vats_codes, dispo) ] , by="orlogid")

# merged_data2 <- merged_data %>% merge(dispo_holder2, by.x="orlogid", by.y="LOG_ID")

merged_data2[ , dc_home := dispo=="home"]


## old way of doing things, pulling discharges from nursing documents
if(FALSE) {
## discharge events that show up in nursing / case management flowsheets. surprisingly complete.
## TODO: scan if missing patients are mostly "outpatient" and probably don't have this class if never admitted
all_ts <- list.files(clarity_root , full.names=TRUE, pattern="Clarity\\sHogue\\sResult\\sSet\\sFlowsheets\\s\\d")
all_ts %<>% grep(pattern="csv$", value=TRUE)

dispo_holder<- vector('list', length(all_ts))
for(ts_index in seq_along (all_ts) ) {
epic_flow_1 <- fread(all_ts[ts_index] )
dispo_holder[[ts_index]] <- epic_flow_1[MEASURE_NAME %chin% c("DISCHARGE TO" , "Discharge Disposition")]

# epic_flow_1$MEASURE_NAME %>% unique -> all_measures
# # "Discharge Location" about fluids
# "Discharge Disposition"
# "Discharge Records"  
# # "DISCHARGE TO" -> 5508. good values
# epic_flow_1[MEASURE_NAME=="DISCHARGE TO", .(INPATIENT_DATA_ID)] %>% uniqueN ## 4.9k
# epic_flow_1[MEASURE_NAME=="Discharge Disposition", .(INPATIENT_DATA_ID)] %>% uniqueN ##2.6k
# epic_flow_1[MEASURE_NAME=="Discharge Location", .(INPATIENT_DATA_ID)] %>% uniqueN ## 95
# epic_flow_1[, .(INPATIENT_DATA_ID)] %>% uniqueN ## 12k
# 
# # "Was Patient Discharged to Home" -> only 1
}

dispo_holder %<>% rbindlist
dispo_holder %>% saveRDS("/research/ActFast_Intermediates/dispo_status.rda")

## scanning flow take a long time (IO bound), so make a copy
dispo_holder2 <- dispo_holder[!(VALUE %chin% c("Nursing Unit", "Other (Comment)") ) ][!is.na(VALUE)]

dispo_holder2[, dispo_type := fcase(
  grepl(VALUE, pattern="Hospice", fixed=T) , "death" ,
  VALUE=='Expired', "death" ,
  grepl(VALUE, pattern="Hospital", fixed=T) , "facility" ,
  grepl(VALUE, pattern="SNF", fixed=T) , "facility" ,
  grepl(VALUE, pattern="LTAC", fixed=T) , "facility" ,
  grepl(VALUE, pattern="Skilled", fixed=T) , "facility" ,
  grepl(VALUE, pattern="Inpatient", fixed=T) , "facility" ,
  grepl(VALUE, pattern="Home", fixed=T) , "home" ,
  VALUE=='AMA', "home"
) ]

dispo_holder2 <- dispo_holder2[!is.na(dispo_type)]

## I'll use this numeric version as a conveinent way to merge if there are multiple matches (which can happen if plans change)
dispo_holder2[ , dispo_type_num := fcase( dispo_type=="death", 1L, dispo_type=="facility" , 2L, dispo_type=="home", 3L  ) ]

## the flowsheets are linked by MRN and a INPATIENT_DATA_ID which doesn't seem to be used anywhere else in my ID set, so instead I need to pull in the actual admit / discharge times of the hospitalization that I am interested in, which have MRNs and the same set of CSNs used elsewhere.
## discharge times aren't an exact science, so will give a bit of leeway

dispo_holder2 <-  merge(dispo_holder2, my_visits, by="CurrentMRN", allow.cartesian=TRUE, nomatch=NULL)
dispo_holder2 <- dispo_holder2[ data.table::between(PERFORMED, admt, dist+lubridate::dhours(12) ) ]
dispo_holder2[, CSN := as.character(CSN)] 
dispo_holder2 <- merge(dispo_holder2, epic_id2[ , .(AN_3_ENC_CSN_ID, LOG_ID)], by.y="AN_3_ENC_CSN_ID", by.x="CSN" )


dispo_holder2 <- dispo_holder2[! is.na(dispo_type)] 
dispo_holder2 <- dispo_holder2[ , .(dispo_type = min(dispo_type_num ), dist= max(dist, na.rm=T) ), by="LOG_ID" ]

merged_data2 <- merged_data %>% merge(dispo_holder2, by.x="orlogid", by.y="LOG_ID")

merged_data2[ , dc_home := dispo_type==3]
}

## fixing up the death events as promised above
merged_data2[, CSN:=as.character(CSN)]
my_visits[, CSN:=as.character(CSN)]
merged_data2 <- merge(merged_data2, my_visits, by="CSN", all.x=TRUE, all.y=FALSE)
merged_data2[ , death := fcase(is.na(death_date), FALSE,  death_date < dist + ddays(30), TRUE, default=FALSE) ]

merged_data2[ , .(  gut_codes, stomach_codes, chole_codes, panc_codes, hyster_codes, lumbar_codes,shoulder_codes, hiatalHernia_codes, knee_codes, totalHip_codes, neph_codes,   prost_codes, bladder_codes, ueavfist_codes, vats_codes)] %>% sapply(sum)
#          gut_codes      stomach_codes        chole_codes         panc_codes 
#                677                161                 93                176 
#       hyster_codes       lumbar_codes     shoulder_codes hiatalHernia_codes 
#                447                422                522                 50 
#         knee_codes     totalHip_codes         neph_codes        prost_codes 
#                504                427                140                282 
#      bladder_codes     ueavfist_codes         vats_codes 
#                 87                 26                 50 


## TODO: I don't know why some groups are so much smaller than expected. Maybe has to do with how the procedure set was queried. Maybe AV fistulas e.g. aren't going to preop

pretty_names <- c("intestinal", "gastric", "cholecystectomy", "pancreatic", "hysterectomy", "lumbar fusion", "total shoulder", "lap hiatal hernia", "total knee", "total hip", "nephrectomy", "prostatectomy", "cystectomy", "AV fistula", "VATS" )

pretty_names <- cbind(pretty_names , names(code_patterns)  ) %>% set_colnames(c("pretty_name", "SurgeryType"))
  
comborbid_vars <- c("COPD" , "CAD" , "CKD" , "CHF" , "CVA_Stroke" , "cancerStatus", "Diabetes" )


## surgery specific effects - build formulas externally because of the non-factor structure
## save these building blocks for various models
base_form <- "thisout ~ 1" %>% formula
surg_vars <- colnames(merged_data2) %>% grep(pattern="_codes", value=T)
surg_form <- paste0(surg_vars, collapse=" + ")
surg_interact_form <- paste0(surg_vars,":AbnCog" ,  collapse=" + ")
comorbid_form <- paste0(comborbid_vars ,  collapse=" + ")

## TODO return here: no-intercept model isn't working even with adding sum(codes) [no-intercept implies reference = everyone else]

myform <- base_form %>% 
  update( paste0("~.+", surg_form) ) %>%
  update( "~.+AbnCog" ) %>%
  update( "~.+bs(age, 5)" ) 

library(dplyr)  
library(splines)
## surgery effects

analysis_pipe <- . %>% mutate(thisout=dc_home) %>% mutate(AbnCog= as.numeric(AbnCog)) %>% 
  glm(data=., formula=myform,  family=binomial() ) %>% 
  summary %>% extract2("coefficients") %>% as_tibble(rownames="rname") %>% filter(grepl(rname, pattern="_codes")) %>% select(-`z value`)

merged_data2 %>% analysis_pipe



analysis_pipe <- . %>% mutate(thisout=dc_home) %>% mutate(AbnCog= as.numeric(AbnCog)) %>% 
  glm(data=., formula=myform,  family=binomial() ) %>% 
  summary %>% extract2("coefficients") %>% as_tibble(rownames="rname") %>% filter(grepl(rname, pattern="AbnCog")) %>% select(-`z value`)

merged_data2 %>% analysis_pipe

myform <- base_form %>% 
  update( paste0("~.+", surg_form) ) %>%
  update( paste0("~.+", surg_interact_form) ) %>%
  update( paste0("~.+", comorbid_form) ) %>%
  update( "~.+bs(age, 5)" ) 
merged_data2 %>% analysis_pipe

