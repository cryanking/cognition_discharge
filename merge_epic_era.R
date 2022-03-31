# docker run -it --rm -v "/mnt/ris/ActFastData/:/research/"  cryanking/verse_plus R  

library(data.table)
library(lubridate)
library(magrittr)
clarity_root <- '/research/ActFast_Epic_Flow/Data 202004/Clarity data/'

## AD8, SBT
processed_preop_screen <- fread("/research/ActFast_Intermediates/epic_cpap_flows.csv")
preop_covariates <- fread("/research/ActFast_Intermediates/epic_preop_before_labs_text_notes.csv")
admit_outcomes <- fread("/research/ActFast_Intermediates/epic_flo_outcomes.csv")
death_outcomes <- fread('/research/ActFast_Big/Epic Through 2020_11/Report 2.csv')
preop_covariates <- merge(death_outcomes, preop_covariates, by.y="CurrentMRN", by.x="Patient Primary MRN", all.y=T)
procedure_codes <- fread('/research/ActFast_Big/Epic Through 2020_11/Report 4.csv', sep=";" )

merged_data <- processed_preop_screen[, .(orlogid, AD8=`AD8 Dementia Score`, SBT=`Short Blessed Total Score` ) ] %>% 
merge(admit_outcomes[ , .(orlogid,ICU,  postop_los, readmit=fcase(is.na(readmission_survival) , FALSE, readmission_survival>30, FALSE, default=TRUE ) ) ] , by="orlogid") %>%
merge(preop_covariates[, .(orlogid, death_date = `Patient Death Date` , RACE , Sex , age, COPD , CAD , CKD , CHF , CVA_Stroke , cancerStatus, Diabetes ) ] , by="orlogid" )


merged_data <- merged_data[ !is.na(AD8) ][!is.na(SBT)]


# -- need LoS
# -- need discharge to

## 'Clarity Harper Result Set Anesthesia Identifiers.csv' grabs the most matches (still less than 1/3 of the total N)
if(FALSE) {
other_ids <- fread("/research/Actfast_reident_epic/epic_orlogid_codes.csv"  )
epic_id1<-  fread(paste0(clarity_root , 'Clarity MRN CSN INP Combined.csv') )
epic_id2<-  fread(paste0(clarity_root , 'Clarity Harper Result Set Anesthesia Identifiers.csv') )
epic_id2 <- epic_id2 [AN_3_ENC_CSN_ID != "NULL"]

epic_id2[, MAR_WINDOW_END := lubridate::ymd_hms(MAR_WINDOW_END)]
epic_id2 <- epic_id2[!is.na(MAR_WINDOW_END)]


num_overlap <- function(x, y) {
  output <- matrix(NA, nrow=ncol(x), ncol=ncol(y))
  rownames(output ) <- colnames(x)
  colnames(output)  <- colnames(y)
  for( i in seq(ncol(x))) {
    for( j in seq(ncol(y))) {
      output[i,j] <- intersect(as.character(x[[i]]), as.character(y[[j]])) %>% length
    }
  }
  return(output)
}

num_overlap(epic_id1, procedure_codes[, .(`Encounter Epic CSN`)])
num_overlap(epic_id2, procedure_codes[, .(`Encounter Epic CSN`)])
}

epic_id2<-  fread(paste0(clarity_root , 'Clarity Harper Result Set Anesthesia Identifiers.csv') )
epic_id2 <- epic_id2 [AN_3_ENC_CSN_ID != "NULL"]
epic_id2[, MAR_WINDOW_END := lubridate::ymd_hms(MAR_WINDOW_END)]
epic_id2 <- epic_id2[!is.na(MAR_WINDOW_END)]

procedure_codes <- epic_id2[ , .(orlogid=LOG_ID, CSN=AN_3_ENC_CSN_ID)][procedure_codes[, .(CSN=`Encounter Epic CSN` %>% as.character, codes=`Procedure Code Concat (1-50)`) ]  , on="CSN"]

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
, ueavfist_codes = c("031[345678569ABCH]0[A-Z0-9][DF]", "36830" , "36818", "36818", "36819", "36821", "36833", "36832", "36825")
, vats_codes = c("0BT[CDFGHJK]4ZZ")
)

procedure_codes <- procedure_codes[codes != ""]

## DONE: (1) modify this to iterate over arrays, as done for mv (2) add more cpt codes
## I am not at all sure what the "fast" way to handle this is - grep with k patterns on the vector n times?  does it help to have shortened the vectors with strplit? For the lookups to be faster I would need to have saved the has map as in fastmatch
## maybe the strsplit -> unlist -> position is the right way to go
## the match approach wants to be match(codes, included_set), for which hashing the included_set is not a big deal
## it probably would have been easier / cleaner to melt this to long then use a by method in data.table
all_codes <- procedure_codes$codes %>% strsplit( split=",", fixed=T)  
## this includes blank lines if not pre-filtered
# code_length <- stringr::str_count(procedure_codes$codes ,",")+1
unique_codes <- all_codes %>% unlist %>% unname %>% unique

observed_codes <- lapply(code_patterns, function(y) unique(unname(unlist(sapply(y, function(x) grep(pattern=paste0("^", x),unique_codes , value=T)) )  ) ))

code_categories <- chmatch(all_codes %>% unlist %>% unname , observed_codes %>% unlist ) %>% cut( breaks = c(0L,cumsum(sapply(observed_codes, length) ) )) %>% forcats::lvls_revalue( c(names(observed_codes) ) )
#%>% sub(pattern="_codes", replacement="")

na_false <- function(x) fifelse(is.na(x), FALSE, x )
for( thisset in names(code_patterns) ) {
  set(procedure_codes , j=thisset, value=relist(na_false(code_categories==thisset), all_codes ) %>% sapply(any) )
}


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

procedure_codes <- procedure_codes[included==TRUE]



merged_data <- merged_data %>% merge(procedure_codes[, .(orlogid, CSN, gut_codes, stomach_codes, chole_codes, panc_codes, hyster_codes, lumbar_codes,shoulder_codes, hiatalHernia_codes, knee_codes, totalHip_codes, neph_codes,   prost_codes, bladder_codes, ueavfist_codes, vats_codes) ] , by="orlogid")


## discharge events
epic_admits <- fread(paste0(clarity_root , 'Clarity Hogue Result Set ADT.csv'))

all_ts <- list.files(clarity_root , full.names=TRUE, pattern="Clarity\\sHogue\\sResult\\sSet\\sFlowsheets\\s\\d")
all_ts %<>% grep(pattern="csv$", value=TRUE)

dispo_holder<- vector('list', length(all_ts))
for(ts_index in seq_along (all_ts) ) {
epic_flow_1 <- fread(all_ts[ts_index] )
epic_flow_1 <- epic_flow_1[ FLOWSHEET_NAME != "Early Detection of Sepsis"]
epic_flow_1 <- epic_flow_1[ MEASURE_NAME!="Monitor Check"]

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

## annoying links 
my_visits <- epic_admits[ADT_EVENT %chin% c("Admission","Discharge" , "Hospital Outpatient") , .(CurrentMRN = first(CurrentMRN), admt = min(EFFECTIVE_TIME), dist=max(EFFECTIVE_TIME) ) , by="CSN"]
# my_visits[ , LoS := as.numeric(difftime(dist, admt, units="days") ) ]

## scanning flow take a long time, so make a copy
dispo_holder2 <-  merge(dispo_holder, my_visits, by="CurrentMRN", allow.cartesian=TRUE, nomatch=NULL)
dispo_holder2 <- dispo_holder2[ between(PERFORMED, admt, dist+lubridate::dhours(1) ) ]
dispo_holder2 <- dispo_holder2[!(VALUE %chin% c("Nursing Unit", "Other (Comment)") ) ][!is.na(VALUE)]
dispo_holder2[, CSN := as.character(CSN)] 
dispo_holder2 <- merge(dispo_holder2, epic_id2[ , .(AN_3_ENC_CSN_ID, LOG_ID)], by.y="AN_3_ENC_CSN_ID", by.x="CSN" )

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

dispo_holder2 <- dispo_holder2[! is.na(dispo_type)] 
dispo_holder2[ , dispo_type_num := fcase( dispo_type=="death", 1L, dispo_type=="facility" , 2L, dispo_type=="home", 3L  ) ]
dispo_holder2 <- dispo_holder2[ , .(dispo_type = min(dispo_type_num ), dist= max(dist, na.rm=T) ), by="LOG_ID" ]

merged_data2 <- merged_data %>% merge(dispo_holder2, by.x="orlogid", by.y="LOG_ID")

merged_data2[ , AbnCog := as.numeric(SBT) >= 5 | as.numeric(AD8) >= 2 ]
merged_data2[ , dc_home := dispo_type==3]

merged_data2[ , death := fcase(is.na(death_date), FALSE,  death_date < dist + ddays(30), TRUE, default=FALSE) ]

merged_data2[ , .(  gut_codes, stomach_codes, chole_codes, panc_codes, hyster_codes, lumbar_codes,shoulder_codes, hiatalHernia_codes, knee_codes, totalHip_codes, neph_codes,   prost_codes, bladder_codes, ueavfist_codes, vats_codes)] %>% sapply(sum)
# 
#          gut_codes      stomach_codes        chole_codes         panc_codes 
#                473                114                 38                135 
#       hyster_codes       lumbar_codes     shoulder_codes hiatalHernia_codes 
#                161                377                365                  8 
#         knee_codes     totalHip_codes         neph_codes        prost_codes 
#                244                360                 71                 67 
#      bladder_codes     ueavfist_codes         vats_codes 
#                 52                  8                 41 

## TODO: i must be missing av fistulas somehow

pretty_names <- c("intestinal", "gastric", "cholecystectomy", "pancreatic", "hysterectomy", "lumbar fusion", "total shoulder", "lap hiatal hernia", "total knee", "total hip", "nephrectomy", "prostatectomy", "cystectomy", "AV fistula", "VATS" )

pretty_names <- cbind(pretty_names , names(code_patterns)  ) %>% set_colnames(c("pretty_name", "SurgeryType"))
  
comborbid_vars <- c("COPD" , "CAD" , "CKD" , "CHF" , "CVA_Stroke" , "cancerStatus", "Diabetes" )


## surgery specific effects - build formulas externally because of the non-factor structure
## save these building blocks for various models
base_form <- "thisout ~ 0" %>% formula
surg_vars <- colnames(merged_data2) %>% grep(pattern="_codes", value=T)
surg_form <- paste0(surg_vars, collapse=" + ")
surg_interact_form <- paste0(surg_vars,":AbnCog" ,  collapse=" + ")
comorbid_form <- paste0(comborbid_vars ,  collapse=" + ")



myform <- base_form %>% 
  update( paste0("~.+", surg_form) ) %>%
  update( "~.+AbnCog" ) %>%
  update( "~.+bs(age, 5)" ) 

library(dplyr)  
library(splines)
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



