# docker run -v "/mnt/ris/ActFastData/:/research/" -v "/home/christopherking/gitdirs/cognition_discharge:/code" -v "/mnt/ris/lavanya/cognition_check/:/output" cryanking/cognitioncheck:1.1 R  -f /code/merge_epic_era.R

#LSF_DOCKER_VOLUMES='/storage1/fs1/christopherking/Active/ActFastData/:/research/  /storage1/fs1/christopherking/Active/lavanya/cognition_check/:/output/ /home/lavanya/gitrepos/cognition_discharge:/code' bsub -G 'compute-christopherking' -n 2 -R 'rusage[mem=32GB] span[hosts=1]' -M 32GB -q general-interactive -Is -a 'docker(cryanking/cognitioncheck:1.1)' /bin/bash
#Digest: sha256:9b99f73d209fb61e18cd4829f7b01c039b6645484dd1c4fa79a20d7d809b7f1d

library(data.table)
library(lubridate)
library(magrittr)
library(purrr)
library(splines)
library(pROC)
library(tidyverse)
library(dplyr)  
library(splines)
library(nonnest2)


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

merged_post <- fread( "/research/Actfast_deident_epic/epic_outcomes.csv" )
matching_ids <- fread( "/research/Actfast_reident_epic/epic_orlogid_codes.csv" )
setnames(merged_post, "orlogid", "orlogid_encoded")

exploratory_outcomes <- matching_ids[ , .(orlogid,orlogid_encoded) ][merged_post[ , .(orlogid_encoded, PNA, AF, post_aki_status, CVA, postop_trop_high, postop_vent_duration ) ] , on="orlogid_encoded"]
exploratory_outcomes[, orlogid_encoded:= NULL]
exploratory_outcomes[ ,post_aki_status := post_aki_status >= 2 ]
exploratory_outcomes[ ,resp_failure := fcase( is.na(postop_vent_duration), FALSE, postop_vent_duration < 1, FALSE, default=TRUE) ]
exploratory_outcomes[, postop_vent_duration:= NULL]

## admission data to get discharge times
epic_admits <- fread(paste0(clarity_root , 'Clarity Hogue Result Set ADT.csv'))
my_visits <- epic_admits[ADT_EVENT %chin% c("Admission","Discharge" , "Hospital Outpatient") , .(CurrentMRN = first(CurrentMRN), admt = min(EFFECTIVE_TIME), dist=max(EFFECTIVE_TIME) ) , by="CSN"]

## this ID mapping file is needed to merge the procedure codes
epic_id2<-  fread(paste0(clarity_root , 'Clarity Harper Result Set Anesthesia Identifiers.csv') )
epic_id2 <- epic_id2 [AN_3_ENC_CSN_ID != "NULL"]
epic_id2[, MAR_WINDOW_END := lubridate::ymd_hms(MAR_WINDOW_END)]
epic_id2 <- epic_id2[!is.na(MAR_WINDOW_END)]


## "Figure 1" data
##  -- all surgeries > 65 
##  -- distinct hospitalizations w / qualifying procedures
##  -- eval w/i 90 days
##  -- cog data present
##  -- admission data present -> 100%
# > figure1
#                   Stage     N deltaN
# 1    surgeries age > 65 33195     NA
# 2 qualifying procedures  5334     NA
# 3      eval w/i 90 days  5008     NA
# 4      cog data present  3674     NA

## Find out how recent the preop was

this_file <- '/research/2021_10_22_Clarity_Extract/2021_10_22_SDE_AnesthPreProcNote_ansi.csv'
single_line <- scan(file=this_file , nlines=1, sep="%", what=character())
 col_pos <- gregexpr(single_line, pattern="\\b\\w" , perl=TRUE)[[1]]
 smartdata <- readr::read_fwf(file=this_file, col_positions=readr::fwf_positions(start=col_pos , end=c(col_pos[-1 ]-1 , NA  ) ), skip=2L, col_types="cicicT",na="NULL" )
 colnames(smartdata) <- stringr::str_extract_all(single_line, "\\w+")[[1]]
 setDT(smartdata)
## this file has ~ 500 broken likes because of some multi-byte character like non-breaking space
## iconv -f utf-8 -t ascii//translit 2021_10_22_SDE_AnesthPreProcNote.csv -o 2021_10_22_SDE_AnesthPreProcNote_ansi.csv

smartdata <- smartdata[!is.na(CUR_VALUE_DATETIME)]
smartdata <- smartdata[CUR_VALUE_DATETIME > lubridate::ymd("2018-08-31")]


setnames(smartdata, "SMRTDTA_ELEM_VALUE" , "Value" )


sheet_list <- readxl::excel_sheets("/research/sync_aw_dump/Anesthesia SDEs.xlsx")
all_sde <- list()
for(i in sheet_list) {all_sde <- c(all_sde, list(readxl::read_xlsx("/research/sync_aw_dump/Anesthesia\ SDEs.xlsx", sheet=i)))}
all_sde %<>% lapply(function(x){ set_colnames(x,c("SDE", "oldName", "label" , "Name" , rep("....a", length=ncol(x)-4L ) ) )[-1,c(1,4)] })
## other tabs are DOS
all_sde <- all_sde [1:2]
all_sde <- do.call(rbind, all_sde)
setDT(all_sde)
all_sde <- all_sde[!is.na(SDE)][!is.na(Name)]
smartdata <- all_sde[smartdata, on="SDE==ELEMENT_ID", nomatch=NULL]
smartdata <- epic_id2[, .(orlogid=LOG_ID, PAT_LINK_ID=AN_PAT_ID, MAR_WINDOW_END)][smartdata, on="PAT_LINK_ID" , allow.cartesian=TRUE][MAR_WINDOW_END > CUR_VALUE_DATETIME]

preop_dates <- smartdata[ ,.(preopdate=last(CUR_VALUE_DATETIME) ), by="orlogid"] 


## and the SBT date


temp_flow<- fread('/research/2021_10_22_Clarity_Extract/filtered_flow.csv' )

flow_names <- fread('/research/sync_aw_dump/flowsheet_measures.csv') 

temp_flow<- flow_names[temp_flow, on=c("FLO_MEAS_ID"= "Flo_Meas_ID")]

cog_dates <- preop_covariates[ ,.(MRN=`Patient Primary MRN`, orlogid, AnestStart, AnestStop)][temp_flow, on="MRN", allow.cartesian=TRUE ][Recorded_Time<AnestStart ]

cog_dates <- cog_dates[MEASURE_NAME %chin% c("Short Blessed Total Score" , "AD8 Dementia Score") , .(cogdate=last(Recorded_Time)) ,  by="orlogid"]


## filter to just the variables of interest
merged_data <- processed_preop_screen[, .(orlogid, AD8=`AD8 Dementia Score`, SBT=`Short Blessed Total Score`, low_barthel=`Barthel index score`<100 ) ] %>% 
merge(admit_outcomes[ , .(orlogid,ICU,  postop_los, readmit=fcase(is.na(readmission_survival) , FALSE, readmission_survival>30, FALSE, default=TRUE ) ) ] , by="orlogid") %>%
merge(preop_covariates[, .(orlogid, death_date = `Patient Death Date` , RACE , Sex , age, COPD , CAD , CKD , CHF , CVA_Stroke , cancerStatus, Diabetes, FunctionalCapacity, AFIB, low_functional_capacity=FunctionalCapacity<3 , BMI=WEIGHT_IN_KG/((HEIGHT_IN_INCHES/39.3701)^2) , HTN) ] , by="orlogid" )
## i merge the discharge dates later, will transform the death dates to an outcome then
merged_data <- merged_data[age>=64.5]

figure1 <- data.frame(Stage="surgeries age > 65", N=merged_data$orlogid %>% uniqueN , deltaN=NA_real_)


procedure_codes <- epic_id2[ , .(orlogid=LOG_ID, CSN=AN_3_ENC_CSN_ID, AN_PROC_NAME, AN_DATE)][procedure_codes[, .(CSN=`Encounter Epic CSN` %>% as.character, codes=`Procedure Code Concat (1-50)`, dist=`Discharge Date`, admt=`Admit Date`, dispo=`Discharge Disposition`,`Procedure Code Type`) ]  , on="CSN"]

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
, bladder_codes = c("0TTB")
, ueavfist_codes = c("031[345678569ABCH]0[A-Z0-9][DF]","03WY0J","05WY0[JK]Z" )
, vats_codes = c("0BT[CDFGHJK]4ZZ")
)
# "51570", "51575", "51596", "51590", "51595", "51580", "51585", "51555", "51550", "51565", "51597"
# , "36830" , "36818", "36818", "36819", "36821", "36833", "36832", "36825"

cpt_codes <- readxl::read_xlsx("/research/sync_aw_dump/CPT codes.xlsx", skip=1)
cpt_codes %<>% dplyr::filter(is.na(Exclude))
cpt_codes <- as.data.frame(cpt_codes)
setDT(cpt_codes)

pattern_names <- data.table(code_names = names(code_patterns), Group= c(2,3,1,4,5,6,9,10,7,8,12,13,15,15,11 ))
cpt_codes <- pattern_names[cpt_codes ,on="Group"]



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





## DONE: (1) modify this to iterate over arrays, as done for mv (2) add more cpt codes
## instead of grep-ing each column many times, the strategy is to create a list of the codes included in each procedure set (using the unique codes), then do a match() and collapse each hospitalization's codes into indicator variables for each procedure set
## this is a little painful seeming because it is trying to avoid explicit loops / use R's vectorized utilities where possible and avoid repeated calls to grep (which is slowish)
## it probably would have been easier / cleaner to melt this to long then use a by method in data.table

## first split the code column into a list
all_codes <- procedure_codes[`Procedure Code Type`=="ICD10-PCS"]$codes %>% strsplit( split=",", fixed=T)  
all_codes_cpt <- procedure_codes[`Procedure Code Type`=="HCPCS CPT-4"]$codes %>% strsplit( split=",", fixed=T)

## save a little time by doing this once
unique_codes <- all_codes %>% unlist %>% unname %>% unique
unique_codes_cpt <- all_codes_cpt %>% unlist %>% unname %>% unique

## per-procedure set list of codes that actually occur
observed_codes <- lapply(code_patterns, function(y) unique(unname(unlist(sapply(y, function(x) grep(pattern=paste0("^", x),unique_codes , value=T)) )  ) ))

observed_codes_cpt <- lapply(pattern_names$code_names, function(x) {cpt_codes[code_names==x]$Proc_code %>% unique} )

names(observed_codes_cpt) <- pattern_names$code_names

## transform a code into the set it belongs to; this requires the assumption that a a specific code doese not overlap into multiple sets
code_categories <- chmatch(all_codes %>% unlist %>% unname , observed_codes %>% unlist ) %>% cut( breaks = c(0L,cumsum(sapply(observed_codes, length) ) )) %>% forcats::lvls_revalue( c(names(observed_codes) ) )

code_categories_cpt <- chmatch(all_codes_cpt %>% unlist %>% unname , observed_codes_cpt %>% unlist ) %>% cut( breaks = c(0L,cumsum(sapply(observed_codes_cpt, length) ) )) %>% forcats::lvls_revalue( c(names(observed_codes_cpt) ) )


na_false <- function(x) fifelse(is.na(x), FALSE, x )
for( thisset in names(code_patterns) ) {
## create a per-hospitalization list of codes in a given category, then create an indicator for any being present
  set(procedure_codes, i = which(procedure_codes[["Procedure Code Type"]]=="ICD10-PCS") , j=thisset, value=na_false(code_categories==thisset) %>% relist(all_codes ) %>% sapply(any) )
  set(procedure_codes, i = which(procedure_codes[["Procedure Code Type"]]=="HCPCS CPT-4") , j=thisset, value=na_false(code_categories_cpt==thisset) %>% relist(all_codes_cpt ) %>% sapply(any) )
  
}




## accumulate if a hospitalization has any matching codes
procedure_codes[ , included := rowSums(.SD, na.rm = TRUE) > 0 , .SDcols=names(code_patterns) ]

## name consistency with older code
# setnames(procedure_codes, names(code_patterns), names(code_patterns)%>% sub(pattern="_codes", replacement="") )



figure1 <- rbind(figure1 , data.frame(Stage="qualifying procedures", N=length(intersect(procedure_codes$orlogid , merged_data$orlogid ) ), deltaN=NA_real_) )


merged_data2 <- merged_data %>% merge(procedure_codes[, .(orlogid, CSN, gut_codes, stomach_codes, chole_codes, panc_codes, hyster_codes, lumbar_codes,shoulder_codes, hiatalHernia_codes, knee_codes, totalHip_codes, neph_codes,   prost_codes, bladder_codes, ueavfist_codes, vats_codes, dispo) ] , by="orlogid")

merged_data2 %<>% merge(preop_covariates[,.(AnestStart, orlogid)], by="orlogid" )
merged_data2 %<>% merge(preop_dates , by="orlogid")
merged_data2 <- merged_data2[preopdate > AnestStart - ddays(90)]

figure1 <- rbind(figure1 , data.frame(Stage="eval w/i 90 days", N=merged_data2$orlogid %>% uniqueN , deltaN=NA_real_) )

merged_data2 %<>% merge(cog_dates , by="orlogid")
merged_data2 <- merged_data2[cogdate > AnestStart - ddays(90)]
merged_data2 <- merged_data2[ !(is.na(AD8) & is.na(SBT) )]

figure1 <- rbind(figure1 , data.frame(Stage="cog data present", N=merged_data2$orlogid %>% uniqueN , deltaN=NA_real_) )


merged_data2[, AD8 := as.numeric(AD8) ]
merged_data2[, SBT := as.numeric(SBT) ]

merged_data2[ , AbnCog := fcase(
  is.na(AD8) & is.na(SBT), NA, 
  is.na(AD8) , SBT>=5 ,
  is.na(SBT) , AD8>=2 ,
  !(is.na(SBT) | is.na(AD8)), AD8>=2 | SBT>=5 ) ]
  



merged_data2[ , dc_home := dispo!="home"]
my_visits<- my_visits[ preop_covariates[ ,.(CurrentMRN=`Patient Primary MRN`, orlogid, AnestStop)] , allow.cartesian=TRUE, nomatch=NULL, on="CurrentMRN"]

los_data <- my_visits[dist>AnestStop ,.(los = min(as.numeric(difftime(dist,AnestStop) ) ) )  , by="orlogid" ]
los_data[, los:=as.numeric(los)]
merged_data2 %<>% merge(los_data, all.x=TRUE, by="orlogid")

# readmit_data <- my_visits[admt>AnestStop & admt<AnestStop +ddays(30) ,.(readmit = TRUE ) , by="orlogid" ]
# merged_data2 %<>% merge(readmit_data, all.x=TRUE, by="orlogid")
# merged_data2[is.na(readmit)  , readmit:= FALSE] 







## old way of doing things, pulling discharges from nursing documents


## fixing up the death events as promised above
merged_data2[, CSN:=as.character(CSN)]
my_visits[, CSN:=as.character(CSN)]
# merged_data2 <- merge(merged_data2, my_visits, by="CSN", all.x=TRUE, all.y=FALSE)

testing_data2<- merge(merged_data2[, .(orlogid, AnestStart)] , my_visits[, .(orlogid, dist)], by="orlogid", all.x=TRUE, all.y=FALSE, allow.cartesian=TRUE)

testing_data2 <- testing_data2[ dist > AnestStart]
testing_data2 <- testing_data2[ , .(dist=min(dist))  , by="orlogid" ]

merged_data2 <- merge(merged_data2, testing_data2 , by="orlogid", all.x=TRUE, all.y=FALSE)


merged_data2[ , death := fcase(is.na(death_date), FALSE,  death_date < dist + ddays(30), TRUE, default=FALSE) ]

merged_data2[ , .(  gut_codes, stomach_codes, chole_codes, panc_codes, hyster_codes, lumbar_codes,shoulder_codes, hiatalHernia_codes, knee_codes, totalHip_codes, neph_codes,   prost_codes, bladder_codes, ueavfist_codes, vats_codes)] %>% sapply(sum) -> code_counts
# 
#          gut_codes      stomach_codes        chole_codes         panc_codes 
#                662                168                 81                174 
#       hyster_codes       lumbar_codes     shoulder_codes hiatalHernia_codes 
#                491                406                491                 56 
#         knee_codes     totalHip_codes         neph_codes        prost_codes 
#                480                417                156                294 
#      bladder_codes     ueavfist_codes         vats_codes 
#                 92                  7                 49 


# setDF(merged_data2)                                                                                
merged_data2$RACE %<>% as.character
merged_data2$RACE %<>% as.factor %>% fct_other(keep=c("0", "1" )) %>% fct_recode(White = "0", Black = "1") %>% fct_explicit_na("Other")                                                                                
merged_data2$cancerStatus %<>% as.character                                                                                
merged_data2$cancerStatus %<>% as.factor %>% fct_other(keep=c( "0", "2", "3", "4")) %>% fct_recode(`Metastatic Cancer` = "4", `Skin Cancer` = "0", `in remission/radiation/chemo`= "2", `Current Cancer` = "3") %>% fct_explicit_na
# setDT(merged_data2)                                                                               

merged_data2 <- merge(merged_data2, exploratory_outcomes, by = "orlogid", all.x=TRUE)

pretty_names <- c("intestinal", "gastric", "cholecystectomy", "pancreatic", "hysterectomy", "lumbar fusion", "total shoulder", "lap hiatal hernia", "total knee", "total hip", "nephrectomy", "prostatectomy", "cystectomy", "AV fistula", "VATS" )

pretty_names <- cbind(pretty_names , names(code_patterns)  ) %>% set_colnames(c("pretty_name", "SurgeryType"))

swap_pretty_names <- . %>% left_join(pretty_names%>% as_tibble, by="SurgeryType") %>% select(-SurgeryType) %>% rename(SurgeryType=pretty_name) %>% select(SurgeryType, everything() )


comborbid_vars <- c("COPD" , "CAD" , "CKD" , "CHF" , "CVA_Stroke" , "cancerStatus", "Diabetes" )


## surgery specific effects - build formulas externally because of the non-factor structure
## save these building blocks for various models
base_form <- "thisout ~ 1" %>% formula
surg_vars <- colnames(merged_data2) %>% grep(pattern="_codes", value=T)
surg_form <- paste0(surg_vars, collapse=" + ")
surg_interact_form <- paste0(surg_vars,":AbnCog" ,  collapse=" + ")
comorbid_form <- paste0(comborbid_vars ,  collapse=" + ")

## TODO return here: no-intercept model isn't working even with adding sum(codes) [no-intercept implies reference = everyone else]
## import brglm2 and use method = "brglmFit" in glm

myform <- base_form %>% 
  update( paste0("~.+", surg_form) ) %>%
  update( "~.+AbnCog" ) %>%
  update( "~.+bs(age, 5)" ) 



## surgery effects

analysis_pipe <- . %>% mutate(thisout=dc_home) %>% mutate(across(contains("_codes"), as.numeric)) %>% mutate(AbnCog= as.numeric(AbnCog)) %>% 
  glm(data=., formula=myform,  family=binomial() ) %>% 
  summary %>% extract2("coefficients") %>% as_tibble(rownames="rname") %>% filter(grepl(rname, pattern="_codes")) %>% select(-`z value`)

merged_data2 %>% analysis_pipe



analysis_pipe <- . %>% mutate(thisout=dc_home)%>% mutate(across(contains("_codes"), as.numeric)) %>% mutate(AbnCog= as.numeric(AbnCog)) %>% 
  glm(data=., formula=myform,  family=binomial() ) %>% 
  summary %>% extract2("coefficients") %>% as_tibble(rownames="rname") %>% filter(grepl(rname, pattern="AbnCog")) %>% select(-`z value`)

merged_data2 %>% analysis_pipe


if(TRUE) {
analysis_pipe_vu <- function(x) {
g1 <- x %>% mutate(thisout=dispo!="home") %>% filter(!is.na(SBT) & ! is.na(AD8) )%>% mutate(AbnCog= as.numeric(SBT >= 5)) %>% glm(data=., formula=myform,  family=binomial() ) 
g2 <- x %>% mutate(thisout=dispo!="home") %>% filter(!is.na(SBT) & ! is.na(AD8) )%>%mutate(AbnCog= as.numeric(AD8 >= 2)) %>% glm(data=., formula=myform,  family=binomial() ) 
vuongtest(g1, g2)
}


analysis_pipe_cv <- function(x) {

  x2 <- x %>% mutate(thisout=dispo!="home")
  rs <- modelr::crossv_kfold(x2, k=100)
  r1 <- map(rs$train, . %>% as.data.frame %>% filter(!is.na(SBT) & ! is.na(AD8) ) %>% mutate(AbnCog= as.numeric(SBT >= 5)) %>% glm(data=., formula=myform,  family=binomial() ) ) %>% 
    map2_dbl(rs$test, function(.x, .y){
      response <- .y %>% as.data.frame %>% pull("thisout")
      if(n_distinct(response) > 1 ) {
        pROC::roc(direction = "<" , response=response, levels=c(FALSE,TRUE), predictor=predict(.x , newdata=.y %>% as.data.frame %>% mutate(AbnCog= as.numeric(SBT >= 5)) )  ) %>% auc } else {NA_real_}
    } )

  r2 <- map(rs$train, . %>% as.data.frame %>% filter(!is.na(SBT) & ! is.na(AD8) ) %>% mutate(AbnCog= as.numeric(AD8 >= 2)) %>% glm(data=., formula=myform,  family=binomial() ) ) %>% map2_dbl(rs$test, function(.x, .y){
    response <- .y %>% as.data.frame %>% pull("thisout")
    if(n_distinct(response) > 1 ) {
      pROC::roc(direction = "<" , response=response, levels=c(FALSE,TRUE), predictor=predict(.x , newdata=.y %>% as.data.frame %>% mutate(AbnCog= as.numeric(AD8 >= 2)) )  ) %>% auc } else {NA_real_}
  } )
  t.test(na.omit(r1), na.omit(r2) )
}

global_age_spline <- bs(merged_data2$age, 3)


myform <- base_form %>% 
  update( paste0("~.+", surg_form) ) %>%
  update( "~.+AbnCog" ) 
merged_data2  %>% analysis_pipe_vu
merged_data2  %>% analysis_pipe_cv

myform <- base_form %>% 
  update( paste0("~.+", surg_form) ) %>%
  update( "~.+AbnCog" ) %>%
  update( "~.+predict(global_age_spline,age)" ) 
merged_data2  %>% analysis_pipe_vu
merged_data2  %>% analysis_pipe_cv

myform <- base_form %>% 
  update( paste0("~.+", surg_form) ) %>%
  update( paste0("~.+", comorbid_form) ) %>%
  update( "~.+AbnCog" ) %>%
  update( "~.+predict(global_age_spline,age)" ) 
analysis_pipe_vu_output <- merged_data2  %>% analysis_pipe_vu
analysis_pipe_cv_output <- merged_data2  %>% analysis_pipe_cv
}

myform <- base_form %>%
  update( paste0("~.+", surg_form) ) %>%
  update( paste0("~.+", comorbid_form) ) %>%
  update( "~.+AbnCog" ) %>%
  update( "~.+bs(age, 5)" )

dc_home_glm <- merged_data2 %>% mutate(thisout=dispo!="home") %>% mutate(AbnCog= as.numeric(AbnCog)) %>% glm(data=., formula=myform,  family=binomial() )
readmit_glm  <- merged_data2 %>%filter(dispo=="home") %>% mutate(thisout=readmit) %>% mutate(AbnCog= as.numeric(AbnCog)) %>% glm(data=., formula=myform,  family=binomial() )
death_glm <- merged_data2 %>% mutate(thisout=death) %>% mutate(AbnCog= as.numeric(AbnCog)) %>% glm(data=., formula=myform,  family=binomial() )
los_glm <- merged_data2 %>% filter %>% filter(dispo =="home") %>% mutate(thisout=los) %>% mutate(AbnCog= as.numeric(AbnCog)) %>% glm(data=., formula=myform,  family=quasipoisson() )
coef_home <- dc_home_glm  %>% summary %>% extract2("coefficients") %>% as_tibble(rownames="rname") %>% filter(grepl(rname, pattern="AbnCog")) %>% select(-`z value`)
coef_readmit <-  readmit_glm %>% summary %>% extract2("coefficients") %>% as_tibble(rownames="rname") %>% filter(grepl(rname, pattern="AbnCog")) %>% select(-`z value`)
coef_death <- death_glm %>% summary %>% extract2("coefficients") %>% as_tibble(rownames="rname") %>% filter(grepl(rname, pattern="AbnCog")) %>% select(-`z value`)
coef_los <- los_glm %>% summary %>% extract2("coefficients") %>% as_tibble(rownames="rname") %>% filter(grepl(rname, pattern="AbnCog")) %>% select(-`t value`)
ci_pipe <- . %>%  confint.default %>% as_tibble(rownames="rname") %>% filter(grepl(rname, pattern="AbnCog")) %>% select(-rname) %>% as.vector %>% exp %>% round(2) %>% sprintf(fmt="%.2f") %>% paste(collapse=" to ")

ci_home <- dc_home_glm %>% ci_pipe
ci_readmit <- readmit_glm %>% ci_pipe
ci_death <- death_glm %>% ci_pipe
ci_los <- los_glm %>% ci_pipe


base_form <- "thisout ~ 1" %>% formula
surg_vars <- colnames(merged_data2) %>% grep(pattern="_codes", value=T)
surg_form <- paste0(surg_vars %>% setdiff(c("vats_codes", "ueavfist_codes") ) , collapse=" + ")
surg_interact_form <- paste0(surg_vars %>% setdiff(c("vats_codes", "ueavfist_codes") ),":AbnCog" ,  collapse=" + ")
comorbid_form <- paste0(comborbid_vars ,  collapse=" + ")
myform <- base_form %>% 
  update( paste0("~.+", surg_form) ) %>%
  update( paste0("~.+", surg_interact_form) ) %>%
  update( paste0("~.+", comorbid_form) ) %>%
  update( "~.+bs(age, 5)" )
  
  
inter_glm <- merged_data2 %>% mutate(thisout=dispo!="home") %>% mutate(AbnCog= as.numeric(AbnCog)) %>% mutate(across(contains("_codes"), as.numeric ) ) %>% glm(data=., formula=myform,  family=binomial() ) 

point_inter <-   inter_glm %>% extract2("coefficients") %>% as_tibble(rownames="rname") %>% filter(grepl(rname, pattern="AbnCog")) %>% select(rname, value)

cis_inter <-inter_glm  %>%  confint.default %>% as_tibble(rownames="rname")  %>% filter(grepl(rname, pattern="AbnCog"))

cis_inter %<>% mutate(SurgeryType =rname %>% sub(pattern=":.*", replacement="") )  %>% swap_pretty_names

point_inter <- point_inter[cis_inter%>% transmute(width=`97.5 %` - `2.5 %`) %>% unlist %>%order(decreasing=TRUE),]

cis_inter %<>% arrange( desc(`97.5 %` - `2.5 %` ) )

temp <- dc_home_glm %>% confint.default %>% as_tibble(rownames="rname") %>% filter(grepl(rname, pattern="AbnCog")) %>% select(-rname) %>% as.vector

setwd("/output/")
png(file="forest_home_epic_surgery.png", width=7, height=5, units="in", res=300)
par(mar=c(3,0,0,0))
plot(x=0, y=0, xlim=c(-6,3), ylim=c(-14, 0), type='n', axes=FALSE, ylab="", xlab="")

text(x=-5.9, y=-seq.int(nrow(cis_inter)) , labels = cis_inter$SurgeryType , pos=4)
abline(v=0)
abline(h=-.1)
text(x=-6, y=0.2, labels="Surgery type", pos=4)
text(x=-3, y=0.2, labels="more dc home", pos=4)
text(x= 0, y=0.2, labels="less dc home", pos=4)

points(x=point_inter$value, y=-seq.int(nrow(cis_inter)), pch=19  )
arrows(y0=-seq.int(nrow(cis_inter)), y1=-seq.int(nrow(cis_inter)), x0=cis_inter[["2.5 %"]], x1=cis_inter[["97.5 %"]]  , length=0)

text(x=-5.9, y=-(nrow(cis_inter)+1) , labels = "overall" , pos=4)
points(x=coef_home[2], y=-(nrow(cis_inter)+1), pch=19 , col='red')
arrows(y0=-(nrow(cis_inter)+1), y1=-(nrow(cis_inter)+1), x0=temp[["2.5 %"]], x1=temp[["97.5 %"]]  , length=0, col='red')
axis(1, at=log(c(.125, .25, .5, 1, 2, 4, 8 )), labels=c("1/8", "1/4", "1/2", "1", "2", "4", "8" )  , cex.axis=.9)
axis(1, at=-4, labels="odds-ratio", lwd=0)
dev.off()

anova(inter_glm, dc_home_glm, test="Rao")


myform <- base_form %>% 
  update( paste0("~.+", surg_form) ) %>%
  update( paste0("~.+", comorbid_form) ) %>%
  update( "~.+AbnCog" ) %>%
  update( "~.+predict(global_age_spline,age)" ) 


# exploratory outcomes

# CVA
CVA_glm <- merged_data2 %>% mutate(thisout=CVA) %>% mutate(AbnCog= as.numeric(AbnCog)) %>% glm(data=., formula=myform,  family=binomial() )
coef_CVA <- CVA_glm  %>% summary %>% extract2("coefficients") %>% as_tibble(rownames="rname") %>% filter(grepl(rname, pattern="AbnCog")) %>% select(-`z value`) 
coef_CVA <- coef_CVA %>% add_column(exploratory_outcomes= "CVA")

# AF
AF_glm <- merged_data2 %>% mutate(thisout=AF) %>% mutate(AbnCog= as.numeric(AbnCog)) %>% glm(data=., formula=myform,  family=binomial() )
coef_AF <- AF_glm  %>% summary %>% extract2("coefficients") %>% as_tibble(rownames="rname") %>% filter(grepl(rname, pattern="AbnCog")) %>% select(-`z value`)  
coef_AF <- coef_AF %>% add_column(exploratory_outcomes= "AF")

# PNA
PNA_glm <- merged_data2 %>% mutate(thisout=PNA) %>% mutate(AbnCog= as.numeric(AbnCog)) %>% glm(data=., formula=myform,  family=binomial() )
coef_PNA <- PNA_glm  %>% summary %>% extract2("coefficients") %>% as_tibble(rownames="rname") %>% filter(grepl(rname, pattern="AbnCog")) %>% select(-`z value`) 
coef_PNA <- coef_PNA %>% add_column(exploratory_outcomes= "PNA")

# post_aki_status
post_AKI_glm <- merged_data2 %>% mutate(thisout=post_aki_status) %>% mutate(AbnCog= as.numeric(AbnCog)) %>% glm(data=., formula=myform,  family=binomial() )
coef_post_AKI <- post_AKI_glm  %>% summary %>% extract2("coefficients") %>% as_tibble(rownames="rname") %>% filter(grepl(rname, pattern="AbnCog")) %>% select(-`z value`) 
coef_post_AKI <- coef_post_AKI %>% add_column(exploratory_outcomes= "post_aki_status")

# postop_top_high
postop_top_high_glm <- merged_data2 %>% mutate(thisout=postop_trop_high) %>% mutate(AbnCog= as.numeric(AbnCog)) %>% glm(data=., formula=myform,  family=binomial() )
coef_postop_trop_high <- postop_top_high_glm  %>% summary %>% extract2("coefficients") %>% as_tibble(rownames="rname") %>% filter(grepl(rname, pattern="AbnCog")) %>% select(-`z value`) 
coef_postop_trop_high <- coef_postop_trop_high %>% add_column(exploratory_outcomes= "post_trop_high")

# resp_failure
resp_failure_glm <- merged_data2 %>% mutate(thisout=resp_failure) %>% mutate(AbnCog= as.numeric(AbnCog)) %>% glm(data=., formula=myform,  family=binomial() )
coef_resp_failure <- resp_failure_glm  %>% summary %>% extract2("coefficients") %>% as_tibble(rownames="rname") %>% filter(grepl(rname, pattern="AbnCog")) %>% select(-`z value`) 
coef_resp_failure <- coef_resp_failure  %>% add_column(exploratory_outcomes= "resp_failure")

# ICU
ICU_glm <- merged_data2 %>% mutate(thisout=ICU) %>% mutate(AbnCog= as.numeric(AbnCog)) %>% glm(data=., formula=myform,  family=binomial() )
coef_ICU <- ICU_glm  %>% summary %>% extract2("coefficients") %>% as_tibble(rownames="rname") %>% filter(grepl(rname, pattern="AbnCog")) %>% select(-`z value`) 
coef_ICU <- coef_ICU  %>% add_column(exploratory_outcomes= "ICU")                                                                                
                                                                                
# conference Intervel
ci_pipe <- . %>%  confint.default %>% as_tibble(rownames="rname") %>% filter(grepl(rname, pattern="AbnCog")) 

ci_CVA <- CVA_glm  %>% ci_pipe %>% mutate_if(is.numeric, round, digits = 3) %>% select(-"rname")
coef_CVA <- bind_cols(coef_CVA, ci_CVA) %>% relocate(exploratory_outcomes, .before = Estimate) 

ci_AF <- AF_glm %>% ci_pipe %>% mutate_if(is.numeric, round, digits = 3)  %>% select(-"rname")
coef_AF <- bind_cols(coef_AF, ci_AF) %>% relocate(exploratory_outcomes, .before = Estimate) 

ci_PNA <- PNA_glm %>% ci_pipe %>% mutate_if(is.numeric, round, digits = 3)  %>% select(-"rname")
coef_PNA <- bind_cols(coef_PNA, ci_PNA) %>% relocate(exploratory_outcomes, .before = Estimate) 

ci_AKI <- post_AKI_glm %>% ci_pipe %>% mutate_if(is.numeric, round, digits = 3)  %>% select(-"rname")
coef_post_AKI<- bind_cols(coef_post_AKI, ci_AKI) %>% relocate(exploratory_outcomes, .before = Estimate) 

ci_postop_top_high <- postop_top_high_glm %>% ci_pipe %>% mutate_if(is.numeric, round, digits = 3)  %>% select(-"rname")
coef_postop_trop_high <- bind_cols(coef_postop_trop_high , ci_postop_top_high) %>% relocate(exploratory_outcomes, .before = Estimate) 

ci_resp_failure <- resp_failure_glm %>% ci_pipe %>% mutate_if(is.numeric, round, digits = 3)  %>% select(-"rname")
coef_resp_failure <- bind_cols(coef_resp_failure, ci_resp_failure) %>% relocate(exploratory_outcomes, .before = Estimate) 
                                                                                
ci_ICU <- ICU_glm %>% ci_pipe %>% mutate_if(is.numeric, round, digits = 3)  %>% select(-"rname")
coef_ICU <- bind_cols(coef_ICU, ci_ICU) %>% relocate(exploratory_outcomes, .before = Estimate)                                                                                 

exploratory_outcomes_glm <- bind_rows(coef_CVA, coef_PNA, coef_AF, coef_post_AKI, coef_postop_trop_high , coef_resp_failure, coef_ICU)
exploratory_outcomes_glm <- exploratory_outcomes_glm %>% select(-c("rname", "Std. Error"))  
exploratory_outcomes_glm[["Estimate"]] %<>% exp %>% round(2)
exploratory_outcomes_glm[["2.5 %"]] %<>% exp %>% round(2)
exploratory_outcomes_glm[["97.5 %"]] %<>% exp %>% round(2)
exploratory_outcomes_glm %<>% rename(`p val`= `Pr(>|z|)`)
exploratory_outcomes_glm[["p val"]] %<>%  round(3) %>% format.pval(eps=.001)  
# exploratory_outcomes_glm $`Std. Error` <- round(exploratory_outcomes_glm$`Std. Error`, digits = 2)


encode_onehot <- function(x, colname_prefix = "", colname_suffix = "") {
  if (!is.factor(x)) {
      x <- as.factor(x)
  }
  encoding_matrix <- contrasts(x, contrasts = FALSE)
  encoded_data <- encoding_matrix[as.integer(x),]
  colnames(encoded_data) <- paste0(colname_prefix, colnames(encoded_data), colname_suffix)
  encoded_data
}

merged_data2$year <- format(merged_data2$AnestStart, format= "%Y")
merged_data2 <- bind_cols(merged_data2, encode_onehot(merged_data2$year, colname_prefix = "year_") %>% as_tibble)

base_form <- "thisout ~ 1" %>% formula
surg_vars <- colnames(merged_data2) %>% grep(pattern="_codes", value=T)
surg_form <- paste0(surg_vars, collapse=" + ")
surg_interact_form <- paste0(surg_vars,":AbnCog" ,  collapse=" + ")
comorbid_form <- paste0(comborbid_vars ,  collapse=" + ")
year_vars <- colnames(merged_data2) %>% grep(pattern = "year_", value=T)
year_form <- paste0(year_vars, collapse = " + ")
year_interact_form <- paste0(year_vars, ":AbnCog" , collapse=" + ")


myform <- base_form %>% 
  update( paste0("~.+", year_form) ) %>%
  update( paste0("~.+", comorbid_form) ) %>%
  update( "~.+AbnCog" )


dc_home_glm_year <- merged_data2 %>% mutate(thisout=dispo!="home") %>% mutate(AbnCog= as.numeric(AbnCog)) %>% glm(data=., formula=myform,  family=binomial() )
readmit_glm_year  <- merged_data2 %>%filter(dispo=="home") %>% mutate(thisout=readmit) %>% mutate(AbnCog= as.numeric(AbnCog)) %>% glm(data=., formula=myform,  family=binomial() )
death_glm_year <- merged_data2 %>% mutate(thisout=death) %>% mutate(AbnCog= as.numeric(AbnCog)) %>% glm(data=., formula=myform,  family=binomial() )
los_glm_year <- merged_data2 %>% filter %>% filter(dispo =="home") %>% mutate(thisout=los) %>% mutate(AbnCog= as.numeric(AbnCog)) %>% glm(data=., formula=myform,  family=quasipoisson() )
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
  

inter_glm_year <- merged_data2 %>% mutate(thisout=dispo!="home") %>% mutate(AbnCog= as.numeric(AbnCog)) %>% mutate(across(contains("_codes"), as.numeric ) ) %>% glm(data=., formula=myform,  family=binomial() ) 

point_inter_year <-   inter_glm_year %>% extract2("coefficients") %>% as_tibble(rownames="rname") %>% filter(grepl(rname, pattern="AbnCog")) %>% select(rname, value)

cis_inter_year <-inter_glm_year  %>%  confint.default %>% as_tibble(rownames="rname")  %>% filter(grepl(rname, pattern="AbnCog"))

cis_inter_year <- inner_join(cis_inter_year, point_inter_year %>% select(rname, value), by="rname")
                                                                                
                                                                                
cis_inter_year %<>% mutate(YEAR =rname %>% sub(pattern=":.*", replacement="") )  

#point_inter_year <- point_inter_year[cis_inter_year %>% transmute(width=`97.5 %` - `2.5 %`) %>% unlist %>%order(decreasing=TRUE),]

#point_inter_year %<>%  arrange(YEAR)
cis_inter_year %<>%  arrange(YEAR)           
           
temp1 <- dc_home_glm_year %>% confint.default %>% as_tibble(rownames="rname") %>% filter(grepl(rname, pattern="AbnCog")) %>% select(-rname) %>% as.vector

cis_inter_year %<>%  bind_rows( data.frame(value=coef_home_year[2], `97.5 %`=temp1[["97.5 %"]], `2.5 %`=temp1[["2.5 %"]], YEAR="All Epic"  ) )

png(file="forest_home_epic_year.png", width=5, height=5, units="in", res=300)
par(mar=c(3,0,0,0))
plot(x=0, y=0, xlim=c(-6,3), ylim=c(-4, 0.3), type='n', axes=FALSE, ylab="", xlab="")

text(x=-5.9, y=-seq.int(nrow(cis_inter_year)) , labels = cis_inter_year$YEAR %>% sub(pattern="year_", replacement="")  , pos=4)
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



setnames(merged_data2, "CVA_Stroke", "CVA(TIA)")                                                                               
saveRDS(merged_data2, "merged_data2.RDS" )
save( file="cognition_cache_epic.rda" ,
  figure1,
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
  swap_pretty_names,
  analysis_pipe_vu_output,
  analysis_pipe_cv_output,
  exploratory_outcomes_glm ,
  cis_inter_year
  )



