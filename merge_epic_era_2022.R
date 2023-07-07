# docker run -v "/mnt/ris/ActFastData/:/research/" -v "/home/christopherking/gitdirs/cognition_discharge:/code" -v "/mnt/ris/lavanya/cognition_check/:/output" cryanking/cognitioncheck:1.1 R  -f /code/merge_epic_era_2022.R

#LSF_DOCKER_VOLUMES='/storage1/fs1/christopherking/Active/ActFastData/:/research/  /storage1/fs1/christopherking/Active/lavanya/cognition_check/:/output/ /home/lavanya/gitrepos/cognition_discharge:/code' bsub -G 'compute-christopherking' -n 2 -R 'rusage[mem=32GB] span[hosts=1]' -M 32GB -q general-interactive -Is -a 'docker(cryanking/cognitioncheck:1.1)' /bin/bash
#Digest: sha256:9b99f73d209fb61e18cd4829f7b01c039b6645484dd1c4fa79a20d7d809b7f1d

library(data.table)
library(lubridate)
library(magrittr)
library(forcats)
# library(purrr)
library(splines)
# library(pROC)
# library(tidyverse)
# library(dplyr)  
# library(nonnest2)


# clarity_root <- '/research/ActFast_Epic_Flow/Data 202004/Clarity data/'
update_root  <- '/research/2022_update/'

## AD8, SBT included in flowsheets
## the barthel ad8 and sbt are also in /research/tectonics-I2Refresh/Tectonics_Intermediates/clarity_preop_vitals.csv
# processed_preop_screen <- fread( paste0(update_root, "intermediates/clarity_cpap_flows.csv") )
processed_preop_screen <- fread( paste0(update_root, "intermediates/clarity_preop_vitals.csv") )

## demographics and comorbidities pre-processed in main actfast
preop_covariates <- fread(paste0(update_root, "intermediates/epic_preop_before_labs_text_notes.csv") ) #surgery related columns missing("SERVICE_NM", "SurgService_Value")

## ICU status, length of stay, readmission, pre-processed in main actfast
admit_outcomes <- fread(paste0(update_root, "intermediates/clarity_flo_outcomes.csv") )

## death is just MRN +  date, need to make survival something relative to surgeries; preop file has already merged orlogid (surgery events) to MRN
an_records <- fread(paste0(update_root, "intermediates/an_records.csv") )
mortality_data <- fread(paste0(update_root, "mortality.csv") )

colnames(mortality_data) %<>% make.names
setorder(mortality_data, Patient.Primary.MRN, -Patient.Death.Date)
mortality_data <- mortality_data[ mortality_data[, .I[1], by="Patient.Primary.MRN"]$V1]
mortality_data <- an_records[mortality_data , on="CurrentMRN==Patient.Primary.MRN" , nomatch=NULL]
mortality_data <- mortality_data[ ,.(orlogid, survival_time=as.numeric(difftime(Patient.Death.Date, an_start, units="days"))) ]
mortality_data [, death_in_30 := survival_time < 30]

preop_covariates <- merge(mortality_data, preop_covariates, by="orlogid", all.y=T)


procedure_codes <- fread(paste0(update_root, "Report 4.csv"), sep=";" )

merged_post <- fread(  paste0(update_root, "update_2022_deident/v.1/2022_new_epic_outcomes.csv" ) )
matching_ids <- fread( paste0(update_root, "update_2022_reident/v.1/new_epic_orlogid_codes.csv" ) )


exploratory_outcomes <- matching_ids[ , .(orlogid,orlogid_encoded) ][merged_post[ , .(orlogid_encoded, PNA, AF, post_aki_status, CVA, postop_trop_high, postop_vent_duration ) ] , on="orlogid_encoded"]
exploratory_outcomes[, orlogid_encoded:= NULL]
exploratory_outcomes[ ,post_aki_status := post_aki_status >= 2 ]
exploratory_outcomes[ ,resp_failure := fcase( is.na(postop_vent_duration), FALSE, postop_vent_duration < 1, FALSE, default=TRUE) ]
exploratory_outcomes[, postop_vent_duration:= NULL]

## admission data to get discharge times
epic_admits <- fread(paste0(update_root , '2022_07_07_Clarity_Result_Set_ADT.csv'))
my_visits <- epic_admits[ADT_EVENT %chin% c("Admission","Discharge" , "Hospital Outpatient") , .(CurrentMRN = first(CurrentMRN), admt = min(EFFECTIVE_TIME), dist=max(EFFECTIVE_TIME) ) , by="CSN"]

## this ID mapping file is needed to merge the procedure codes
## it turns out that MRN + date works just as well
# epic_id2<-  fread("/research/2022_update/intermediates/fixed_an_ids.csv")
# epic_id2 <- epic_id2 [AN_3_ENC_CSN_ID != "NULL"]
# epic_id2[, MAR_WINDOW_END := lubridate::ymd_hms(MAR_WINDOW_END)]
# epic_id2 <- epic_id2[!is.na(MAR_WINDOW_END)]
# 
# 
# length(intersect(epic_id2$AN_3_ENC_CSN_ID, procedure_codes[["Encounter Epic CSN"]] %>% as.character ) )
# ## 17535
# length(intersect(an_records$CurrentMRN, procedure_codes[["Patient Primary MRN (Current)"]]  %>% as.character ) )
# ## 16k
# procedure_codes[ !is.na(`Discharge Date`) & ! is.na(`Admit Date`)] %>% nrow
setnames(procedure_codes, "Patient Primary MRN (Current)", "CurrentMRN")
## require within 1 day
procedure_codes <- merge(procedure_codes, an_records, by="CurrentMRN", all=T)[!is.na(orlogid)][!is.na(`Procedure Code Concat (1-50)`)][, `Discharge Date` := ymd_hms(`Discharge Date`)][`Discharge Date` > an_start][mdy(`Admit Date`) < an_start + ddays(1)  ]

## these are unique
# procedure_codes_mapped$orlogid %>% table %>% table
#     1 
# 20021

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
## TODO: check current file for criteria: preop in date range, test data in date range!

## just get the date of most recent smartdata
smartdata <- fread("/research/2022_update/2023_01_SmartData.csv", sep = '^')
setnames(smartdata, "SMRTDTA_ELEM_VALUE" , "Value" )
## to get eval date, this element is the METs, which should always be present (Even for phone evals)
smartdata <-  smartdata[ELEMENT_ID == "EPIC#37999"][Value != "" ]
smartdata <- smartdata[!is.na(CUR_VALUE_DATETIME)]
smartdata <- smartdata[CUR_VALUE_DATETIME > lubridate::ymd("2018-08-31")]

an_ids <- fread("/research/2022_update/intermediates/fixed_an_ids.csv")
## this only goes through 3/2022!


smartdata <- an_ids[, .(orlogid=LOG_ID, PAT_LINK_ID=AN_PAT_ID, an_start=ymd_hms(AN_START_DATETIME) )][smartdata, on="PAT_LINK_ID" , allow.cartesian=TRUE][an_start > CUR_VALUE_DATETIME][ CUR_VALUE_DATETIME + lubridate::ddays(90) > an_start  ]

preop_dates <- smartdata[ ,.(preopdate=max(CUR_VALUE_DATETIME) ), by="orlogid"] 

rm(smartdata)
  
#################
## and the SBT date
#################

flow_files <- update_root %>% paste0(c(
"2023_02_Flowsheets_202101.csv", 
"2023_02_Flowsheets_202102.csv", 
"2023_02_Flowsheets_202103.csv", 
"2023_02_Flowsheets_202104.csv", 
"2023_02_Flowsheets_202105.csv", 
"2023_02_Flowsheets_202106.csv", 
"2023_02_Flowsheets_202107.csv", 
"2023_02_Flowsheets_202108.csv", 
"2023_02_Flowsheets_202109.csv", 
"2023_02_Flowsheets_2021Q4.csv",
"2023_02_Flowsheets_2022Q1.csv",
"2023_02_Flowsheets_2022Q2.csv"
))

used_pre_flow <-  fread( paste0(update_root, "intermediates/used_pre_flow.csv") )
used_pre_flow <- used_pre_flow[ used_pre_flow[,.I[1], by='FLO_MEAS_ID']$V1]

tempfun <- function(x) {used_pre_flow[ fread(x,sep="^"), on='FLO_MEAS_ID', nomatch=NULL][MEASURE_NAME %chin% c("Barthel index score" , "Short Blessed Total Score", "AD8 Dementia Score" ) ]}
barthel <- rbindlist(lapply(flow_files, tempfun))
setnames(barthel, "PAT_MRN_ID", "CurrentMRN") 

# clarity_flow <- rbindlist(lapply(flow_files, fread, sep="^"))
# setnames(clarity_flow, "PAT_MRN_ID", "CurrentMRN") 
# setnames(an_records, 'an_start', 'AnestStart')
# setnames(an_records, 'an_stop', 'AnestStop')
# 
# clarity_flow <- used_pre_flow[clarity_flow, on='FLO_MEAS_ID', nomatch=NULL]
# 
# barthel <- clarity_flow[MEASURE_NAME %chin% c("Barthel index score" , "Short Blessed Total Score", "AD8 Dementia Score" ) ]
# rm(clarity_flow)


barthel <-  an_records[barthel , on="CurrentMRN", allow.cartesian=TRUE, nomatch=NULL]
setnames(barthel, 'an_start', 'AnestStart')
setnames(barthel, 'an_stop', 'AnestStop')
barthel <- barthel[RECORDED_TIME < AnestStop][ RECORDED_TIME > AnestStop - ddays(90)]

setorder(barthel, orlogid, MEASURE_NAME, -RECORDED_TIME)
barthel <- barthel[ barthel[, .I[1], by=.(orlogid, MEASURE_NAME) ]$V1 ] ## this step is technically unnecessary, duplicative
barthel <- dcast(barthel, orlogid~MEASURE_NAME ,value.var="MEAS_VALUE", drop=FALSE, fun=dplyr::first )
setnames(barthel, c("Barthel index score" , "Short Blessed Total Score", "AD8 Dementia Score" ), c("low_barthel" , "SBT", "AD8" ))



if(FALSE) {
cog_dates <- preop_covariates[ ,.(MRN=`Patient Primary MRN`, orlogid, AnestStart, AnestStop)][barthel, on="MRN", allow.cartesian=TRUE ][Recorded_Time<AnestStart ]
cog_dates <- cog_dates[MEASURE_NAME %chin% c("Short Blessed Total Score" , "AD8 Dementia Score") , .(cogdate=last(Recorded_Time)) ,  by="orlogid"]
} ## I don't think this is needed because of the restriction above


## filter to just the variables of interest
merged_data <- processed_preop_screen[, .(orlogid, AD8=`AD8 Dementia Score`, SBT=`Short Blessed Total Score`, low_barthel=`Barthel index score`<100 , preop_los) ] %>% 
merge(admit_outcomes[ , .(orlogid,ICU,  postop_los, readmit=fcase(is.na(readmission_survival) , FALSE, readmission_survival>30, FALSE, default=TRUE ) ) ] , by="orlogid") %>%
merge(preop_covariates[, .(orlogid, death=death_in_30 , RACE , Sex , age, COPD , CAD , CKD , CHF , CVA_Stroke , cancerStatus, Diabetes, FunctionalCapacity, AFIB, low_functional_capacity=FunctionalCapacity<3 , BMI=WEIGHT_IN_KG/((HEIGHT_IN_INCHES/39.3701)^2) , HTN, Dementia_MildCognitiveImpairment) ] , by="orlogid" )
## i merge the discharge dates later, will transform the death dates to an outcome then
merged_data <- merged_data[age>=64.5]

figure1 <- data.frame(Stage="surgeries age > 65", N=merged_data$orlogid %>% uniqueN , deltaN=NA_real_)


# procedure_codes <- epic_id2[ , .(orlogid=LOG_ID, CSN=AN_3_ENC_CSN_ID, AN_PROC_NAME, AN_DATE)][procedure_codes[, .(CSN=`Encounter Epic CSN` %>% as.character, codes=`Procedure Code Concat (1-50)`, dist=`Discharge Date`, admt=`Admit Date`, dispo=`Discharge Disposition`,`Procedure Code Type`) ]  , on="CSN"]
procedure_codes <- procedure_codes[, .(orlogid, codes=`Procedure Code Concat (1-50)`, dispo=`Discharge Disposition`,`Procedure Code Type`) ] 

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
, vats_codes = c("0BT[CDFGHJK]4Z[XZ]" , "0BB[CDFGHJKLNP][348]Z[XZ]")
)
# "51570", "51575", "51596", "51590", "51595", "51580", "51585", "51555", "51550", "51565", "51597"
# , "36830" , "36818", "36818", "36819", "36821", "36833", "36832", "36825"

cpt_codes <- readxl::read_xlsx("/research/sync_aw_dump/CPT codes.xlsx", skip=1) ## TODO: safely rehome this file
cpt_codes %<>% dplyr::filter(is.na(Exclude))
cpt_codes <- as.data.frame(cpt_codes)
setDT(cpt_codes)

pattern_names <- data.table(code_names = names(code_patterns), Group= c(2,3,1,4,5,6,9,10,7,8,12,13,14,15,11 ))
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

# code_patterns$ueavfist_codes <- c(code_patterns$ueavfist_codes, cpt_code_pattern[cpt_code_pattern$group==15 , "code", drop=TRUE] ) %>% unique
code_patterns$ueavfist_codes <- NULL ## not interested in this group

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
procedure_codes <- procedure_codes[is.finite(included)] [included >0 ]
## name consistency with older code
# setnames(procedure_codes, names(code_patterns), names(code_patterns)%>% sub(pattern="_codes", replacement="") )



figure1 <- rbind(figure1 , data.frame(Stage="qualifying procedures", N=length(intersect(procedure_codes$orlogid , merged_data$orlogid ) ), deltaN=NA_real_) )


merged_data2 <- merged_data %>% merge(procedure_codes[, .(orlogid, gut_codes, stomach_codes, chole_codes, panc_codes, hyster_codes, lumbar_codes,shoulder_codes, hiatalHernia_codes, knee_codes, totalHip_codes, neph_codes,   prost_codes, bladder_codes, vats_codes, dispo) ] , by="orlogid")

merged_data2 %<>% merge(an_records[,.(AnestStart=an_start, orlogid)], by="orlogid" )
merged_data2 %<>% merge(preop_dates , by="orlogid")
merged_data2 <- merged_data2[preopdate > AnestStart - ddays(90)]

figure1 <- rbind(figure1 , data.frame(Stage="eval w/i 90 days", N=merged_data2$orlogid %>% uniqueN , deltaN=NA_real_) )

# merged_data2 %<>% merge(cog_dates , by="orlogid")
# merged_data2 <- merged_data2[cogdate > AnestStart - ddays(90)]
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


## use pre-computed los
if(FALSE) {
my_visits<- my_visits[ preop_covariates[ ,.(CurrentMRN=`Patient Primary MRN`, orlogid, AnestStop)] , allow.cartesian=TRUE, nomatch=NULL, on="CurrentMRN"]

los_data <- my_visits[dist>AnestStop ,.(los = min(as.numeric(difftime(dist,AnestStop) ) ) )  , by="orlogid" ]
los_data[, los:=as.numeric(los)]
merged_data2 %<>% merge(los_data, all.x=TRUE, by="orlogid")
readmit_data <- my_visits[admt>AnestStop & admt<AnestStop +ddays(30) ,.(readmit = TRUE ) , by="orlogid" ]
merged_data2 %<>% merge(readmit_data, all.x=TRUE, by="orlogid")
merged_data2[is.na(readmit)  , readmit:= FALSE] 
}
# setnames(merged_data2, "postop_los", "los")
merged_data2[, los := fcase(is.na(postop_los), preop_los, is.na(preop_los), postop_los, is.finite(preop_los+postop_los), preop_los+postop_los   )]

merged_data2[ , .(  gut_codes, stomach_codes, chole_codes, panc_codes, hyster_codes, lumbar_codes,shoulder_codes, hiatalHernia_codes, knee_codes, totalHip_codes, neph_codes,   prost_codes, bladder_codes, vats_codes)] %>% sapply(sum) -> code_counts
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
setnames(merged_data2, "CVA_Stroke", "CVA(TIA)")                                                                               
merged_data2[, year := year(AnestStart)]

encode_onehot <- function(x, colname_prefix = "", colname_suffix = "") {
  if (!is.factor(x)) {
      x <- as.factor(x)
  }
  encoding_matrix <- contrasts(x, contrasts = FALSE)
  encoded_data <- encoding_matrix[as.integer(x),]
  colnames(encoded_data) <- paste0(colname_prefix, colnames(encoded_data), colname_suffix)
  encoded_data
}


merged_data2 <- data.table(merged_data2, encode_onehot(merged_data2$year, colname_prefix = "year_") %>% data.table)

saveRDS(merged_data2, "/output/merged_data2022.RDS" )
saveRDS(figure1, "/output/figure1_2022.RDS" )
# 
# other_data <- readRDS("/research/sync_aw_dump/old_epic_cog.RDS")
# 
# setdiff( colnames(other_data), colnames(merged_data2) )
# setdiff(  colnames(merged_data2), colnames(other_data) )
# 
# all_data <- rbind(merged_data2, other_data, fill=T)
# for( thisvar in colnames(all_data ) %>% grep(pattern="year_", value=T)  ) {
# set(all_data, j=thisvar, i= which(is.na(all_data[[thisvar]] ) ), 0)
# }
# 
# 
