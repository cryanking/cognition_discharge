# docker run --rm -it -v "/mnt/ris/ActFastData/:/research/" -v '/mnt/ris/lavanya/cognition_check/:/research2/' cryanking/verse_plus R
library(magrittr)
library(data.table)
library(bit64)  
clarity_root <- '/research/ActFast_Epic_Flow/Data 202004/Clarity data/'

merged_data2 <- readRDS("/research2/merged_data2.RDS" )
load(file="/research2/cognition_cache_epic.rda")

epic_new_events <- fread(paste0(clarity_root , 'Clarity Harper Result Set Operating Room Event Log.csv'))
epic_new_events[, CPB_TIME := lubridate::ymd_hm(CPB_TIME)]
an_start_stop <- epic_new_events[ , .(CurrentMRN = min(CurrentMRN), AnestStart=min(CPB_TIME[Event=="Anesthesia Start"] ), AnestStop=min(CPB_TIME[Event=="Anesthesia Stop"] ) ) , by="orlogid"]
an_start_stop <- an_start_stop[!is.na(AnestStart) & !is.na(AnestStop)]

an_start_stop <- an_start_stop[ merged_data2[, .(orlogid)], on="orlogid" , nomatch=NULL]

all_ts <- list.files(clarity_root , full.names=TRUE, pattern="Clarity\\sHogue\\sResult\\sSet\\sFlowsheets\\s\\d")
all_ts %<>% grep(pattern="csv$", value=TRUE)

cam_holder<- vector('list', length(all_ts))


used_flow <- NULL

for(ts_index in seq_along (all_ts) ) {

# ts_index <- 1L
  epic_flow_1 <- fread(all_ts[ts_index] )
  epic_flow_1 <- epic_flow_1[ MEASURE_NAME=="Type of Residence"]
  epic_flow_1 <- an_start_stop[epic_flow_1, allow.cartesian=TRUE, on="CurrentMRN" , nomatch=NULL]

  epic_flow_1 <- epic_flow_1[ abs(as.numeric(difftime(PERFORMED , AnestStop, units="days"))) <90 ][AnestStop + lubridate::ddays(7) > PERFORMED ]

  setorder(epic_flow_1, orlogid, PERFORMED)
  cam_holder[[ts_index]] <- dcast(epic_flow_1, orlogid~MEASURE_NAME ,value.var="VALUE", drop=FALSE, fun=dplyr::last )
  used_flow <- c(used_flow ,unique(epic_flow_1$FLO_MEAS_ID) )
}

cam_holder %<>% rbindlist
used_flow %<>% unique


update_root  <- '/research/2022_update/'
an_records <- fread(paste0(update_root, "intermediates/an_records.csv") )
an_records <- an_records[ merged_data2[, .(orlogid)], on="orlogid" , nomatch=NULL]

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


tempfun <- function(x) { an_records[fread(x,sep="^")[FLO_MEAS_ID %in% c(400150L, 304031059L) ] , allow.cartesian=TRUE, on="CurrentMRN==PAT_MRN_ID" , nomatch=NULL][an_start + lubridate::ddays(7) > RECORDED_TIME  ][an_start - lubridate::ddays(90) < RECORDED_TIME ] }

barthel <- rbindlist(lapply(flow_files, tempfun))
setorder(barthel, orlogid, RECORDED_TIME )


cam_holder <- rbind(cam_holder , barthel[barthel[ , .I[1],  by="orlogid"]$V1 , .(orlogid, `Type of Residence`=MEAS_VALUE)])
cam_holder <- unique(cam_holder , by="orlogid")


saveRDS(cam_holder, "/research2/residence_epic_1.rda")
residence_data <- cam_holder
library(splines)
library(dplyr)
setnames(merged_data2 ,"CVA(TIA)", "CVA_Stroke")
## recreates the base reults
base_glm <- glm(formula=dc_home_glm$formula, data=merged_data2 %>% mutate(thisout=dispo!="home") %>% mutate(AbnCog= as.numeric(AbnCog)), family=binomial() )
## merged to residence
merged_data3 <- merge(merged_data2 , residence_data, by="orlogid")
setnames(merged_data3 , "Type of Residence", "Residence")
restricted_glm <- glm(formula=dc_home_glm$formula, data=merged_data3 %>% filter(Residence  %like% "Private residence") %>% mutate(thisout=dispo!="home") %>% mutate(AbnCog= as.numeric(AbnCog)), family=binomial() ) 

my_result <- function(x) {
c(summary(x)$coef[ "AbnCog",], confint(x, parm="AbnCog") )
}

# glm(formula=update(dc_home_glm$formula, "~.+" , data=merged_data2 %>% mutate(thisout=dispo!="home") %>% mutate(AbnCog= as.numeric(AbnCog)), family=binomial() )




