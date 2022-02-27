## it is a good practice to put the environment you are running code it up front (or in a readme)
## docker run -it --rm -v '/mnt/ris/:/research/' cryanking/cognitioncheck:1.0 /bin/bash
#Analysis-1
## I typically call all my libraries up-front. It makes the code easier to parse
## calling library(tidyverse) is ok, but it's often good to be explicit about the actual libraries you're using

library(readxl)
library(magrittr)
library(dplyr)
library(forcats)
library(readr)
library(lubridate)
library(table1)

## library(lubridate)
## library(janitor)

## this is just style, but if I need exactly 1 function in a package I just check that it is installed instead of cluttering the namespace. This is not as safe as using require() because it doesn't check for version conflicts
find.package("janitor" )
#find.package("openssl" ) ## you don't really need this, I use it to transform name+dob+salt into an id. 



## if you are running in a container, it is nice to use absolute file paths
na_zero <- function(x) {  ifelse(is.na(x), 0, x) }
Signals <- read_excel("2020_01_15_Gregory_Cognative_Dysfunction_Data.xlsx", 
                      sheet = "Signals", na = "NULL", col_types = c("numeric", 
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


## you should always read identifier numbers as strings. You will occasionally run into long numbers that get mangled into floats or short-int defaults which don't quite hold a 10 digit number.

CDS_ADT <- read_csv("CDS ADT.csv", col_types = cols(REFERENCE_NO = col_character(), 
                                                    REG_NO = col_character(), FACILITY_CONCEPT_ID = col_character()))


## I made a folder on RIS to hold this
id_links <- read_excel("../ActFastData/ActFast_Epic_Flow/2021_01_ActFast_Identifiers.xlsx", na= c("", "NULL"), col_types=c("text","text","text","text","date"), sheet="MetaVision" )

## link a CPAP event to a surgery event
cpap_vs_surgery <- read_excel("2020_01_King_ProcedureCodes_MostRecentCPAP.xlsx" , sheet=2 , col_types ="text")

TextSignals <- read_excel("2020_01_15_Gregory_Cognative_Dysfunction_Data.xlsx", 
                          sheet = "TextSignals",na = "NULL", col_types = c("numeric", 
                                                                           "numeric", "numeric", "numeric", "numeric", 
                                                                           "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                                           "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                                           "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                                           "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                                           "numeric", "numeric", "numeric", "numeric", "numeric"))

FreeText <- read_excel("2020_01_15_Gregory_Cognative_Dysfunction_Data.xlsx", 
                       sheet = "FreeText", na = "NULL", col_types = c("numeric", 
                                                                      "numeric", "text", "text"))

Dictionary <- read_excel("2020_01_15_Gregory_Cognative_Dysfunction_Data.xlsx", 
                         sheet = "Dictionary", na = "NULL", col_types = c("text", 
                                                                          "numeric","numeric","text"))

ProcedureCodes <- read_excel("2020_01_15_Gregory_Cognative_Dysfunction_Data.xlsx", 
                             sheet = "ProcedureCodes",na = "NULL", col_types = c("numeric", 
                                                                                 "text", "text", "numeric", "text"))


other <- read_excel("2020_01_15_Gregory_Cognative_Dysfunction_Data.xlsx", 
                    sheet = "Other", na = "NULL", col_types = c("numeric", 
                                                                "text", "numeric"))

## did you have a plan for this exclusion criteria? :
## "For patients who underwent multiple procedures within the study period, only include the first surgical procedure."

# > FreeText %>% group_by(MRN) %>% summarize(nvisit = n_distinct(CPAPPatientID)) %>% pull("nvisit") %>% table
# `summarise()` ungrouping output (override with `.groups` argument)
# .
#     1     2     3     4     5     6     7     8     9    11 
# 27878  3736   579   114    37    18     9     8     1     3 

## find a set of indicies and re-use it
## ideally, this would be done after filtering only to cases with outcomes and exposures. I'll let you handle that.
first_visit <- FreeText %>% select(CPAPPatientID,MRN) %>% inner_join(Signals %>% select(CPAPPatientID, Surgery_Date), by = "CPAPPatientID") %>% group_by(MRN) %>% slice_min( order_by="Surgery_Date", n=1 , with_ties=FALSE) %>% ungroup

# first_visit %>% group_by(MRN) %>% summarize(nvisit = n_distinct(CPAPPatientID)) %>% pull("nvisit") %>% table
#     1 
# 32383 


Signals %<>% semi_join(first_visit, by= "CPAPPatientID", na_matches="never")
TextSignals %<>% semi_join(first_visit, by= "CPAPPatientID", na_matches="never")
FreeText %<>% semi_join(first_visit, by= "CPAPPatientID", na_matches="never")
ProcedureCodes %<>% semi_join(first_visit, by= "CPAPPatientID", na_matches="never")
other %<>% semi_join(first_visit, by= "CPAPPatientID", na_matches="never")

## part of working with pipes is to avoid naming lots of intermediates that you never use again.
## R doesn't garbage collect except for anonymous - as long as a memory location has a name pointing to it it will stick around
# inclusion <- filter(Signals, Age_at_CPAP >= 65)
# inclusion1 <- inclusion%>% filter( CPAP_Date >= as.Date("2013-01-01") & CPAP_Date <= as.Date("2018-06-30") )
# inclusion2<- inclusion1 %>% filter(!is.na(AD8) ) %>% filter(!is.na(SBT) )

# Procedure1 = filter(ProcedureCodes, ICD_CODE_VERSION == "9-CM" | ICD_CODE_VERSION =="10-PCS")
# Procedure2<- Procedure1[Procedure1$ICD_PROCEDURE_CODE %in% c("0FT44ZZ","0DTN0ZZ", "0DB64Z3", "0FBG0ZZ", "0FBG3ZZ","0UT90ZZ", "0UB74ZZ", "0SG00A0","0SRC0J9","0SRB04Z","0RRJ00Z","0DQ53ZZ","0FT20ZZ","0TT10ZZ","0VT08ZZ","0TTB0ZZ","B50W","51.23","45.8","43.82","52.7","52.0","68.4","68.5","68.9","68.49","81.06","81.54","81.51","81.80","53.9","32.41","32.49","55.4","60.5","57.71","39.52","39.53"),]

#Procedure4 = Procedure2[!duplicated(Procedure2$CPAPPatientID),]



signals_procedure <- Signals %>% filter(Age_at_CPAP >= 65)   %>% 
 filter(!is.na(AD8) ) %>% filter(!is.na(SBT) ) %>% 
 left_join(ProcedureCodes %>% filter(ICD_CODE_VERSION == "9-CM" | ICD_CODE_VERSION =="10-PCS") %>% filter( ICD_PROCEDURE_CODE %in% c("0FT44ZZ","0DTN0ZZ", "0DB64Z3", "0FBG0ZZ", "0FBG3ZZ","0UT90ZZ", "0UB74ZZ", "0SG00A0", "0SRC0J9","0SRB04Z","0RRJ00Z","0DQ53ZZ","0FT20ZZ","0TT10ZZ","0VT08ZZ","0TTB0ZZ","B50W","51.23","45.8","43.82","52.7","52.0","68.4","68.5","68.9","68.49","81.06","81.54","81.51","81.80","53.9","32.41","32.49","55.4","60.5","57.71","39.52","39.53")) %>% distinct(CPAPPatientID, .keep_all=T), by="CPAPPatientID")

## this isn't used
# signals_procedure_text <- signals_procedure %>% left_join(TextSignals, by= "CPAPPatientID")


## this is a no-op
## please say what printed output is for

# > signals_procedure_text %>% class
# [1] "tbl_df"     "tbl"        "data.frame"
# df<- data.frame(signals_procedure_text)
#df %>% group_by(CPAPPatientID, Surgery.type, Surgery_Date) %>% summarise(n = n()) -> aggData
#aggData %>% group_by(CPAPPatientID) %>% summarise(n = n()) -> aggData1
#df %>% group_by(CPAPPatientID) %>% summarise(n = n()) -> uniquedf



TextSignals %<>% select(one_of("CPAPPatientID", "Sex", "Race", "Ethnicity", "Alcohol use", "Drinks/week", "Functional capacity", "Dialysis history", "Cirrhosis etiology", "Surgical Service") )
TextSignals %<>% (janitor::clean_names)
## I assume you intended to save this
TextSignals$race %<>% as.factor %>% fct_other(keep=c("7","9")) %>% fct_recode(White="9", Black="7") %>% fct_explicit_na("Missing") %>% relevel(ref="White")
TextSignals %<>% mutate(current_heavy = ifelse(sex==1, drinks_week > 16, drinks_week >10)%>% as.factor %>% fct_explicit_na ) %>% select(-one_of("drinks_week"))
TextSignals$alcohol_use %<>% is_in(4:6) %>% as.factor %>% fct_explicit_na
TextSignals$dialysis_history %<>% is_in(76:80)%>% as.factor %>% fct_explicit_na
TextSignals$functional_capacity %<>% is_in(9:11)%>% as.factor %>% fct_explicit_na
TextSignals$cirrhosis_etiology %<>% is_in(107:113)%>% as.factor %>% fct_explicit_na


TextSignals %<>% rename(prior_heavy_alcohol=alcohol_use, low_functional_capacity=functional_capacity, cirrhosis=cirrhosis_etiology)#, ESRD =dialysis_history)

signals_procedure %<>% left_join(TextSignals  , by=c("CPAPPatientID"="cpap_patient_id")) %>% mutate(dialysis=na_zero(Dialysis))

signals_procedure %<>% mutate(male_sex = sex==1) 

## I would strongly recommend that anything you intend someone to see, save the output to a location, unless you are producing a markdown / notebook. 
## there is a built-in for this task. You'll see me use the pull() or extract2() function in pipes sometimes instead of df$var; it is just a habit being in the middle of pipes.
# signals_procedure %>% group_by(male_sex) %>% summarise(percent = 100 * n() / nrow(signals_procedure))
# signals_procedure %>% pull("male_sex") %>% table(useNA='a') %>% prop.table %>% multiply_by(100)

signals_procedure %<>% mutate(low_barthel = Barthel %>% is_less_than(100) %>% as.factor %>% fct_explicit_na ) 

# signals_procedure %<>% mutate(BMI = ifelse(between(BMI, 15, 60 ), BMI, mean(BMI, na.rm=TRUE))) 
signals_procedure %<>% mutate(BMI = ifelse(between(BMI, 15, 60 ), BMI, NA_real_)) 

signals_procedure %<>% mutate(Age_at_CPAP = ifelse(is_greater_than(Age_at_CPAP, 64.9 ), Age_at_CPAP, NA_real_)) 

# signals_procedure %>% select(AD8, SBT) %>% mutate_all(is.na) %>% table

signals_procedure %<>% mutate(bmi_cats = cut(BMI, breaks=c(14, 18.5, 25, 30, 35, 40, 61), right=FALSE ))


## analysis 1

filter_the_nullvalues <- . %>% filter(!is.na(AD8) ) %>% filter(!is.na(SBT) )
make_abnl <- . %>% mutate(abn_cog = (AD8>=2) | (SBT >=5))


conditions <- . %>% mutate(CVA = `Cerebrovascular disease` | `Cerebrovascular disease, stroke, or TIA` | CVA | TIA) %>% mutate_at(vars(one_of("Coronary artery disease", "Congestive heart failure", "Atrial fibrillation or flutter history" , "COPD" , "Asthma" , "Peripheral artery disease" , "Diabetes mellitus" , "Current cancer", "CVA" ,"Hypertension")) , na_zero) %>% 
  mutate_at(vars(one_of("Coronary artery disease", "Congestive heart failure", "Atrial fibrillation or flutter history" , "COPD" , "Asthma" , "Peripheral artery disease" , "Diabetes mellitus" , "Current cancer", "CVA","prior_heavy_alcohol","low_functional_capacity" , "cirrhosis"
                        , "dialysis","Hypertension")) , as.logical)


Analysis1table <- signals_procedure %>% filter_the_nullvalues %>% make_abnl %>% conditions %>% mutate(abn_cog = abn_cog %>% as.factor %>% fct_recode(abnl="TRUE",nl="FALSE")) %>%
  table1(~Age_at_CPAP + race + male_sex + bmi_cats  + `Coronary artery disease` + low_barthel + `Congestive heart failure` + `Atrial fibrillation or flutter history` + CVA + COPD + Asthma + `Peripheral artery disease` + `Diabetes mellitus` + `Current cancer`+current_heavy+prior_heavy_alcohol+low_functional_capacity+cirrhosis+dialysis|abn_cog, data=.)

cat(file="Analysis1.html",Analysis1table)


###############
##Analysis-2
###############

TextSignals <- read_excel("2020_01_15_Gregory_Cognative_Dysfunction_Data.xlsx", 
                          sheet = "TextSignals",na = "NULL", col_types = c("numeric", 
                                                                           "numeric", "numeric", "numeric", "numeric", 
                                                                           "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                                           "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                                           "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                                           "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                                           "numeric", "numeric", "numeric", "numeric", "numeric"))

Text1<- TextSignals%>%filter(`Home status` %in% c(2,3))

#part-1
## again, just a style choice, but I think it is cleaner / more readable to define the outcome then filter
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
  "Discharged/transferred to a federal health care facility with a planned acute care hospital inpatient readmission")

home_list <-c(
  "Discharge to home or self care (routine discharge)",
  "Discharged/transferred to home - organized home health service",
  "DSCH/TRNF TO HOME-HOME HEALTH SKILLED CARE",
  "DSCH/TRNF TO HOME W/ HOME HEALTH CARE WITH PLAN READMIT",
  "Discharge to home or self care",
  "DISCHARGE TO HOME OR SELF CARE"
)  
  
other %<>% mutate( dc_status = case_when( 
  is.na(UB_DISCHARGE_DISPOSITION_DESCRIPTION ) ~ "missing",
  grepl(UB_DISCHARGE_DISPOSITION_DESCRIPTION, pattern="hospice", ignore.case=T ) ~ "hospice or death",
  grepl(UB_DISCHARGE_DISPOSITION_DESCRIPTION, pattern="expired", ignore.case=T) ~ "hospice or death",
  UB_DISCHARGE_DISPOSITION_DESCRIPTION %in% home_list  ~ "home" ,
  UB_DISCHARGE_DISPOSITION_DESCRIPTION %in% facility_list ~ "facility",
  TRUE ~ "other" ) )

## no important categories aren't caught
# other %>% filter(dc_status == "other") %>% pull("UB_DISCHARGE_DISPOSITION_DESCRIPTION") %>% table %>% sort  

## it isn't clear to me what this is supposed to be  
discharged_patient_other_home <- other %>% filter(!UB_DISCHARGE_DISPOSITION_DESCRIPTION %in% c("Discharge to home or self care (routine discharge)","Discharged/transferred to home - organized home health service","Discharged/transferred to Skilled Nursing Facility","Discharged/transferred to Hospice - home","DSCH/TRNF TO HOME-HOME HEALTH SKILLED CARE","DISCHARGE TO HOSPICE/HOME","DSCH/TRNF TO HOME W/ HOME HEALTH CARE WITH PLAN READMIT","Discharge to home or self care","DISCHARGE TO HOME OR SELF CARE")) %>% right_join(Text1, by="CPAPPatientID")


## if you're going to label sections, do so meaningfully
#part-2

## In general, it is a mistake to assume that two ID numbers mean the same thing.
## why did you switch to using merge instead of joins? 
# mergedata <- merge(Signals, CDS_ADT, by.x = c("CPAPPatientID"), by.y = ("REFERENCE_NO"), all.y=TRUE)

## a helper function to see how many distinct matches exist
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

num_overlap(CDS_ADT, first_visit ) ## nothing matches
num_overlap(CDS_ADT, id_links ) ## shows that CDS_ADT$REG_NO is id_links$PAN
num_overlap(cpap_vs_surgery,first_visit) ## shows first_visit$CPAPPatientID is cpap_vs_surgery$CPAP_PatientID
num_overlap(cpap_vs_surgery, id_links ) ## shows that cpap_vs_surgery$Surg_PatientID is id_links$PatientID
## you can also link on MRN, but then you have to add a step of matching on dates 
# num_overlap(id_links,first_visit)

## PAN is unique to a hospitalization, not a patient, you need MRN or MPI for that
# > id_links %>% group_by(MRN) %>% summarize(nd = n_distinct(PAN)) %>% select(nd) %>% table
# .
#     1     2     3     4     5     6     7     8     9    10    11    12    13 
# 92257 15175  3583  1154   434   197   121    53    33    24    12     8     7 
#    14    15    16    17    18    19    20    21    22    26    27 
#     6     2     1     1     2     1     2     1     1     1     1 

## walk the chain of keys
first_visit %<>% mutate(CPAPPatientID = as.character(CPAPPatientID)) %>% 
  left_join( cpap_vs_surgery %>% select(CPAP_PatientID, Surg_PatientID), by = c("CPAPPatientID"="CPAP_PatientID") ) %>% 
  left_join(id_links %>% select(PatientID,PAN ), by = c( "Surg_PatientID"="PatientID" ) ) 

joined_ADT <- first_visit %>% select(CPAPPatientID, PAN) %>% left_join(CDS_ADT, by = c("PAN" ="REG_NO" ))

## again, just cleaning this. You can break up long lines in R as long as an expression doesn't finish. Because %>% is a binary operator on expressions, if it is the last thing on a line the expression continues. I think that's true of the new |> operator as well
## I mostly use the magrittr pipe %>% instead of the R.4 |> because I have a lot of legacy code that uses it; there are rare fucntional differences
filtered_mereged_data1 <- joined_ADT %>% left_join( Signals %>% mutate(CPAPPatientID = as.character(CPAPPatientID)),  by =c("CPAPPatientID")) %>% 
 filter(!is.na(NAME)) %>% filter(NAME != "") %>%
 filter(Age_at_CPAP >= 65) %>% filter(between(as.Date(CPAP_Date),ymd("2013-01-01"), ymd("2018-06-30") ) ) %>% 
 filter(!is.na(AD8) ) %>% filter(!is.na(SBT) )

## note that there are multiple room numbers per patient
 
 # print(as_date(filtered_mereged_data1$CPAP_Date,tz=NULL))

 ## this is not the right format
# filtered_mereged_data1$Discharge<- as_date(filtered_mereged_data1$ROOM_END_DATE, "%Y-%m-%d", tz=NULL)
# > joined_ADT$ROOM_START_DATE %>% head
# [1] "2015-02-16-09.30.00.380000" "2015-02-16-17.47.00.570000"
# [3] "2013-11-11-06.56.00.400000" "2013-11-11-06.56.00.430000"
# [5] NA                           NA  
## You also want to be careful with specifying the TZ vs not. Usually best to specify it everywhere, in this case I know that no inputs specify a TZ
filtered_mereged_data1 %<>% mutate(Discharge = ymd_hms(ROOM_END_DATE) )

## let's see if we can create admit and discharge times (we could have done this before joining)
## I think you will find this much easier to organize that way
## (this is much faster in data.table but tolerable in tidyverse)
filtered_mereged_data1 %>% group_by(PAN) %>% summarize(admit=min(ymd_hms(ROOM_START_DATE), na.rm=T ), discharge = max(ymd_hms(ROOM_END_DATE), na.rm=T ), CPAPPatientID = first(CPAPPatientID) ) -> temp

## I have another file with admit / dc events
los_data <- read_csv("2020_02_MV_LoS.csv")

los_data %<>% mutate(PatientID = as.character(PatientID)) %>% left_join(first_visit , by = c("PatientID"="Surg_PatientID" ) )

## these have a lot of concordance
temp %>% left_join(los_data, by = "CPAPPatientID" ) %>% select(CPAPPatientID, admit, ADMIT_TMSTP )
#  1 1145349       2016-07-21 05:53:00 2016-07-21 05:53:00
#  2 1145338       NA NA               2016-07-26 10:39:00
#  3 1138906       2016-07-27 09:05:00 2016-07-27 09:05:00
#  4 1157519       2016-07-07 11:39:00 2016-07-07 05:48:00
#  5 1145707       2016-06-13 12:24:00 2016-06-13 12:24:00
#  6 1140280       2016-06-16 07:51:00 NA                 
#  7 1140849       2016-06-14 06:32:00 NA                 
#  8 1138088       2016-06-27 15:31:00 2016-06-27 09:06:00
#  9 1143023       NA NA               2016-06-20 05:53:00
# 10 1142835       NA NA               2016-07-18 11:43:00


##########
## I'll let you start over here since there wasn't a good link
##########

filtered_mereged_data1$Admission<-as_date(filtered_mereged_data1$CPAP_Date,tz=NULL) ## not correct - the CPAP date is substantially before admit in most cases (and occasionally after)
filtered_mereged_data1$SurgeryDate<-as_date(filtered_mereged_data1$Surgery_Date,tz=NULL)

library(data.table)
temp_dataset <- data.table(filtered_mereged_data1)

setkey(temp_dataset, CPAPPatientID, Admission, SurgeryDate)


## Always give difftime() a units argument, otherwise the auto select might not be what you want
final_dataset <- temp_dataset[ , Daydiff_AdmissionDischarge := as.numeric(difftime(Discharge, shift(Admission, n = 1L, fill = 0, type = "lag"))), 
                               by = "CPAPPatientID"]



final_dataset$Daydiff_AdmissionDischarge <- ifelse(final_dataset$Daydiff_AdmissionDischarge>=10000, NA, final_dataset$Daydiff_AdmissionDischarge)




final_dataset2 <- final_dataset[final_dataset$Daydiff_AdmissionDischarge>0]
final_dataset3 <- final_dataset2[final_dataset2$Daydiff_AdmissionDischarge<10000]


readmited_list = ifelse(abs(final_dataset3$Daydiff_AdmissionDischarge) <= 30, 1, 0)


final_dataset4 <- final_dataset3 %>% mutate(readmited_30_days = as.numeric(readmited_list))

readmitted_within30_dataset <- final_dataset4[final_dataset4$readmited_30_days>0]



readmitted_within30_dataset = data.frame(readmitted_within30_dataset)

readmitted_within30_dataset <- subset(readmitted_within30_dataset, select=-c(`ROOM_NO`,`ROOM_START_DATE`,`ROOM_END_DATE`))



readmitted_within30_dataset <- unique(readmitted_within30_dataset)

print(readmitted_within30_dataset)

#part-3
tempe_dataset <- data.table(filtered_mereged_data1)

setkey(tempe_dataset, CPAPPatientID, Admission, SurgeryDate)

fiinal_dataset <- tempe_dataset[ , Daydiff_SurgeryDischarge := as.numeric(difftime(Discharge, shift(SurgeryDate, n = 1L, fill = 0, type = "lag"))), 
                                 by = "CPAPPatientID"]



fiinal_dataset$Daydiff_SurgeryDischarge <- ifelse(fiinal_dataset$Daydiff_SurgeryDischarge>=10000, NA, fiinal_dataset$Daydiff_SurgeryDischarge)




fiinal_dataset2 <- fiinal_dataset[fiinal_dataset$Daydiff_SurgeryDischarge>0]
fiinal_dataset3 <- fiinal_dataset2[fiinal_dataset2$Daydiff_SurgeryDischarge<10000]
fiinal_dataset4<- fiinal_dataset[fiinal_dataset$WARD %in% c("5600 ICU", "10400 NICU", "15500 ICU", "4400 ICU", "7800 BMT ICU", "7800 SICU", "8200 CCU", "83 CTICU", "8300 MICU", "8400 MICU", "8400 SICU", "8900 ICU"),]



#part-4

inhospital_mortaility<- filter(discharged_patient_other_home, UB_DISCHARGE_DISPOSITION_DESCRIPTION == "EXPIRED"|UB_DISCHARGE_DISPOSITION_DESCRIPTION == "Expired (or did not recover-Religious Non Medical Health Care Patient")
inhospital_mortaility<-inhospital_mortaility%>% rename(mortality_rate = UB_DISCHARGE_DISPOSITION_DESCRIPTION)
#part-5
library(dplyr)
library(lubridate)
inclusion = filter(Signals, Age_at_CPAP >= 65)
inclusion1 <- inclusion%>% filter( CPAP_Date >= as.Date("2013-01-01") & CPAP_Date <= as.Date("2018-06-30") )
inclusion2<- inclusion1 %>% filter(!is.na(AD8) ) %>% filter(!is.na(SBT) )

Procedure1 = filter(ProcedureCodes, ICD_CODE_VERSION == "9-CM" | ICD_CODE_VERSION =="10-PCS")
Procedure2<- Procedure1[Procedure1$ICD_PROCEDURE_CODE %in% c("0FT44ZZ","0DTN0ZZ", "0DB64Z3", "0FBG0ZZ", "0FBG3ZZ","0UT90ZZ", "0UB74ZZ", "0SG00A0","0SRC0J9","0SRB04Z","0RRJ00Z","0DQ53ZZ","0FT20ZZ","0TT10ZZ","0VT08ZZ","0TTB0ZZ","B50W","51.23","45.8","43.82","52.7","52.0","68.4","68.5","68.9","68.49","81.06","81.54","81.51","81.80","53.9","32.41","32.49","55.4","60.5","57.71","39.52","39.53"),]

#Procedure4 = Procedure2[!duplicated(Procedure2$CPAPPatientID),]

signals_procedure = inclusion2 %>% left_join(Procedure2, by="CPAPPatientID")
signals_procedure_text = signals_procedure %>% left_join(TextSignals, by= "CPAPPatientID")

df<- data.frame(signals_procedure_text)
library(dplyr)
df %>% group_by(CPAPPatientID) %>% summarise(n = n()) -> aggDataa
aggDataa %>% group_by(CPAPPatientID) %>% summarise(n = n()) -> aggDataa1



sipro = inclusion2 %>% left_join(Procedure2, by="CPAPPatientID")
#sipro$Creatinine[is.na(sipro$Creatinine)] <-0
#sipro$`Last creatinine`[is.na(sipro$`Last creatinine`)] <- 0
siproo<- sipro %>% filter(!is.na(Creatinine) ) %>% filter(!is.na(`Last creatinine`) )
#sipro3[, list("Creatinine", "Last creatinine", creatdifference = diff("Last creatinine"-"Creatinine"))]

siproo<- sipro %>% filter(!is.na(Creatinine) ) %>% filter(!is.na(`Last creatinine`) )
siproo$Creatine_diff <- (siproo$`Last creatinine` - siproo$Creatinine)

#Comaparasion b/w desired column with abnormal and normal cog
siprob<- siproo %>% mutate(AKI =case_when(Creatine_diff <= 0.3 ~ "0", Creatine_diff >= 0.4 ~ "1"))
sipro13<- filter(siprob, AD8>=2)
AKI_abnormalAD8<-length(sipro13$AD8)

sipro14<- filter(siprob, AD8<2)
AKI_normalAD8<-length(sipro14$AD8)

sipro15<-filter(siprob, SBT>=5)
AKI_abnormalSBT<-length(sipro15$SBT)

sipro16<-filter(siprob, SBT<5)
AKI_normalSBT<-length(sipro16$SBT)



filtered_text1 <- filter(other,!UB_DISCHARGE_DISPOSITION_DESCRIPTION %in% c("Discharge to home or self care (routine discharge)","Discharged/transferred to home - organized home health service","Discharged/transferred to Skilled Nursing Facility","Discharged/transferred to Hospice - home","DSCH/TRNF TO HOME-HOME HEALTH SKILLED CARE","DISCHARGE TO HOSPICE/HOME","DSCH/TRNF TO HOME W/ HOME HEALTH CARE WITH PLAN READMIT","Discharge to home or self care","DISCHARGE TO HOME OR SELF CARE", "NA"))

discharged_patient_other_home = filtered_text1 %>% left_join(Text1, by="CPAPPatientID")
discharged_patient_other_home1<- discharged_patient_other_home %>% filter(!is.na(UB_DISCHARGE_DISPOSITION_DESCRIPTION) )
sipro %>% select(AD8, SBT) %>% mutate_all(is.na) %>% table
make_abn1 <- . %>% mutate(abn_cog = (AD8>=2) | (SBT >= 5))
discharged_patient_other_home1 %>% select(UB_DISCHARGE_DISPOSITION_DESCRIPTION)

sipro1 = sipro %>% right_join(discharged_patient_other_home1, by="CPAPPatientID")
make_abn1 <- . %>% mutate(abn_cog = (AD8>=2) | (SBT >= 5))

sipro2<- filter(sipro1, AD8>=2, )
Abnormal_Homeless_discharge_AD8<-length(sipro2$AD8)
length(sipro2$AD8)

sipro3<- filter(sipro1, AD8<2)
Normal_Homeless_discharge_AD8<-length(sipro3$AD8)
length(sipro3$AD8)

sipro4<- filter(sipro1, SBT>=5)
Abnormal_Homeless_discharge_SBT<-length(sipro4$SBT)
length(sipro4$SBT)
sipro5<- filter(sipro1, SBT<5)
Normal_Homeless_discharge_SBT<- length(sipro5$SBT)
length(sipro5$SBT)

inhospital_mortaility<- filter(other, UB_DISCHARGE_DISPOSITION_DESCRIPTION == "EXPIRED"|UB_DISCHARGE_DISPOSITION_DESCRIPTION == "Expired (or did not recover - Religious Non Medical Health Care Patient)")
inhospital_mortaility<-inhospital_mortaility%>% rename(mortality_rate = UB_DISCHARGE_DISPOSITION_DESCRIPTION)

sipro8 = sipro %>% right_join(inhospital_mortaility, by="CPAPPatientID")

sipro9<- filter(sipro8, AD8>=2)
mortality_abnormalAD8<-length(sipro9$AD8)
length(sipro9$AD8)
sipro10<- filter(sipro8, AD8<2)
mortality_normalAD8<-length(sipro10$AD8)
length(sipro10$AD8)
sipro11<-filter(sipro8, SBT>=5)
mortality_abnormalSBT<-length(sipro11$SBT)
length(sipro11$SBT)
sipro12<-filter(sipro8, SBT<5)
mortality_normalSBT<-length(sipro12$SBT)
length(sipro12$SBT)


siproTIA<- sipro %>% filter(!is.na(TIA) ) 

siproTIA1<-filter(siproTIA, AD8>=2)
TIA_abnormalAD8<-length(siproTIA1$AD8)
length(siproTIA1$AD8)
siproTIA2<- filter(siproTIA, AD8<2)
TIA_normalAD8<-length(siproTIA2$AD8)
length(siproTIA2$AD8)
siproTIA3<-filter(siproTIA, SBT>=5)
TIA_abnormalSBT<-length(siproTIA3$SBT)
length(siproTIA3$SBT)
siproTIA4<-filter(siproTIA, SBT<5)
TIA_normalSBT<-length(siproTIA4$SBT)
length(siproTIA4$SBT)

siprostroke<- sipro %>% filter(!is.na(`Cerebrovascular disease, stroke, or TIA`) ) 

siprostroke1<-filter(siprostroke, AD8>=2)
stroke_abnormalAD8<-length(siprostroke1$AD8)
length(siprostroke1$AD8)
siprostroke2<- filter(siprostroke, AD8<2)
stroke_normalAD8<-length(siprostroke2$AD8)
length(siprostroke2$AD8)
siprostroke3<-filter(siprostroke, SBT>=5)
stroke_abnormalSBT<-length(siprostroke3$SBT)
length(siprostroke3$SBT)
siprostroke4<-filter(siprostroke, SBT<5)
stroke_normalSBT<-length(siprostroke4$SBT)
length(siprostroke4$SBT)

fiinal_dataset5<-filter(fiinal_dataset4, AD8>=2)
ICU_abnormalAD8<-length(fiinal_dataset5$AD8)
length(fiinal_dataset5$AD8)
fiinal_dataset6<- filter(fiinal_dataset4, AD8<2)
ICU_normalAD8<-length(fiinal_dataset6$AD8)
length(fiinal_dataset6$AD8)
fiinal_dataset7<-filter(fiinal_dataset4, SBT>=5)
ICU_abnormalSBT<-length(fiinal_dataset7$SBT)
length(fiinal_dataset7$SBT)
fiinal_dataset8<-filter(fiinal_dataset4, SBT<5)
ICU_normalSBT<-length(fiinal_dataset8$SBT)
length(fiinal_dataset8$SBT)


sipro6<- data.frame(conditions = c("Discharge_other_than_home", "Mortality_RATE", "AKI", "STroke", "tia", "ICU_admission"), Normal_AD8 =c(924,50,235,3252,1137,104), Abnormal_AD8=c(180,7,44,629,215,4), Normal_SBT=c(846,41,64,2834,1022,98), Abnormal_SBT = c(258,16,215,1047,330,10))
print(sipro6)

#Analysis-3

siproRE<- fiinal_dataset3 

siproRE1<-filter(siproRE, AD8>=2)
RE_abnormalAD8<-length(siproRE1$AD8)
length(siproRE1$AD8)
siproRE2<- filter(siproRE, AD8<2)
RE_normalAD8<-length(siproRE2$AD8)
length(siproRE2$AD8)
siproRE3<-filter(siproRE, SBT>=5)
RE_abnormalSBT<-length(siproRE3$SBT)
length(siproRE3$SBT)
siproRE4<-filter(siproRE, SBT<5)
RE_normalSBT<-length(siproRE4$SBT)
length(siproRE4$SBT)

