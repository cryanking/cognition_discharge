library(tidyverse) 
library(lubridate) 
library(readxl)
library(magrittr)
library(dplyr)
library(forcats)
library(readr)
library(lubridate)
library(table1)
library(data.table)
CDS_ADT <- read_csv("CDS ADT.csv", col_types = cols(REFERENCE_NO = col_number(), 
                                                    REG_NO = col_number(), FACILITY_CONCEPT_ID = col_number()))


X2020_02_MV_LoS <- read_csv("2020_02_MV_LoS.csv", 
                            col_types = cols(PatientID = col_number(), 
                                             PAN = col_double(),
                                             EMPI = col_double(),
                                             ADMIT_TMSTP = col_character(),
                                             DISCHARGE_TMSTP = col_character(),
                                             LoS_Days = col_double()
                                             ), na = "NULL")
cpap_vs_surgery <- read_excel("2020_01_King_ProcedureCodes_MostRecentCPAP.xlsx", sheet= "MostRecentCPAP",na = "NULL", col_types =c("numeric","text", "numeric", "date"))



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

new_procedure_code <- read_excel("2020_01_15_Gregory_Cognative_Dysfunction_Data.xlsx", 
                                 sheet = "ProcedureCodes",na = "NULL", col_types = c("numeric", 
                                                                                     "text", "text", "numeric", "text"))

# signals_procedure = Signals %>% left_join(CDS_ADT, by="REFERENCE_NO = CPAPPatientID")
## CRK this is never used
# mergedata <- merge(Signals, CDS_ADT, by.x = c("CPAPPatientID"), by.y = ("REFERENCE_NO"), all.y=TRUE)

## CRK this is almost right - you want to merge over (or take the last) visit before a given surgery.  
first_visit <- FreeText %>% select(CPAPPatientID,MRN) %>% inner_join(Signals %>% select(CPAPPatientID, Surgery_Date), by = "CPAPPatientID") %>% group_by(MRN) %>% slice_min( order_by="Surgery_Date", n=1 , with_ties=FALSE) %>% ungroup

Signals %<>% semi_join(first_visit, by= "CPAPPatientID", na_matches="never")
TextSignals %<>% semi_join(first_visit, by= "CPAPPatientID", na_matches="never")
FreeText %<>% semi_join(first_visit, by= "CPAPPatientID", na_matches="never")
ProcedureCodes %<>% semi_join(first_visit, by= "CPAPPatientID", na_matches="never")
other %<>% semi_join(first_visit, by= "CPAPPatientID", na_matches="never")

## CRK you probably want to apply the age filter before selecting the "first_visit"
## similarly, you'd like to coalese SBTs (backwards in time) - I haven't looked to see if there are cases were the cognition measures are absent at a later time (sometime people forget to do them)

signals_procedure <- Signals %>% filter(Age_at_CPAP >= 65)   %>% 
  filter(!is.na(AD8) ) %>% filter(!is.na(SBT) ) %>% 
  left_join(ProcedureCodes %>% filter(ICD_CODE_VERSION == "9-CM" | ICD_CODE_VERSION =="10-PCS") %>% filter( ICD_PROCEDURE_CODE %in% c("0FT44ZZ","0DTN0ZZ", "0DB64Z3", "0FBG0ZZ", "0FBG3ZZ","0UT90ZZ", "0UB74ZZ", "0SG00A0", "0SRC0J9","0SRB04Z","0RRJ00Z","0DQ53ZZ","0FT20ZZ","0TT10ZZ","0VT08ZZ","0TTB0ZZ","B50W","51.23","45.8","43.82","52.7","52.0","68.4","68.5","68.9","68.49","81.06","81.54","81.51","81.80","53.9","32.41","32.49","55.4","60.5","57.71","39.52","39.53")) %>% distinct(CPAPPatientID, .keep_all=T), by="CPAPPatientID")

## CRK little typo here (was self-joined) - best practice to specify the by
signals_procedure<- signals_procedure %>% left_join(TextSignals, by = "CPAPPatientID")
#part-1
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
  
other_than_home<- left_join(signals_procedure, other, by = "CPAPPatientID")

## making a bunch of data frames is wierd way to do this presentation - don't worry about it, but this is the tidy way. Also, never make bar plots.

other_than_home %>% filter(dc_status == "facility") %>% summarize(
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
# 1        0.173  1484        0.252  1484  0.342    1484

## what is the point of this though? don't you want to do a comparison?
## you can do expression in group_by statements, but I will often name the variable
other_than_home %>% filter(dc_status != "missing") %>% group_by(dc_status == "home") %>% summarize(
  Abnormal_AD8 = mean(AD8>=2, na.rm=T), 
  NAD8 = sum(is.finite(AD8)), 
  Abnormal_SBT = mean(SBT>=5, na.rm=T), 
  NSBT = sum(is.finite(SBT)), 
  AbnCog = mean( case_when(
    is.na(AD8) & is.na(SBT) ~ NA ,
    is.na(AD8) ~ SBT>=5 ,
    is.na(SBT) ~ AD8>=2 ,
    TRUE ~ AD8>=2 | SBT>=5 ) , na.rm=T),
  NAnyCog = sum(is.finite(AD8) | is.finite(SBT) )
)

# # A tibble: 2 x 7
#   `dc_status == "home"` Abnormal_AD8  NAD8 Abnormal_SBT  NSBT AbnCog NAnyCog
#   <lgl>                        <dbl> <int>        <dbl> <int>  <dbl>   <int>
# 1 FALSE                       0.170   1598        0.254  1598  0.340    1598
# 2 TRUE                        0.0867 18597        0.179 18597  0.224   18597

## really though you want to pivot this; it's a cohort study not a case-control study

other_than_home %>% filter(dc_status != "missing") %>% mutate(
  AbnCog =  case_when(
    is.na(AD8) & is.na(SBT) ~ NA ,
    is.na(AD8) ~ SBT>=5 ,
    is.na(SBT) ~ AD8>=2 ,
    TRUE ~ AD8>=2 | SBT>=5 ) ) %>% 
    group_by(AbnCog) %>% summarize( dc_home=mean(dc_status == "home") , n() )

## these are the numbers that would go in the results summary    
# # A tibble: 2 x 3
#   AbnCog dc_home `n()`
#   <lgl>    <dbl> <int>
# 1 FALSE    0.932 15482
# 2 TRUE     0.885  4713
    
## the other two are basically the same    
other_than_home %>% filter(dc_status != "missing") %>% mutate(
  Abnormal_AD8 =(AD8>=2) ) %>% 
    group_by(Abnormal_AD8) %>% summarize( dc_home=mean(dc_status == "home") , n() )

other_than_home %>% filter(dc_status != "missing") %>% mutate(
  Abnormal_SBT = (SBT>=5) ) %>% 
    group_by(Abnormal_SBT) %>% summarize( dc_home=mean(dc_status == "home") , n() )

## let's do the linear models while we are here

other_than_home %>% filter(dc_status != "missing") %>% mutate(
  dc_home=dc_status == "home" ,
  AbnCog =  case_when(
    is.na(AD8) & is.na(SBT) ~ NA ,
    is.na(AD8) ~ SBT>=5 ,
    is.na(SBT) ~ AD8>=2 ,
    TRUE ~ AD8>=2 | SBT>=5 ) ) %>% glm(data=., dc_home ~ AbnCog, family=binomial() ) %>% summary %>% extract2("coefficients") %>% 
    extract(2,-3, drop=F) ## I often skip printing all the junk and just pull the coefs and drop the z column

#              Estimate Std. Error     Pr(>|z|)
# AbnCogTRUE -0.5800961 0.05564326 1.901148e-25

## now to add covariates - I just did a few. Note that variable names with spaces in them have to be quoted with back-tics
other_than_home %>% filter(dc_status != "missing") %>% mutate(
  dc_home=dc_status == "home" ,
  AbnCog =  case_when(
    is.na(AD8) & is.na(SBT) ~ NA ,
    is.na(AD8) ~ SBT>=5 ,
    is.na(SBT) ~ AD8>=2 ,
    TRUE ~ AD8>=2 | SBT>=5 ) ) %>% 
    glm(data=., 
      dc_home ~ AbnCog + Hypertension + `Coronary artery disease` + `Atrial fibrillation or flutter history` +  `Cerebrovascular disease` + `Diabetes mellitus`
      , family=binomial() ) %>% summary %>% extract2("coefficients") %>% 
    extract("AbnCogTRUE",-3, drop=F) 

## adding in these adjusting variables already makes it non-significant, but doesn't change the point estimate much; it's just that abnormal cognition is closely correlated to them
#              Estimate Std. Error  Pr(>|z|)
# AbnCogTRUE -0.4176495  0.4040873 0.3013408

## you can also add splines or smoothing splines (in the MASS package), this is a b-spline with 3 degrees of freedom
other_than_home %>% filter(dc_status != "missing") %>% mutate(
  dc_home=dc_status == "home" ,
  AbnCog =  case_when(
    is.na(AD8) & is.na(SBT) ~ NA ,
    is.na(AD8) ~ SBT>=5 ,
    is.na(SBT) ~ AD8>=2 ,
    TRUE ~ AD8>=2 | SBT>=5 ) ) %>% 
    glm(data=., 
      dc_home ~ AbnCog + bs(Age_at_CPAP, 3) + Hypertension + `Coronary artery disease` + `Atrial fibrillation or flutter history` +  `Cerebrovascular disease` + `Diabetes mellitus`
      , family=binomial() ) %>% summary %>% 
      extract2("coefficients") %>% extract(,-3) %>% as.data.frame %>%
      mutate(Estimate=round(Estimate,2), `Std. Error`=round(`Std. Error`, 2), `Pr(>|z|)` = round(`Pr(>|z|)`,3) )

## just showing the multiple coefficients for age
## there are functions for pretty p-values that don't round them to zero, but weren't needed here
#                                          Estimate Std. Error Pr(>|z|)
# (Intercept)                                  1.88       1.06    0.076
# AbnCogTRUE                                  -0.28       0.42    0.509
# bs(Age_at_CPAP, 3)1                         -3.60       2.79    0.197
# bs(Age_at_CPAP, 3)2                          1.83       2.37    0.439
# bs(Age_at_CPAP, 3)3                         -4.35       2.74    0.113
# Hypertension                                -0.07       0.83    0.928
# `Coronary artery disease`                    1.30       0.73    0.075
# `Atrial fibrillation or flutter history`    -0.27       0.81    0.740
# `Cerebrovascular disease`                    0.00       0.72    0.996
# `Diabetes mellitus`                          0.41       0.77    0.598



## you can generate this formula as a string
    
    


## one of the things that is awkward about R is how scoping works and when it does "standard" vs "non-standard" evaluation 
## dplyr::filter will look for names in the names of its first argument (passed by %>% ), a form of non-standard evaluation (because the expression dc_status isn't evaluated before looking in the names of of its first argument)
## but if it doesn't find them it will go up into the parent namespace, as in thise case where it didn't find "other_than_home$dc_status" as a name in other_than_home
## non-standard evaluation (when you can pass a symbol like `dc_status` vs when you have to pass a string "dc_status") is complicated, especially when you want to, for example, put a variable name in a variable!

# other_than_home<-other_than_home %>% filter(other_than_home$dc_status == "facility") 


## skip all this

# Abnormal_AD8<- filter(other_than_home, AD8>=2)
# otherthanHOME_abnormalAD8<-length(Abnormal_AD8$AD8)
# 
# Normal_AD8<- filter(other_than_home, AD8<2)
# otherthanHOME_normalAD8<-length(Normal_AD8$AD8)
# 
# Abnormal_SBT<-filter(other_than_home, SBT>=5)
# otherthanHOME_abnormalSBT<-length(Abnormal_SBT$SBT)
# 
# Normal_SBT<-filter(other_than_home, SBT<5)
# otherthanHOME_normalSBT<-length(Normal_SBT$SBT)
# 
# titles <- c("otherthanHOME_abnormalAD8", "otherthanHOME_normalAD8", "otherthanHOME_abnormalSBT", "otherthanHOME_normalSBT")
# values <- c(otherthanHOME_abnormalAD8, otherthanHOME_normalAD8, otherthanHOME_abnormalSBT, otherthanHOME_normalSBT)
# out_of <- c(length(other_than_home$dc_status), length(other_than_home$dc_status), length(other_than_home$dc_status), length(other_than_home$dc_status))
# df <- data.frame(titles, values, out_of)
# df
# 
# labels <- c("abnormalAD8", "normalAD8", "abnormalSBT", "normalSBT")
# values <- c(otherthanHOME_abnormalAD8, otherthanHOME_normalAD8, otherthanHOME_abnormalSBT, otherthanHOME_normalSBT)
# png(file = "Discharge other than home")
# barplot(values,names.arg= labels,xlab="cognition",ylab="values",col="blue",
#         main="other than home chat",border="red")
# dev.off()



#part-2
surgery <- left_join(cpap_vs_surgery, X2020_02_MV_LoS, by = c("Surg_PatientID" = "PatientID") )
filtered_mereged_data1<- left_join(signals_procedure, surgery, by = c("CPAPPatientID" = "CPAP_PatientID"))
filtered_mereged_data1$Admission<-as_date(ymd_hms(filtered_mereged_data1$ADMIT_TMSTP,tz=NULL)) 
filtered_mereged_data1$Discharge<-as_date(ymd_hms(filtered_mereged_data1$DISCHARGE_TMSTP,tz=NULL))
filtered_mereged_data1$Surgery<-as_date(filtered_mereged_data1$Surgery_Date,tz=NULL)
filtered_mereged_data1 %<>% mutate(new_id = openssl::md5(paste(NAME, DoB)))
temp_dataset <- data.table(filtered_mereged_data1)
setkey(temp_dataset, new_id, Admission,Surgery, Discharge)
final_dataset <- temp_dataset[ , Daydiff_AdmissionDischarge := as.numeric(difftime(Discharge, shift(Admission, n = 1L, fill = 0, type = "lag"), units="days")), by = "new_id"]
final_dataset$Daydiff_AdmissionDischarge <- ifelse(final_dataset$Daydiff_AdmissionDischarge>=10000, NA, final_dataset$Daydiff_AdmissionDischarge)
readmited_list = ifelse(abs(final_dataset$Daydiff_AdmissionDischarge) <= 30, 1, 0)
final_dataset2 <- final_dataset %>% mutate(readmited_30_days = as.numeric(readmited_list)) 
readmitted_within30_dataset <- final_dataset2[final_dataset2$readmited_30_days>0]

Abnormal_AD8<- filter(readmitted_within30_dataset, AD8>=2)
readmitted_abnormalAD8<-length(Abnormal_AD8$AD8)

Normal_AD8<- filter(readmitted_within30_dataset, AD8<2)
readmitted_normalAD8<-length(Normal_AD8$AD8)

Abnormal_SBT<-filter(readmitted_within30_dataset, SBT>=5)
readmitted_abnormalSBT<-length(Abnormal_SBT$SBT)

Normal_SBT<-filter(readmitted_within30_dataset, SBT<5)
readmitted_normalSBT<-length(Normal_SBT$SBT)

titles <- c("readmitted_abnormalAD8", "readmitted_normalAD8", "readmitted_abnormalSBT", "readmitted_normalSBT")
values <- c(readmitted_abnormalAD8, readmitted_normalAD8, readmitted_abnormalSBT, readmitted_normalSBT)
out_of <- c(length(readmitted_within30_dataset$readmited_30_days), length(readmitted_within30_dataset$readmited_30_days), length(readmitted_within30_dataset$readmited_30_days), length(readmitted_within30_dataset$readmited_30_days))
df <- data.frame(titles, values, out_of)
df

labels <- c("abnormalAD8", "normalAD8", "abnormalSBT", "normalSBT")
values <- c(readmitted_abnormalAD8, readmitted_normalAD8, readmitted_abnormalSBT, readmitted_normalSBT)
png(file = "Readmissions")
barplot(values,names.arg= labels,xlab="cognition",ylab="values",col="blue",
        main="Readmissions with in 30 days chat",border="red")
dev.off()

#part-3
Surg_discharge<-temp_dataset[, Daydiff_SurgeryDischarge := as.numeric(difftime(Discharge, Surgery), units = "days"), by = "CPAPPatientID"] 
Surg_discharge<- Surg_discharge %>% filter(!is.na(Daydiff_SurgeryDischarge) )
#Surg_discharge <- Surg_discharge[Surg_discharge$Daydiff_SurgeryDischarge>0]


Abnormal_AD8<- filter(Surg_discharge, AD8>=2)
SurgeryDischarge_abnormalAD8<-length(Abnormal_AD8$AD8)

Normal_AD8<- filter(Surg_discharge, AD8<2)
SurgeryDischarge_normalAD8<-length(Normal_AD8$AD8)

Abnormal_SBT<-filter(Surg_discharge, SBT>=5)
SurgeryDischarge_abnormalSBT<-length(Abnormal_SBT$SBT)

Normal_SBT<-filter(Surg_discharge, SBT<5)
SurgeryDischarge_normalSBT<-length(Normal_SBT$SBT)

titles <- c("SurgeryDischarge_abnormalAD8", "SurgeryDischarge_normalAD8", "SurgeryDischarge_abnormalSBT", "SurgeryDischarge_normalSBT")
values <- c(SurgeryDischarge_abnormalAD8, SurgeryDischarge_normalAD8, SurgeryDischarge_abnormalSBT, SurgeryDischarge_normalSBT)
out_of <- c(length(Surg_discharge$Daydiff_SurgeryDischarge), length(Surg_discharge$Daydiff_SurgeryDischarge), length(Surg_discharge$Daydiff_SurgeryDischarge), length(Surg_discharge$Daydiff_SurgeryDischarge))
df <- data.frame(titles, values, out_of)
df

labels <- c("abnormalAD8", "normalAD8", "abnormalSBT", "normalSBT")
values <- c(SurgeryDischarge_abnormalAD8, SurgeryDischarge_normalAD8, SurgeryDischarge_abnormalSBT, SurgeryDischarge_normalSBT)
png(file = "Surgery & Discharge")
barplot(values,names.arg= labels,xlab="cognition",ylab="values",col="blue",
        main="Surgery Discharge chart",border="red")
dev.off()

#part-4

inhospital_mortaility<- left_join(signals_procedure, other, by = "CPAPPatientID")
inhospital_mortaility<-inhospital_mortaility %>% filter(inhospital_mortaility$dc_status == "hospice or death") 

Abnormal_AD8<- filter(inhospital_mortaility, AD8>=2)
mortality_abnormalAD8<-length(Abnormal_AD8$AD8)

Normal_AD8<- filter(inhospital_mortaility, AD8<2)
mortality_normalAD8<-length(Normal_AD8$AD8)

Abnormal_SBT<-filter(inhospital_mortaility, SBT>=5)
mortality_abnormalSBT<-length(Abnormal_SBT$SBT)

Normal_SBT<-filter(inhospital_mortaility, SBT<5)
mortality_normalSBT<-length(Normal_SBT$SBT)

titles <- c("mortality_abnormalAD8", "mortality_normalAD8", "mortality_abnormalSBT", "mortality_normalSBT")
values <- c(mortality_abnormalAD8, mortality_normalAD8, mortality_abnormalSBT, mortality_normalSBT)
out_of <- c(length(inhospital_mortaility$dc_status), length(inhospital_mortaility$dc_status), length(inhospital_mortaility$dc_status), length(inhospital_mortaility$dc_status))
df <- data.frame(titles, values, out_of)
df

labels <- c("abnormalAD8", "normalAD8", "abnormalSBT", "normalSBT")
values <- c(mortality_abnormalAD8, mortality_normalAD8, mortality_abnormalSBT, mortality_normalSBT)
png(file = "Mortality rates")
barplot(values,names.arg= labels,xlab="cognition",ylab="values",col="blue",
        main="Mortality chart",border="red")
dev.off()

#part-5

creatinine_difference<- signals_procedure %>% filter(!is.na(Creatinine) ) %>% filter(!is.na(`Last creatinine`) )
creatinine_difference$Creatine_diff <- (creatinine_difference$`Last creatinine` - creatinine_difference$Creatinine)

creatinine_difference<- creatinine_difference %>% mutate(AKI =case_when(Creatine_diff <= 0.3 ~ "0", Creatine_diff >= 0.4 ~ "1"))
Abnormal_AD8<- filter(creatinine_difference, AD8>=2)
AKI_abnormalAD8<-length(Abnormal_AD8$AD8)

Normal_AD8<- filter(creatinine_difference, AD8<2)
AKI_normalAD8<-length(Normal_AD8$AD8)

Abnormal_SBT<-filter(creatinine_difference, SBT>=5)
AKI_abnormalSBT<-length(Abnormal_SBT$SBT)

Normal_SBT<-filter(creatinine_difference, SBT<5)
AKI_normalSBT<-length(Normal_SBT$SBT)

titles <- c("AKI_abnormalAD8", "AKI_normalAD8", "AKI_abnormalSBT", "AKI_normalSBT")
values <- c(AKI_abnormalAD8, AKI_normalAD8, AKI_abnormalSBT, AKI_normalSBT)
out_of <- c(length(creatinine_difference$Creatine_diff), length(creatinine_difference$Creatine_diff), length(creatinine_difference$Creatine_diff), length(creatinine_difference$Creatine_diff))
df <- data.frame(titles, values, out_of)
df

labels <- c("abnormalAD8", "normalAD8", "abnormalSBT", "normalSBT")
values <- c(AKI_abnormalAD8, AKI_normalAD8, AKI_abnormalSBT, AKI_normalSBT)
png(file = "AKI")
barplot(values,names.arg= labels,xlab="cognition",ylab="values",col="blue",
        main="AKI chart",border="red")
dev.off()


TIA_dataset<- signals_procedure %>% filter(!is.na(TIA) ) 

Abnormal_AD8<- filter(TIA_dataset, AD8>=2)
TIA_abnormalAD8<-length(Abnormal_AD8$AD8)

Normal_AD8<- filter(TIA_dataset, AD8<2)
TIA_normalAD8<-length(Normal_AD8$AD8)

Abnormal_SBT<-filter(TIA_dataset, SBT>=5)
TIA_abnormalSBT<-length(Abnormal_SBT$SBT)

Normal_SBT<-filter(TIA_dataset, SBT<5)
TIA_normalSBT<-length(Normal_SBT$SBT)

titles <- c("TIA_abnormalAD8","TIA_normalAD8" , "TIA_abnormalSBT","TIA_normalSBT")
values <- c(TIA_abnormalAD8, TIA_normalAD8, TIA_abnormalSBT, TIA_normalSBT)
out_of <- c(length(TIA_dataset$TIA), length(TIA_dataset$TIA), length(TIA_dataset$TIA), length(TIA_dataset$TIA))
df <- data.frame(titles, values, out_of)
df

labels <- c("abnormalAD8", "normalAD8", "abnormalSBT", "normalSBT")
values <- c(TIA_abnormalAD8, TIA_normalAD8, TIA_abnormalSBT, TIA_normalSBT)
png(file = "TIA")
barplot(values,names.arg= labels,xlab="cognition",ylab="values",col="blue",
        main="TIA chart",border="red")
dev.off()


stroke_dataset<- signals_procedure %>% filter(!is.na(`Cerebrovascular disease, stroke, or TIA`) ) 

Abnormal_AD8<- filter(stroke_dataset, AD8>=2)
stroke_abnormalAD8<-length(Abnormal_AD8$AD8)

Normal_AD8<- filter(stroke_dataset, AD8<2)
stroke_normalAD8<-length(Normal_AD8$AD8)

Abnormal_SBT<-filter(stroke_dataset, SBT>=5)
stroke_abnormalSBT<-length(Abnormal_SBT$SBT)

Normal_SBT<-filter(stroke_dataset, SBT<5)
stroke_normalSBT<-length(Normal_SBT$SBT)

titles <- c(stroke_abnormalAD8, stroke_normalAD8, stroke_abnormalSBT, stroke_normalSBT)
values <- c(stroke_abnormalAD8, stroke_normalAD8, stroke_abnormalSBT, stroke_normalSBT)
out_of <- c(length(stroke_dataset$CPAPPatientID), length(stroke_dataset$CPAPPatientID), length(stroke_dataset$CPAPPatientID), length(stroke_dataset$CPAPPatientID))
df <- data.frame(titles, values, out_of)
df

labels <- c("abnormalAD8", "normalAD8", "abnormalSBT", "normalSBT")
values <- c(stroke_abnormalAD8, stroke_normalAD8, stroke_abnormalSBT, stroke_normalSBT)
png(file = "stroke")
barplot(values,names.arg= labels,xlab="cognition",ylab="values",col="blue",
        main="stroke chart",border="red")
dev.off()






