# LSF_DOCKER_VOLUMES='/storage1/fs1/christopherking/Active/lavanya/cognition_check:/research/' bsub -G 'compute-christopherking' -n 8 -R 'rusage[mem=64GB] span[hosts=1]' -M 64GB -q general-interactive -Is -a 'docker(cryanking/verse_words:1.2)' R


# library(tidyverse) 
library(lubridate) 
library(readxl)
library(magrittr)
library(dplyr) 
library(forcats)
library(readr)
library(data.table)
library(splines)
## again, you basically always want to treat ID numbers as strings. You don't want to do any "number" operations on them, and you don't want it to think that it can store them like floats. Int64 support isn't built into all packages.
CDS_ADT <- read_csv("CDS ADT.csv", col_types = cols(REFERENCE_NO = col_character(), 
                                                    REG_NO = col_character(), FACILITY_CONCEPT_ID = col_character()))

## there is built in support for datetime conversion if you are going through the trouble to do this
X2020_02_MV_LoS <- read_csv("2020_02_MV_LoS.csv", 
                            col_types = cols(PatientID = col_character(), 
                                             PAN = col_character(),
                                             EMPI = col_character(), 
                                             ADMIT_TMSTP = col_datetime(),
                                             DISCHARGE_TMSTP = col_datetime(),
                                             LoS_Days = col_double()
                                             ), na = "NULL")

## fixing classes, switching to read_xlsx since it is an xlsx file
cpap_vs_surgery <- read_xlsx("2020_01_King_ProcedureCodes_MostRecentCPAP.xlsx", sheet= "MostRecentCPAP",na = "NULL", col_types =c("text","date", "text", "date"))



Signals <- read_xlsx("2020_01_15_Gregory_Cognative_Dysfunction_Data.xlsx", 
                      sheet = "Signals", na = "NULL", col_types = c("text", 
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
                          sheet = "TextSignals",na = "NULL", col_types = c("text", 
                                                                           "numeric", "numeric", "numeric", "numeric", 
                                                                           "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                                           "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                                           "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                                           "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                                           "numeric", "numeric", "numeric", "numeric", "numeric"))

FreeText <- read_xlsx("2020_01_15_Gregory_Cognative_Dysfunction_Data.xlsx", 
                       sheet = "FreeText", na = "NULL", col_types = c("text", 
                                                                      "text", "text", "text"))

## you never use this
# Dictionary <- read_xlsx("2020_01_15_Gregory_Cognative_Dysfunction_Data.xlsx", 
#                          sheet = "Dictionary", na = "NULL", col_types = c("text", 
#                                                                           "numeric","numeric","text"))

ProcedureCodes <- read_xlsx("2020_01_15_Gregory_Cognative_Dysfunction_Data.xlsx", 
                             sheet = "ProcedureCodes",na = "NULL", col_types = c("text", 
                                                                                 "text", "text", "numeric", "text"))


other <- read_xlsx("2020_01_15_Gregory_Cognative_Dysfunction_Data.xlsx", 
                    sheet = "Other", na = "NULL", col_types = c("text", 
                                                                "text", "numeric"))

new_procedure_code <- read_xlsx("2020_01_15_Gregory_Cognative_Dysfunction_Data.xlsx", 
                                 sheet = "ProcedureCodes",na = "NULL", col_types = c("text", 
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

## there are only 1047 patients matching this set of procedures!
## I don't think we expected this to be so small
## the inner join here is a choice that might need to be returned to - having the other-procedure patients in the regression helps estimate the effect of covariates.


signals_procedure <- Signals %>% filter(Age_at_CPAP >= 65)   %>% 
  filter(!is.na(AD8) ) %>% filter(!is.na(SBT) ) %>% 
  inner_join(ProcedureCodes %>% filter(ICD_CODE_VERSION == "9-CM" | ICD_CODE_VERSION =="10-PCS") %>% filter( ICD_PROCEDURE_CODE %in% c("0FT44ZZ","0DTN0ZZ", "0DB64Z3", "0FBG0ZZ", "0FBG3ZZ","0UT90ZZ", "0UB74ZZ", "0SG00A0", "0SRC0J9","0SRB04Z","0RRJ00Z","0DQ53ZZ","0FT20ZZ","0TT10ZZ","0VT08ZZ","0TTB0ZZ","B50W","51.23","45.8","43.82","52.7","52.0","68.4","68.5","68.9","68.49","81.06","81.54","81.51","81.80","53.9","32.41","32.49","55.4","60.5","57.71","39.52","39.53")) %>% distinct(CPAPPatientID, .keep_all=T), by="CPAPPatientID")

## 0FT20ZZ (liver resection) is not in the analysis plan
## 
  
signals_procedure %<>% mutate(SurgeryType = case_when(
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
                              ICD_PROCEDURE_CODE == "39.52" | ICD_PROCEDURE_CODE == "39.53" ~ 'Arteriovenous'))
  
  
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

## it is often worthwhile to get rid of column names with spaces to avoid all this escaping with something like
## colnames(df) <- make.names(colnames(df)) 
## this is an alias for `colnames<-`(df, new_values) , which like the other <- functions modifies its arguments (there is only 1 place that I remember where R cares about white space: a <- -b (assign the value of b *-1 to a) is different from a<--b (assign the value of b to a in the global scope) )


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



## you can generate this formula as a string if you want to re-use it and save space, but you'll want to fix names with spaces in them first, like above
## formula(paste( "y ~" , paste( array_of_variable_names, collapse=" + ")  ) )
    
    


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


## again, please give a meaningful organizational comment on the purpose of this section of code
#part-2

## I am going to use the CDS_ADT data file to define readmissions instead of name+dob matching, but I left all these comments and code changes so that you can see why these were incorrect
if(FALSE) {
surgery <- left_join(cpap_vs_surgery, X2020_02_MV_LoS, by = c("Surg_PatientID" = "PatientID") )
filtered_mereged_data1<- left_join(signals_procedure, surgery, by = c("CPAPPatientID" = "CPAP_PatientID"))
## why discard the time part of the timestamps?
filtered_mereged_data1$Admission<-as_date(ymd_hms(filtered_mereged_data1$ADMIT_TMSTP,tz=NULL)) 
filtered_mereged_data1$Discharge<-as_date(ymd_hms(filtered_mereged_data1$DISCHARGE_TMSTP,tz=NULL))
filtered_mereged_data1$Surgery<-as_date(filtered_mereged_data1$Surgery_Date,tz=NULL)

filtered_mereged_data1 %<>% mutate(new_id = openssl::md5(paste(NAME, DoB)))
final_dataset <- data.table(filtered_mereged_data1)
setkey(final_dataset, new_id, Admission)
## keys aren't free - set keys when you plan to need them to fetch data efficently. If you want to order a dataset you can setorder which is cheaper
## 
## this is usually not what you want to do; the := operator in data.table is for in-place modification.
## data.table, unlike almost everything else in R does not respect the copy-on-write principal
## so for example, when you create final_dataset here, if you make change in it they will *also* change temp_dataset (because they are pointer to the same objects) and vice versa. you can explicity dt2 <- copy(dt1) if you really need to
## although dplyr verbs (like mutate) work on data.table, you generally want to pick one of the two to avoid this kind of problem
## using dplyr verbs on data.table objects often sacrifices the performance gains of data.table

final_dataset[ , Daydiff_AdmissionDischarge := as.numeric(difftime(Discharge, shift(Admission, n = 1L, fill = 0, type = "lag"), units="days")), by = "new_id"]
#final_dataset$Daydiff_AdmissionDischarge <- ifelse(final_dataset$Daydiff_AdmissionDischarge>=10000, NA, final_dataset$Daydiff_AdmissionDischarge)
## why using abs? you used Admission as a key; they are already sorted. 
## ifelse is inferior to if_else (dplyr) and fifelse (data.table) - it doesn't check type consistency and is slower
## you don't have to if_else(test, 1, 0) ; test (logical) can be used anywhere a numeric can (it is similar to python in this regard)
#readmited_list = ifelse(abs(final_dataset$Daydiff_AdmissionDischarge) <= 30, 1, 0)
#final_dataset2 <- final_dataset %>% mutate(readmited_30_days = as.numeric(readmited_list)) 

## in dplyr (the efficency of this operation depends on what other packages you are using that affect this backend):
## final_dataset %<>% mutate( readmited_30_days = Daydiff_AdmissionDischarge <= 30  )
## in data.table
final_dataset [ , readmited_30_days := Daydiff_AdmissionDischarge <= 30]

## you don't have to dt[dt$var < val ] like Pandas, dt[ var < val] 
## and you turned a logical into a numeric just to check > 0 !
##  as.numeric(test) > 0 usually works in R, but as you know this is generally an unsafe way to check conditions
##  unlike base R dataframes, they data.table doesn't like dt[var] to get rows where var is true, prefers dt[var == TRUE] (T is also an alias of TRUE)
##  also, this is the wrong analysis. you want to compare those readmitted vs not rather than generating counts
# readmitted_within30_dataset <- final_dataset2[ readmited_30_days == TRUE]

## same as above, you can use the summarize methods in dplyr or the by methods in data.table instead of making a bunch of global variables
## illustrating data.table's fcase since I used dplyr's case_when above
readmitted_within30_dataset[ , abnormal_cog := fcase (
  is.na(AD8) & is.na(SBT), NA ## the default type of NA is logical, but there are NA_real_ NA_character_ etc
  is.na(ADB), SBT>=5L, ## 5L is the integer, although comparing an integer to a float usually works it sometimes gets equality wrong
  is.na(SBT), AD8 >= 2L,
  default = AD8 >= 2L | SBT>=5L  ## unlike case_when, instead of making the default condition what happens with a fixed TRUE condition, explicity name the default
  )  ] 

## in data.table .() is an alias of list()
readmitted_within30_dataset[ , .(readmit_rate = mean(readmited_30_days , na.rm=TRUE), num=.N ), by="abnormal_cog"] %>% print  


## nrow(dt) is what you're looking for instead of length(dt$var), or maybe sum(is.finite(dt$var))
#out_of <- c(length(readmitted_within30_dataset$readmited_30_days), length(readmitted_within30_dataset$readmited_30_days), length(readmitted_within30_dataset$readmited_30_days), length(readmitted_within30_dataset$readmited_30_days))
} else {

############
## part 2: creating the readmission outcome and predicting it with cognition status
############

## generating the readmission outcome
## join the readmission data to the LOS data file as an intermediate
## note that the CDS_ADT file has the REFERENCE_NO padded to 24 characters wide for no good reason
  ## the package::object notation gets something from an unloaded package that is marked as ok to export, the package:::object notation gets private objects as well
X2020_02_MV_LoS$EMPI %<>% stringr::str_pad(string=., width=24, side="left", pad="0")
  ## the %<>% (pipe and assign) operator when given a function on the RHS does not need parens
CDS_ADT$ROOM_START_DATE %<>% ymd_hms 
  ## you can chain using %<>% and it uses the final value
## we are only interested in admissions after discharge, I also remove any that are close to the discharge timestamps to eliminate "discharges" that are really transfers (this happens sometimes from the psych-inpatient to regular medicine services)
  ## the ddays is a handy function in lubridate for doing shifts on datetimes
  ## the dplyr::between and data.table::between functions are in their simplest uses the same, but they have different function signatures; whichever package was loaded last is used unless you specify!
  ## I do something here unusual for me and create a global var to simplify the expression
readmitted_pid <- X2020_02_MV_LoS %>%  left_join(CDS_ADT, by=c("EMPI"="REFERENCE_NO" ) ) %>% filter( data.table::between(ROOM_START_DATE , DISCHARGE_TMSTP+ddays(.25), DISCHARGE_TMSTP+ddays(30) )  ) %>% pull("PatientID") %>% unique
  


## join CPAP data to the LOS data file 
  ## inner join gets rid of any CPAP visits with no matched surgery event
  ## there are 9782 such non-matches, so I will go back to the "raw-raw" data and get the bigger set of maps
joined_ADT <- signals_procedure %>%  inner_join( cpap_vs_surgery %>% select(CPAP_PatientID, Surg_PatientID), by = c("CPAPPatientID"="CPAP_PatientID") ) %>%
  mutate(readmitted_in_30= Surg_PatientID %in%  readmitted_pid )

  ## I won't repeat everything from above  
joined_ADT %>% mutate(
  AbnCog =  case_when(
    is.na(AD8) & is.na(SBT) ~ NA ,
    is.na(AD8) ~ SBT>=5 ,
    is.na(SBT) ~ AD8>=2 ,
    TRUE ~ AD8>=2 | SBT>=5 ) ) %>% 
    glm(data=., 
      readmitted_in_30 ~ AbnCog
      , family=binomial() ) %>% summary %>% extract2("coefficients") %>% 
    extract("AbnCogTRUE",-3, drop=F) 

#             Estimate Std. Error   Pr(>|z|)
# AbnCogTRUE 0.1007585 0.04891816 0.03942384

joined_ADT %>% mutate(
  AbnCog =  case_when(
    is.na(AD8) & is.na(SBT) ~ NA ,
    is.na(AD8) ~ SBT>=5 ,
    is.na(SBT) ~ AD8>=2 ,
    TRUE ~ AD8>=2 | SBT>=5 ) ) %>% 
    glm(data=., 
      readmitted_in_30 ~ AbnCog + Hypertension + `Coronary artery disease` + `Atrial fibrillation or flutter history` +  `Cerebrovascular disease` + `Diabetes mellitus`
      , family=binomial() ) %>% summary %>% extract2("coefficients") %>% 
    extract("AbnCogTRUE",-3, drop=F) 

#             Estimate Std. Error  Pr(>|z|)
# AbnCogTRUE 0.1058638  0.3869793 0.7844194

## examining surgery-specific effective
## I switched from base-R to tidy type manipulation because it got a little more complex
## this kind of output with v large standard errors is ususally because of perfect separation

joined_ADT %>% mutate(
  AbnCog =  case_when(
    is.na(AD8) & is.na(SBT) ~ NA ,
    is.na(AD8) ~ SBT>=5 ,
    is.na(SBT) ~ AD8>=2 ,
    TRUE ~ AD8>=2 | SBT>=5 ) ) %>% 
    glm(data=., 
      readmitted_in_30 ~ AbnCog*SurgeryType
      , family=binomial(), contrasts=list(SurgeryType="contr.sum") ) %>% summary %>% extract2("coefficients") %>% as_tibble(rownames="rname") %>% filter(grepl(rname, pattern="AbnCog")) %>% select(-`z value`)

## there are only 1007 patients whose surgery type is classified, so this approach is not going to work just yet
joined_ADT$SurgeryType %>% table
#        Arteriovenous      cholecystectomy            Colectomy 
#                    4                   73                   29 
#           Cystectomy          Gastrectomy         Hysterectomy 
#                   61                    5                  111 
#            lobectomy         lumbarFusion          Nephrectomy 
#                   42                   43                   52 
#        Prostatectomy             totalHip totalKneeArthropathy 
#                   77                   13                  213 
#        totalShoulder              whipple 
#                  135                  149 
      
joined_ADT %>% filter(SurgeryType == "cholecystectomy") %>% mutate(
  AbnCog =  case_when(
    is.na(AD8) & is.na(SBT) ~ NA ,
    is.na(AD8) ~ SBT>=5 ,
    is.na(SBT) ~ AD8>=2 ,
    TRUE ~ AD8>=2 | SBT>=5 ) ) %>% 
    glm(data=., 
      readmitted_in_30 ~ AbnCog
      , family=binomial() ) %>% summary %>% extract2("coefficients") %>% as_tibble(rownames="rname") %>% filter(grepl(rname, pattern="AbnCog")) %>% select(-`z value`)





}


## CRK I stopped here

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






