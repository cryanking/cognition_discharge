library(readxl)

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

library(dplyr)
library(magrittr) 
inclusion = filter(Signals, Age_at_CPAP >= 65)
inclusion1 = filter(inclusion, CPAP_Date >= "2013-01-01" & CPAP_Date <= "2018-06-30" )
# inclusion2<- inclusion1 %>% filter(!is.na(AD8) ) %>% filter(!is.na(SBT) )


## some of these are the same person
inclusion1 %<>% mutate(new_id = openssl::md5(paste(NAME, DoB)))
inclusion1 %>% nrow
inclusion1$new_id %>% n_distinct
inclusion1$CPAPPatientID %>% n_distinct


Procedure1 = filter(ProcedureCodes, ICD_CODE_VERSION == "9-CM" | ICD_CODE_VERSION =="10-PCS")
ICD_10CODE = filter(Procedure1, ICD_PROCEDURE_CODE == "0FT44ZZ"|ICD_PROCEDURE_CODE == "0DTN0ZZ"|ICD_PROCEDURE_CODE =="0DB64Z3"|ICD_PROCEDURE_CODE =="0FBG0ZZ"|ICD_PROCEDURE_CODE =="0FBG3ZZ"|ICD_PROCEDURE_CODE =="0UT90ZZ"|ICD_PROCEDURE_CODE =="0UB74ZZ"|ICD_PROCEDURE_CODE =="0SG00A0"|ICD_PROCEDURE_CODE =="0SRC0J9"|ICD_PROCEDURE_CODE =="0SRB04Z"|ICD_PROCEDURE_CODE =="0RRJ00Z"|ICD_PROCEDURE_CODE =="0DQ53ZZ"|ICD_PROCEDURE_CODE =="0FT20ZZ"|ICD_PROCEDURE_CODE =="0TT10ZZ"|ICD_PROCEDURE_CODE =="0VT08ZZ"|ICD_PROCEDURE_CODE =="0TTB0ZZ"|ICD_PROCEDURE_CODE =="B50W")
ICD_9CODE = filter(Procedure1, ICD_PROCEDURE_CODE == "51.23"|ICD_PROCEDURE_CODE == "45.8"|ICD_PROCEDURE_CODE =="43.82"|ICD_PROCEDURE_CODE == "52.7"|ICD_PROCEDURE_CODE == "52.0"|ICD_PROCEDURE_CODE =="68.4"|ICD_PROCEDURE_CODE =="68.5"|ICD_PROCEDURE_CODE =="68.9"|ICD_PROCEDURE_CODE =="68.49"|ICD_PROCEDURE_CODE =="81.06"|ICD_PROCEDURE_CODE =="81.54"|ICD_PROCEDURE_CODE =="81.51"|ICD_PROCEDURE_CODE =="81.80"|ICD_PROCEDURE_CODE =="53.9"|ICD_PROCEDURE_CODE =="32.41"|ICD_PROCEDURE_CODE =="32.49"|ICD_PROCEDURE_CODE =="55.4"|ICD_PROCEDURE_CODE =="60.5"|ICD_PROCEDURE_CODE =="57.71"|ICD_PROCEDURE_CODE =="39.52"|ICD_PROCEDURE_CODE =="39.53")
Procedure2 = filter(Procedure1, ICD_PROCEDURE_CODE == "51.23"|ICD_PROCEDURE_CODE == "45.8"|ICD_PROCEDURE_CODE =="43.82"|ICD_PROCEDURE_CODE == "52.7"|ICD_PROCEDURE_CODE == "52.0"|ICD_PROCEDURE_CODE =="68.4"|ICD_PROCEDURE_CODE =="68.5"|ICD_PROCEDURE_CODE =="68.9"|ICD_PROCEDURE_CODE =="68.49"|ICD_PROCEDURE_CODE =="81.06"|ICD_PROCEDURE_CODE =="81.54"|ICD_PROCEDURE_CODE =="81.51"|ICD_PROCEDURE_CODE =="81.80"|ICD_PROCEDURE_CODE =="53.9"|ICD_PROCEDURE_CODE =="32.41"|ICD_PROCEDURE_CODE =="32.49"|ICD_PROCEDURE_CODE =="55.4"|ICD_PROCEDURE_CODE =="60.5"|ICD_PROCEDURE_CODE =="57.71"|ICD_PROCEDURE_CODE =="39.52"|ICD_PROCEDURE_CODE =="39.53"|ICD_PROCEDURE_CODE == "0FT44ZZ"|ICD_PROCEDURE_CODE == "0DTN0ZZ"|ICD_PROCEDURE_CODE =="0DB64Z3"|ICD_PROCEDURE_CODE =="0FBG0ZZ"|ICD_PROCEDURE_CODE =="0FBG3ZZ"|ICD_PROCEDURE_CODE =="0UT90ZZ"|ICD_PROCEDURE_CODE =="0UB74ZZ"|ICD_PROCEDURE_CODE =="0SG00A0"|ICD_PROCEDURE_CODE =="0SRC0J9"|ICD_PROCEDURE_CODE =="0SRB04Z"|ICD_PROCEDURE_CODE =="0RRJ00Z"|ICD_PROCEDURE_CODE =="0DQ53ZZ"|ICD_PROCEDURE_CODE =="0FT20ZZ"|ICD_PROCEDURE_CODE =="0TT10ZZ"|ICD_PROCEDURE_CODE =="0VT08ZZ"|ICD_PROCEDURE_CODE =="0TTB0ZZ"|ICD_PROCEDURE_CODE =="B50W")

# Procedure4 = Procedure2[!duplicated(Procedure2$CPAPPatientID),]
## dplyr::distinct is a nice function because it makes it fast/easy to use multiple indicies, computed indicies, respects groups. Note that it uses non-standard evaluation (CPAPPatientID instead of "CPAPPatientID")
## You also don't need to make all these intermediaries. It has a tendency to be inefficent with memeory, for example
## do you want to include a patient multiple times if they have multiple matching procedure codes? because that is what a left join will do. It so happens to never come up
signals_procedure <- inclusion1 %>% filter(!is.na(AD8) ) %>% filter(!is.na(SBT) ) %>% left_join(Procedure2 %>% distinct(CPAPPatientID, .keep_all=T), by="CPAPPatientID")

## are there any repeats?
signals_procedure %>% pull("CPAPPatientID") %>% table %>% table

## see above to skip naming the unused intermediates
# signals_procedure = inclusion2 %>% left_join(Procedure4, by="CPAPPatientID")

## this is never used
# signals_procedure_text = signals_procedure %>% left_join(TextSignals, by= "CPAPPatientID")

## the %<>% operator is exported from from the magrittr package, which you had not imported (I added it above). If you had run this code start to finish it would have errored. Always check that a script runs completely without errors.
TextSignals %<>% select(one_of("CPAPPatientID", "Sex", "Race", "Ethnicity", "Alcohol use", "Drinks/week", "Functional capacity", "Dialysis history", "Cirrhosis etiology", "Surgical Service") )

## clean_names is exported from the janitor package, which you have not imported. If you had run this code start to finish it would have errored. Always check that a script runs completely without errors. the package::object notion is nice if you only need 1 or 2 functions and don't want to clutter your namespace. It does require you to protect the :: operator with a parens
# TextSignals %<>% clean_names
TextSignals %<>% (janitor::clean_names)
library(forcats) ## again, not imported
TextSignals$race %<>% as.factor %>% fct_other(keep=c("7","9")) %>% fct_recode(White="9", Black="7") %>% fct_explicit_na("Missing") %>% relevel(ref="White")

## the base package ifelse doesn't check type consistency, which is a frequent source of difficult to detect bugs. use if_else (in tidyverse or fifelse in data.table)
TextSignals %<>% mutate(current_heavy = if_else(sex==1, drinks_week > 16, drinks_week >10)%>% as.factor %>% fct_explicit_na ) %>% select(-one_of("drinks_week"))
TextSignals$alcohol_use %<>% is_in(4:6) %>% as.factor %>% fct_explicit_na
TextSignals$dialysis_history %<>% is_in(76:80)%>% as.factor %>% fct_explicit_na
TextSignals$functional_capacity %<>% is_in(9:11)%>% as.factor %>% fct_explicit_na
TextSignals$cirrhosis_etiology %<>% is_in(107:113)%>% as.factor %>% fct_explicit_na


TextSignals %<>% rename(prior_heavy_alcohol=alcohol_use, low_functional_capacity=functional_capacity, cirrhosis=cirrhosis_etiology)#, ESRD =dialysis_history)

signals_procedure %<>% left_join(TextSignals  , by=c("CPAPPatientID"="cpap_patient_id")) %>% mutate(dialysis=na_zero(Dialysis))

## were you going to reconcile the two sources of dialysis information?
signals_procedure %>% select( one_of("dialysis", "dialysis_history" ) ) %>% table

#         dialysis_history
# dialysis FALSE  TRUE
#        0 20149  4845
#        1   259   217

signals_procedure %<>% mutate(male_sex = sex==1) 
signals_procedure %>% group_by(male_sex) %>% summarise(percent = 100 * n() / nrow(signals_procedure))

signals_procedure %<>% mutate(low_barthel = Barthel %>% is_less_than(100) %>% as.factor %>% fct_explicit_na ) 


## for a descriptive section there is no justification for imputing BMI, just NA if you think these are data entry errors. For the regression procedure I'd truncate them at 15/60 instead of mean-imputing
# signals_procedure %<>% mutate(BMI = ifelse(between(BMI, 15, 60 ), BMI, mean(BMI, na.rm=TRUE))) 
signals_procedure %<>% mutate(BMI = if_else(between(BMI, 15, 60 ), BMI, NA_real_) ) 

## these aren't necessarilly all errors either 
# > signals_procedure %>% filter(BMI < 15) %>% pull("BMI") %>% sort %>% round
#  [1]  0  0  1  3  5  7 10 11 12 12 13 13 14 14 14 15 15 15



# signals_procedure %<>% mutate(Age_at_CPAP = ifelse(is_greater_than(Age_at_CPAP, 64.9 ), Age_at_CPAP, mean(Age_at_CPAP, na.rm=TRUE))) 
## this never comes up in this dataset. It is certainly plausible that some 64 year olds had AD8 / SBT done; I don't know why you'd mark them as 73.
# signals_procedure %<>% mutate(Age_at_CPAP = if_else(is_greater_than(Age_at_CPAP, 64.9 ), Age_at_CPAP, mean(Age_at_CPAP, na.rm=TRUE))) 

signals_procedure %>% select(AD8, SBT) %>% mutate_all(is.na) %>% table

signals_procedure %<>% mutate(bmi_cats = cut(BMI, breaks=c(14, 18.5, 25, 30, 35, 40, 61), right=FALSE ))


## analysis 1

filter_the_nullvalues <- . %>% filter(!is.na(AD8) ) %>% filter(!is.na(SBT) )
make_abnl <- . %>% mutate(abn_cog = (AD8>=2) | (SBT >=5))


conditions <- . %>% mutate(CVA = `Cerebrovascular disease` | `Cerebrovascular disease, stroke, or TIA` | CVA | TIA) %>% mutate_at(vars(one_of("Coronary artery disease", "Congestive heart failure", "Atrial fibrillation or flutter history" , "COPD" , "Asthma" , "Peripheral artery disease" , "Diabetes mellitus" , "Current cancer", "CVA" ,"Hypertension")) , na_zero) %>% 
  mutate_at(vars(one_of("Coronary artery disease", "Congestive heart failure", "Atrial fibrillation or flutter history" , "COPD" , "Asthma" , "Peripheral artery disease" , "Diabetes mellitus" , "Current cancer", "CVA","prior_heavy_alcohol","low_functional_capacity" , "cirrhosis"
                        , "dialysis","Hypertension")) , as.logical)


Analysis1table <- signals_procedure %>% filter_the_nullvalues %>% make_abnl %>% conditions %>% mutate(abn_cog = abn_cog %>% as.factor %>% fct_recode(abnl="TRUE",nl="FALSE")) %>%
  table1::table1(~Age_at_CPAP + race + male_sex + bmi_cats  + `Coronary artery disease` + low_barthel + `Congestive heart failure` + `Atrial fibrillation or flutter history` + CVA + COPD + Asthma + `Peripheral artery disease` + `Diabetes mellitus` + `Current cancer`+current_heavy+prior_heavy_alcohol+low_functional_capacity+cirrhosis+dialysis|abn_cog, data=.)

cat(file="Analysis1.html",Analysis1table)

