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


X2020_01_15_Gregory_Cognative_Dysfunction_Data <- read_excel("E:/2020_01_15_Gregory_Cognative_Dysfunction_Data.xlsx", 
                                                             sheet = "home_Discharge")
View(X2020_01_15_Gregory_Cognative_Dysfunction_Data)


library(dplyr)
inclusion = filter(Signals, Age_at_CPAP >= 65)
inclusion1 = filter(inclusion, CPAP_Date >= "2013-01-01" & CPAP_Date <= "2018-06-30" )
inclusion2<- inclusion1 %>% filter(!is.na(AD8) ) %>% filter(!is.na(SBT) )


Procedure1 = filter(ProcedureCodes, ICD_CODE_VERSION == "9-CM" | ICD_CODE_VERSION =="10-PCS")
SA = filter(Procedure1, ICD_PROCEDURE_CODE == "0FT44ZZ"|ICD_PROCEDURE_CODE == "0DTN0ZZ"|ICD_PROCEDURE_CODE =="0DB64Z3"|ICD_PROCEDURE_CODE =="0FBG0ZZ"|ICD_PROCEDURE_CODE =="0FBG3ZZ"|ICD_PROCEDURE_CODE =="0UT90ZZ"|ICD_PROCEDURE_CODE =="0UB74ZZ"|ICD_PROCEDURE_CODE =="0SG00A0"|ICD_PROCEDURE_CODE =="0SRC0J9"|ICD_PROCEDURE_CODE =="0SRB04Z"|ICD_PROCEDURE_CODE =="0RRJ00Z"|ICD_PROCEDURE_CODE =="0DQ53ZZ"|ICD_PROCEDURE_CODE =="0FT20ZZ"|ICD_PROCEDURE_CODE =="0TT10ZZ"|ICD_PROCEDURE_CODE =="0VT08ZZ"|ICD_PROCEDURE_CODE =="0TTB0ZZ"|ICD_PROCEDURE_CODE =="B50W")
SW = filter(Procedure1, ICD_PROCEDURE_CODE == "51.23"|ICD_PROCEDURE_CODE == "45.8"|ICD_PROCEDURE_CODE =="43.82"|ICD_PROCEDURE_CODE == "52.7"|ICD_PROCEDURE_CODE == "52.0"|ICD_PROCEDURE_CODE =="68.4"|ICD_PROCEDURE_CODE =="68.5"|ICD_PROCEDURE_CODE =="68.9"|ICD_PROCEDURE_CODE =="68.49"|ICD_PROCEDURE_CODE =="81.06"|ICD_PROCEDURE_CODE =="81.54"|ICD_PROCEDURE_CODE =="81.51"|ICD_PROCEDURE_CODE =="81.80"|ICD_PROCEDURE_CODE =="53.9"|ICD_PROCEDURE_CODE =="32.41"|ICD_PROCEDURE_CODE =="32.49"|ICD_PROCEDURE_CODE =="55.4"|ICD_PROCEDURE_CODE =="60.5"|ICD_PROCEDURE_CODE =="57.71"|ICD_PROCEDURE_CODE =="39.52"|ICD_PROCEDURE_CODE =="39.53")
Procedure2 = filter(Procedure1, ICD_PROCEDURE_CODE == "51.23"|ICD_PROCEDURE_CODE == "45.8"|ICD_PROCEDURE_CODE =="43.82"|ICD_PROCEDURE_CODE == "52.7"|ICD_PROCEDURE_CODE == "52.0"|ICD_PROCEDURE_CODE =="68.4"|ICD_PROCEDURE_CODE =="68.5"|ICD_PROCEDURE_CODE =="68.9"|ICD_PROCEDURE_CODE =="68.49"|ICD_PROCEDURE_CODE =="81.06"|ICD_PROCEDURE_CODE =="81.54"|ICD_PROCEDURE_CODE =="81.51"|ICD_PROCEDURE_CODE =="81.80"|ICD_PROCEDURE_CODE =="53.9"|ICD_PROCEDURE_CODE =="32.41"|ICD_PROCEDURE_CODE =="32.49"|ICD_PROCEDURE_CODE =="55.4"|ICD_PROCEDURE_CODE =="60.5"|ICD_PROCEDURE_CODE =="57.71"|ICD_PROCEDURE_CODE =="39.52"|ICD_PROCEDURE_CODE =="39.53"|ICD_PROCEDURE_CODE == "0FT44ZZ"|ICD_PROCEDURE_CODE == "0DTN0ZZ"|ICD_PROCEDURE_CODE =="0DB64Z3"|ICD_PROCEDURE_CODE =="0FBG0ZZ"|ICD_PROCEDURE_CODE =="0FBG3ZZ"|ICD_PROCEDURE_CODE =="0UT90ZZ"|ICD_PROCEDURE_CODE =="0UB74ZZ"|ICD_PROCEDURE_CODE =="0SG00A0"|ICD_PROCEDURE_CODE =="0SRC0J9"|ICD_PROCEDURE_CODE =="0SRB04Z"|ICD_PROCEDURE_CODE =="0RRJ00Z"|ICD_PROCEDURE_CODE =="0DQ53ZZ"|ICD_PROCEDURE_CODE =="0FT20ZZ"|ICD_PROCEDURE_CODE =="0TT10ZZ"|ICD_PROCEDURE_CODE =="0VT08ZZ"|ICD_PROCEDURE_CODE =="0TTB0ZZ"|ICD_PROCEDURE_CODE =="B50W")

Procedure4 = Procedure2[!duplicated(Procedure2$CPAPPatientID),]

sipro = inclusion2 %>% left_join(Procedure4, by="CPAPPatientID")

Text1<- TextSignals%>%filter(`Home status` %in% c(2,3))


#Analysis-2
#part-1
filtered_text1 <- filter(other,!UB_DISCHARGE_DISPOSITION_DESCRIPTION %in% c("Discharge to home or self care (routine discharge)","Discharged/transferred to home - organized home health service","Discharged/transferred to Skilled Nursing Facility","Discharged/transferred to Hospice - home","DSCH/TRNF TO HOME-HOME HEALTH SKILLED CARE","DISCHARGE TO HOSPICE/HOME","DSCH/TRNF TO HOME W/ HOME HEALTH CARE WITH PLAN READMIT","Discharge to home or self care","DISCHARGE TO HOME OR SELF CARE"))

discharged_patient_other_home = filtered_text1 %>% right_join(Text1, by="CPAPPatientID")


#part-2

text_signal_mapped_data = Text1 %>% inner_join(Signals, by="CPAPPatientID")

ordered_data<-text_signal_mapped_data[order(text_signal_mapped_data$CPAPPatientID),]


# Temp Line for Data split 
# ordered_data <- ordered_data1[0:100,]


row.names(ordered_data) <- NULL

#patients_unique_list = unique(ordered_data$CPAPPatientID)

#len <- length(patients_unique_list)

empty_list <- vector(mode = "list", length = 0)

readmitted_patient_list<- vector(mode = "list", length = 0)

count<- assign("count", 0)
for (x in ordered_data$CPAPPatientID) {
  if (x %in% empty_list){
    print(x)
    frst_admission_index <- match(x,empty_list)
    readmission_date <- ordered_data$CPAP_Date[count] 
    frst_Adm_date <- ordered_data$CPAP_Date[frst_admission_index]
    surgery_date <- ordered_data$Surgery_Date[frst_admission_index]
    maxtime = difftime(readmission_date, surgery_date , units="days")
    print(maxtime)
    
    if (count >= 2){
      
      if(maxtime <= 30 ){ 
        print("===========================Patient Readmitted within 30 days=========================")
        readmitted_patient_list <- c(readmitted_patient_list, x)
      }
    }
    
  }
  else{
    print('in else')
    empty_list <- c(empty_list, x)
  }
  count <- count + 1
}

readmitted_patient_data <- text_signal_mapped_data[text_signal_mapped_data$CPAPPatientID %in% readmitted_patient_list ,]

#part-3


inhospital_mortaility<- filter(discharged_patient_other_home, UB_DISCHARGE_DISPOSITION_DESCRIPTION == "EXPIRED"|UB_DISCHARGE_DISPOSITION_DESCRIPTION == "Expired (or did not recover-Religious Non Medical Health Care Patient")
inhospital_mortaility<-inhospital_mortaility%>% rename(mortality_rate = UB_DISCHARGE_DISPOSITION_DESCRIPTION)
#part-4

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

sipro6<- data.frame(conditions = c("Discharge_other_than_home", "Mortality_RATE", "AKI", "STroke", "tia"), Normal_AD8 =c(23081,50,235,3248,1134), Abnormal_AD8=c(2389,7,44,629,215), Normal_SBT=c(20823,41,64,2831,1019), Abnormal_SBT = c(4647,16,215,1046,330))
print(sipro6)
