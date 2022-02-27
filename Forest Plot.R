library(readxl)
library(magrittr)
library(dplyr)
library(forcats)
library(readr)
library(lubridate)
library(table1)
library(data.table)

#install.packages('meta', dependencies = TRUE, repos='http://cran.rstudio.com/')
library(meta)

find.package("janitor" )
 

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



CDS_ADT <- read_csv("CDS ADT.csv", col_types = cols(REFERENCE_NO = col_character(), 
                                                    REG_NO = col_character(), FACILITY_CONCEPT_ID = col_character()))


id_links <- read_excel("2021_01_ActFast_Identifiers.xlsx", na= c("", "NULL"), col_types=c("text","text","text","text","date"), sheet="MetaVision" )


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

new_procedure_code <- read_excel("2020_01_15_Gregory_Cognative_Dysfunction_Data.xlsx", 
                                 sheet = "ProcedureCodes",na = "NULL", col_types = c("numeric", 
                                                                                     "text", "text", "numeric", "text"))

first_visit <- FreeText %>% select(CPAPPatientID,MRN) %>% inner_join(Signals %>% select(CPAPPatientID, Surgery_Date), by = "CPAPPatientID") %>% group_by(MRN) %>% slice_min( order_by="Surgery_Date", n=1 , with_ties=FALSE) %>% ungroup

Signals %<>% semi_join(first_visit, by= "CPAPPatientID", na_matches="never")
TextSignals %<>% semi_join(first_visit, by= "CPAPPatientID", na_matches="never")
FreeText %<>% semi_join(first_visit, by= "CPAPPatientID", na_matches="never")
ProcedureCodes %<>% semi_join(first_visit, by= "CPAPPatientID", na_matches="never")
other %<>% semi_join(first_visit, by= "CPAPPatientID", na_matches="never")


signals_procedure <- Signals %>% filter(Age_at_CPAP >= 65)   %>% 
  filter(!is.na(AD8) ) %>% filter(!is.na(SBT) ) %>% 
  left_join(ProcedureCodes %>% filter(ICD_CODE_VERSION == "9-CM" | ICD_CODE_VERSION =="10-PCS") %>% filter( ICD_PROCEDURE_CODE %in% c("0FT44ZZ","0DTN0ZZ", "0DB64Z3", "0FBG0ZZ", "0FBG3ZZ","0UT90ZZ", "0UB74ZZ", "0SG00A0", "0SRC0J9","0SRB04Z","0RRJ00Z","0DQ53ZZ","0FT20ZZ","0TT10ZZ","0VT08ZZ","0TTB0ZZ","B50W","51.23","45.8","43.82","52.7","52.0","68.4","68.5","68.9","68.49","81.06","81.54","81.51","81.80","53.9","32.41","32.49","55.4","60.5","57.71","39.52","39.53")) %>% distinct(CPAPPatientID, .keep_all=T), by="CPAPPatientID")



signals_procedureSurgeryType<- signals_procedure%>%mutate(SurgeryType = case_when(ICD_PROCEDURE_CODE == "51.23" | ICD_PROCEDURE_CODE == "0FT44ZZ" ~ 'cholecystectomy', 
                                                                                  ICD_PROCEDURE_CODE == "45.8" | ICD_PROCEDURE_CODE == "0DTN0ZZ" ~ 'Colectomy', 
                                                                                  ICD_PROCEDURE_CODE == "43.82" | ICD_PROCEDURE_CODE == "0DB64Z3" ~ 'Gastrectomy',
                                                                                  ICD_PROCEDURE_CODE == "52.7" | ICD_PROCEDURE_CODE == "0FBG0ZZ" ~ 'whipple',
                                                                                  ICD_PROCEDURE_CODE == "68.4" | ICD_PROCEDURE_CODE == "0UT90ZZ" |ICD_PROCEDURE_CODE == "68.5"|ICD_PROCEDURE_CODE =="68.9" ~ 'Hysterectomy',
                                                                                  ICD_PROCEDURE_CODE == "68.49" | ICD_PROCEDURE_CODE == "0UB74ZZ" ~ 'TAH',
                                                                                  ICD_PROCEDURE_CODE == "52.0" | ICD_PROCEDURE_CODE ==  "0FBG3ZZ" ~ 'pancreatectomy',
                                                                                  ICD_PROCEDURE_CODE == "81.06" | ICD_PROCEDURE_CODE == "0SG00A0" ~ 'lumbarInfusion',
                                                                                  ICD_PROCEDURE_CODE == "81.54" | ICD_PROCEDURE_CODE == "0SRC0J9" ~ 'totalKneeArthropathy',
                                                                                  ICD_PROCEDURE_CODE == "81.51" | ICD_PROCEDURE_CODE == "0SRB04Z" ~ 'totalHipArthritis',
                                                                                  ICD_PROCEDURE_CODE == "81.80" | ICD_PROCEDURE_CODE == "0RRJ00Z" ~ 'totalShoulderArthritis',
                                                                                  ICD_PROCEDURE_CODE == "53.9" | ICD_PROCEDURE_CODE ==  "0DQ53ZZ" ~ 'laproscopicHiatalHernia',
                                                                                  ICD_PROCEDURE_CODE == "32.49" | ICD_PROCEDURE_CODE == "32.41" ~ 'lobectomyOpenVats',
                                                                                  ICD_PROCEDURE_CODE == "55.4" | ICD_PROCEDURE_CODE == "0TT10ZZ" ~ 'Nephrectomy',
                                                                                  ICD_PROCEDURE_CODE == "60.5" | ICD_PROCEDURE_CODE == "0VT08ZZ" ~ 'Prostatectomy',
                                                                                  ICD_PROCEDURE_CODE == "57.71" | ICD_PROCEDURE_CODE == "0TTBOZZ" ~ 'Cystectomy',
                                                                                  ICD_PROCEDURE_CODE == "39.52" | ICD_PROCEDURE_CODE == "39.53" ~ 'Arteriovenous'))

signalsCognitive<-signals_procedureSurgeryType %>% mutate(Cognition = case_when(AD8 >= 2 | SBT >= 5 ~ 1,
                                                                                        AD8 < 2  | SBT <5 ~ 0))
signalsCognitive1<- signalsCognitive%>% filter(!is.na(SurgeryType) )

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

discharge_status_data<-other %>% mutate(home_status = case_when(UB_DISCHARGE_DISPOSITION_DESCRIPTION %in% home_list ~ 1 , UB_DISCHARGE_DISPOSITION_DESCRIPTION %in% facility_list ~ 0))


signalsCognitive2 <- left_join(signalsCognitive1,discharge_status_data,by="CPAPPatientID")


#temp1<- glm(home_status ~ SurgeryType*Cognition, family = binomial,data = signalsCognitive2)
#summary(temp1)$coef
#exp(summary(temp1)$coef )
#require(MASS)
#temp2<-exp(cbind(coef(temp1), confint(temp1)))

count = 0
for(each_proc_code in unique(signalsCognitive2$SurgeryType)){
  temp_model <- glm(home_status~Cognition, data= signalsCognitive2 %>% filter(each_proc_code==signalsCognitive2$SurgeryType))
  temp_reslt <- exp(cbind(OR = coef(temp_model), confint.default(temp_model)))
  print(temp_reslt)
  
  temp_reslt1 <- data.table(temp_reslt)
  
  y1 <- each_proc_code
  temp_reslt1 <- temp_reslt1[ , `:=` (SurgeryType = y1)]
  
  coef_type <- c('intercept', 'abnrml_cog')
  temp_reslt1 <- temp_reslt1[ , `:=` (Coef_type = coef_type)]
  index <- count + 1
  temp_reslt1 <- temp_reslt1[ , `:=` (Index = index)]
  
  print(temp_reslt1)
  
  if(count==0){
    final_result <- temp_reslt1
  }
  else{
    final_result<- rbind(final_result, temp_reslt1)
  }
  count <- count + 1
  
  #print(exp(cbind(OR = coef(temp_model), confint.default(temp_model))))
}



colnames(final_result) <- c('Odds Ratio','lower_bound','upper_bound', 'surgery type','Coef_type', 'Index')


# when we wanted to run code with intercept data
final_result <- final_result[final_result$Coef_type=='intercept']

# when we want to run code with cognition data
final_result_abnl_cog <- final_result[final_result$Coef_type=='abnrml_cog']
final_result <- final_result_abnl_cog

library(ggplot2)
f_plot1<-ggplot(final_result, aes(y = Index, x=final_result$`Odds Ratio`))+
  geom_point(shape = 18, size = 5) +  
  geom_errorbarh(aes(xmin = lower_bound, xmax = upper_bound), height = 0.25) +
  geom_vline(xintercept = 1, color = "red", linetype = "dashed", cex = 1, alpha = 0.5) +
  scale_y_continuous(name = "", breaks=1:14, labels = final_result$`surgery type`, trans = "reverse") +
  xlab("Odds Ratio (95% CI)") + 
  ylab(" ") + 
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size = 12, colour = "black"),
        axis.text.x.bottom = element_text(size = 12, colour = "black"),
        axis.title.x = element_text(size = 12, colour = "black"))


## Create the table-base pallete
table_base <- ggplot(final_result, aes(y=final_result$`surgery type`)) +
  ylab(NULL) + xlab("  ") + 
  theme(plot.title = element_text(hjust = 0.5, size=12), 
        axis.text.x = element_text(color="white", hjust = -3, size = 25), ## This is used to help with alignment
        axis.line = element_blank(),
        axis.text.y = element_blank(), 
        axis.ticks = element_blank(),
        axis.title.y = element_blank(), 
        legend.position = "none",
        panel.background = element_blank(), 
        panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        plot.background = element_blank())       

## OR point estimate table
tab1 <- table_base + 
  labs(title = "space") +
  geom_text(aes(y = rev(Index), x = 1, label = sprintf("%0.2f", round(final_result$`Odds Ratio`, digits = 4))), size = 4) + ## decimal places
  ggtitle("OR")

## 95% CI table
tab2 <- table_base +
  geom_text(aes(y = rev(Index), x = 1, label = final_result$upper_bound), size = 4) + 
  ggtitle("95% CI")       
      
library(gridExtra)
## Merge tables with plot
lay <-  matrix(c(1,1,1,1,1,1,1,1,1,1,2,3,3), nrow = 1)
grid.arrange(f_plot1, tab1, tab2, layout_matrix = lay)
       
       


