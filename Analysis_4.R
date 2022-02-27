## analysis 4
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
library(lubridate)
inclusion = filter(Signals, Age_at_CPAP >= 65)
inclusion1 <- inclusion%>% filter( CPAP_Date >= as.Date("2013-01-01") & CPAP_Date <= as.Date("2018-06-30") )
inclusion2<- inclusion1 %>% filter(!is.na(AD8) ) %>% filter(!is.na(SBT) )

Procedure1 = filter(ProcedureCodes, ICD_CODE_VERSION == "9-CM" | ICD_CODE_VERSION =="10-PCS")
Procedure2<- Procedure1[Procedure1$ICD_PROCEDURE_CODE %in% c("0FT44ZZ","0DTN0ZZ", "0DB64Z3", "0FBG0ZZ", "0FBG3ZZ","0UT90ZZ", "0UB74ZZ", "0SG00A0","0SRC0J9","0SRB04Z","0RRJ00Z","0DQ53ZZ","0FT20ZZ","0TT10ZZ","0VT08ZZ","0TTB0ZZ","B50W","51.23","45.8","43.82","52.7","52.0","68.4","68.5","68.9","68.49","81.06","81.54","81.51","81.80","53.9","32.41","32.49","55.4","60.5","57.71","39.52","39.53"),]

#Procedure4 = Procedure2[!duplicated(Procedure2$CPAPPatientID),]

procedure5<-Procedure2 %>% mutate(procedurecode = case_when(ICD_PROCEDURE_CODE=="51.23" ~ 1, ICD_PROCEDURE_CODE=="45.8" ~ 2, ICD_PROCEDURE_CODE == "43.82" ~ 3, ICD_PROCEDURE_CODE== "52.7" ~ 4, ICD_PROCEDURE_CODE== "68.4" ~ 5, ICD_PROCEDURE_CODE == "52.0" ~ 6, ICD_PROCEDURE_CODE== "68.5" ~ 7, ICD_PROCEDURE_CODE== "68.9" ~ 8, ICD_PROCEDURE_CODE == "68.49" ~ 9, ICD_PROCEDURE_CODE== "81.06" ~ 10, ICD_PROCEDURE_CODE== "81.54" ~ 11, ICD_PROCEDURE_CODE == "81.51" ~ 12,ICD_PROCEDURE_CODE== "53.9" ~ 13, ICD_PROCEDURE_CODE== "32.41" ~ 14, ICD_PROCEDURE_CODE == "32.49" ~ 15, ICD_PROCEDURE_CODE== "55.4" ~ 16, ICD_PROCEDURE_CODE== "81.80" ~ 17, ICD_PROCEDURE_CODE == "60.5" ~ 18, ICD_PROCEDURE_CODE== "57.71" ~ 19, ICD_PROCEDURE_CODE== "39.52" ~ 20, ICD_PROCEDURE_CODE == "39.53" ~ 21, ICD_PROCEDURE_CODE== "0FT44ZZ" ~ 22, ICD_PROCEDURE_CODE== "0DTN0ZZ" ~ 23, ICD_PROCEDURE_CODE == "0DB64Z3" ~ 24, ICD_PROCEDURE_CODE== "0FBG0ZZ" ~ 25, ICD_PROCEDURE_CODE== "0FBG3ZZ" ~ 26, ICD_PROCEDURE_CODE == "0UT90ZZ" ~ 27, ICD_PROCEDURE_CODE== "0UB74ZZ" ~ 28, ICD_PROCEDURE_CODE== "0SG00A0" ~ 29, ICD_PROCEDURE_CODE == "0SRC0J9" ~ 30, ICD_PROCEDURE_CODE== "0SRB04Z" ~ 31, ICD_PROCEDURE_CODE== "0RRJ00Z" ~ 32, ICD_PROCEDURE_CODE == "0DQ53ZZ" ~ 33, ICD_PROCEDURE_CODE== "0DTN0ZZ" ~ 34, ICD_PROCEDURE_CODE== "0DQ53ZZ" ~ 35, ICD_PROCEDURE_CODE == "0FT20ZZ" ~ 36, ICD_PROCEDURE_CODE== "0TT10ZZ" ~ 37))

signals_procedure = inclusion2 %>% left_join(procedure5, by="CPAPPatientID")
signals_procedure_text = signals_procedure %>% left_join(TextSignals, by= "CPAPPatientID")

df <- data.frame(signals_procedure_text)
tab1 <- table(df$SBT,df$AD8)
tab1

#Analysis-4.1
siprotex<- signals_procedure_text
siprotex1<- as.data.frame(siprotex)
siprotex2<- siprotex1[,c( "SBT", "procedurecode", "Age_at_CPAP", "Dementia","Mild cognitive impairment", "Etiology of dementia or mild cognitive impairment" )]
siprotex2[is.na(siprotex2)]<-0
df2<-siprotex2[complete.cases(siprotex2),]

str(siprotex2)

test1 = siprotex2[(20001:25470),]
train1 = siprotex2[(1:20000),]

library(class)
lm_reg = lm(SBT~., data = train1, )
summary(lm_reg)

predicted = predict(lm_reg, newdata = test1)
predicted


TAB<-table(test1$SBT, predicted>5)
TAB
mcrate <- 1-sum(diag(TAB))/sum(TAB)
mcrate


TAB_high <- table(test1$SBT, predicted < 5)
TAB_high
mcrate_high<- 1-sum(diag(TAB_high))/sum(TAB_high)
mcrate_high



#Analysis-4.2

siprotex<- signals_procedure_text
siprotex1<- as.data.frame(siprotex)
siprotex2<- siprotex1[,c( "AD8", "procedurecode", "Age_at_CPAP", "Dementia","Mild cognitive impairment", "Etiology of dementia or mild cognitive impairment" )]
siprotex2[is.na(siprotex2)]<-0
df2<-siprotex2[complete.cases(siprotex2),]

str(siprotex2)

test1 = siprotex2[(20001:25470),]
train1 = siprotex2[(1:20000),]

library(class)
lm_reg = lm(AD8~., data = train1, )
summary(lm_reg)

predicted = predict(lm_reg, newdata = test1)
predicted


TAB<-table(test1$AD8, predicted>2)
TAB
mcrate <- 1-sum(diag(TAB))/sum(TAB)
mcrate


TAB_high <- table(test1$AD8, predicted < 2)
TAB_high
mcrate_high<- 1-sum(diag(TAB_high))/sum(TAB_high)
mcrate_high






#Analysis-4 part-1
sipro<- signals_procedure_text
ad8_cat <-  vector(mode = "list", length = 0)
for (x in sipro$AD8) {
  
  if (x>=2){
    ad8_cat <- c(ad8_cat, 1)
    
  }
  else{
    ad8_cat <- c(ad8_cat, 0)
  }
  
}

sbt_cat <-  vector(mode = "list", length = 0)
for (x in sipro$SBT) {
  
  if (x>=5){
    sbt_cat <- c(sbt_cat, 1)
    
  }
  else{
    sbt_cat <- c(sbt_cat, 0)
  }
  
}


siprotex <- sipro


siprotex$AD8_cat <- unlist(ad8_cat)
siprotex$SBT_cat <- unlist(sbt_cat)



siprotex1<- as.data.frame(siprotex)
siprotex2<- siprotex1[,c( "AD8_cat","procedurecode", "Age_at_CPAP", "Dementia","Mild cognitive impairment", "Etiology of dementia or mild cognitive impairment" )]
siprotex2[is.na(siprotex2)]<-0
df2<-siprotex2[complete.cases(siprotex2),]

str(siprotex2)

test1 = siprotex2[(20001:25470),]
train1 = siprotex2[(1:20000),]

library(class)

model_glm = glm(AD8_cat~., data = train1)
summary(model_glm)

predicted1 = predict(model_glm, newdata = test1, type = "response")
predicted1


TAB1<-table(test1$AD8_cat, predicted1>2)
TAB1
mcrate1 <- 1-sum(diag(TAB1))/sum(TAB1)
mcrate1

TAB2<-table(test1$AD8_cat, predicted1<2)
TAB2
mcrate2 <- 1-sum(diag(TAB2))/sum(TAB2)
mcrate2



#Analysis-4 part-2
sipro<-signals_procedure_text
ad8_cat <-  vector(mode = "list", length = 0)
for (x in sipro$AD8) {
  
  if (x>=2){
    ad8_cat <- c(ad8_cat, 1)
    
  }
  else{
    ad8_cat <- c(ad8_cat, 0)
  }
  
}

sbt_cat <-  vector(mode = "list", length = 0)
for (x in sipro$SBT) {
  
  if (x>=5){
    sbt_cat <- c(sbt_cat, 1)
    
  }
  else{
    sbt_cat <- c(sbt_cat, 0)
  }
  
}


siprotex <- sipro

siprotex$AD8_cat <- unlist(ad8_cat)
siprotex$SBT_cat <- unlist(sbt_cat)



siprotex1<- as.data.frame(siprotex)
siprotex2<- siprotex1[,c("SBT_cat", "procedurecode", "Age_at_CPAP", "Dementia","Mild cognitive impairment", "Etiology of dementia or mild cognitive impairment" )]
siprotex2[is.na(siprotex2)]<-0
df2<-siprotex2[complete.cases(siprotex2),]

str(siprotex2)

test1 = siprotex2[(20001:25470),]
train1 = siprotex2[(1:20000),]


library(class)

model_glm = glm(SBT_cat~., data = train1)
summary(model_glm)

predicted1 = predict(model_glm, newdata = test1, type = "response")
predicted1


TAB1<-table(test1$SBT_cat, predicted1>5)
TAB1
mcrate1 <- 1-sum(diag(TAB1))/sum(TAB1)
mcrate1

TAB2<-table(test1$SBT_cat, predicted1<5)
TAB2
mcrate2 <- 1-sum(diag(TAB2))/sum(TAB2)
mcrate2






