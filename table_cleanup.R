custom_transpose <- function(tempmv, tempep, digits=2) {
temp1 <- tempmv[,-'AD8'] %>% transpose %>% set_colnames(tempmv$AD8  %>% as.character %>% fcoalesce("NA"))
temp1 <-temp1 [,round(.SD, digits)]
temp1[, "SBT":=sub(colnames(tempmv)[-1], pattern="SBT", replacement="") ]
temp2 <- tempep[,-'AD8'] %>% transpose %>% set_colnames(tempep$AD8  %>% as.character %>% fcoalesce("NA")) 
temp2 <-temp2 [,round(.SD, digits)]
temp2[, "SBT":=sub(colnames(tempep)[-1], pattern="SBT", replacement="") ]
merge(temp1, temp2, by="SBT", suffix=c("",".2"), all=T)[order(as.numeric(SBT))]
}
tempmv<- fread("sup_nonlinear_raw.csv")
tempep<- fread("sup_nonlinear_raw_epic.csv")

custom_transpose(tempmv, tempep) %>% fwrite("raw_rates_joined.csv")

tempmv<- fread("sup_nonlinear_imputed_rates.csv")
tempep<- fread("sup_nonlinear_imputed_rates_epic.csv")
custom_transpose(tempmv, tempep) %>% fwrite("imputed_rates_joined.csv")

tempmv<- fread("sup_nonlinear_marginal_effect_thresholds_raw.csv")
tempep<- fread("sup_nonlinear_marginal_effect_thresholds_raw_epic.csv")
custom_transpose(tempmv, tempep) %>% fwrite("raw_thresholds_joined.csv")

tempmv<- fread("sup_nonlinear_marginal_effect_thresholds.csv")
tempep<- fread("sup_nonlinear_marginal_effect_thresholds_epic.csv")
custom_transpose(tempmv, tempep) %>% fwrite("adjusted_thresholds_joined.csv")
  
