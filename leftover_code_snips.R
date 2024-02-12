
dtrain <- xgb.DMatrix(train$data, label=(hosp_proc$dc_status!="home" )%>% as.numeric)


bootfunction <- function(indicies, dtrain) {
  local_model <- xgb.train( data= xgboost::slice(dtrain ,indicies), 
    params= list(
      max_depth=transformed_params[1] 
    , eta=transformed_params[2]
    ,  min_child_weight=exp(transformed_params[4])
    , gamma=transformed_params[5]
    , subsample=transformed_params[6] 
    , lambda=exp(transformed_params[7])
    , num_parallel_tree=transformed_params[8] 
    , objective = "binary:logistic"
    , tree_method="auto"
    , eval_metric="logloss"
    )  , nrounds=transformed_params[3], verbose=0L )
  results <- predict(local_model , dtrain) 
  return(results)
}

train_or_test <- apply( folds, 2, function(x) {seq.int(length(x)) %in% x } ) 
   
predictions_mv_boot <- lapply(folds, bootfunction, dtrain )
   
temp <- train_set %>% select(-SBT) %>% select(-AD8) %>% mutate_all(as.numeric) %>% as.matrix
temp[!is.finite(temp)] <- NA
dtrain <- xgb.DMatrix(train$data, label=(hosp_proc$dc_status!="home" )%>% as.numeric)

predictions_mv_boot_null <- lapply(folds, bootfunction, dtrain )


## what is the rate at each possible set of thresholds? 
## two ways to handle unmeasured -> treated like a low number -> move to top
events_rates %>% pivot_wider(values_from="not_home", names_from="SBT", id_cols="AD8", names_prefix="SBT") %>% select(AD8, `SBTNA`, everything() ) %>% arrange(!is.na(AD8), AD8)   -> numerator 

events_rates %>% pivot_wider(values_from="n1", names_from="SBT", id_cols="AD8", names_prefix="SBT") %>% select(AD8, `SBTNA`, everything() ) %>% arrange(!is.na(AD8), AD8)  -> denominator 

round((numerator %>% select(-AD8) %>% as.matrix %>% single_thresh_sum) / (denominator %>% select(-AD8)%>% as.matrix %>% single_thresh_sum), 2)  %>% as_tibble %>% mutate(AD8=c(NA,0, seq.int(3))) %>% relocate(AD8) %>% write_csv("/research/outputs/sup_nonlinear_raw_cum_or_thresh.csv")  

png("/research/outputs/sup_nonlinear_raw_cum_or_thresh.png", res=300, width=4, height=4, units="in")
((numerator %>% select(-AD8) %>% as.matrix %>% single_thresh_sum) / (denominator %>% select(-AD8)%>% as.matrix %>% single_thresh_sum)) %>%heatmap( Colv = NA, Rowv = NA  %>% unlist, revC=T, ylab="AD8", scale="none", labRow= c(0, seq.int(3)),NA)
dev.off()

((numerator %>% as.matrix %>% `[`(,-1) %>% double_thresh_sum ) /  (denominator %>% as.matrix %>% `[`(,-1) %>% double_thresh_sum) ) %>% round(2) %>% as_tibble %>% mutate(AD8=c(NA,0, seq.int(3))) %>% relocate(AD8) %>% write_csv("/research/outputs/sup_nonlinear_raw_cum_thresh.csv") 


png("/research/outputs/sup_nonlinear_raw_cum_thresh_or.png", res=300, width=4, height=4, units="in")
((numerator %>% as.matrix %>% `[`(,-1) %>% double_thresh_sum ) /  (denominator %>% as.matrix %>% `[`(,-1) %>% double_thresh_sum) ) %>%heatmap( Colv = NA, Rowv = NA  %>% unlist, revC=T, ylab="AD8", scale="none", labRow= c(0, seq.int(3)),NA)
dev.off()

#### interaction analysis - raw data


merged_data2%>% mutate( AD8 = pmin(AD8, 3) %>% as.integer, SBT = pmin(SBT, 11)%>% as.integer, not_home = dc_home) %>%
  mutate(AD8 = if_else(is.finite(AD8), AD8, NA_integer_ ) , SBT = if_else(is.finite(SBT), SBT, NA_integer_ ) ) %>%
  group_by(AD8, SBT) %>% summarize(not_home=sum(not_home), n1=n() ) %>% ungroup -> events_rates
  
events_rates %>% mutate(not_home= not_home/n1)%>% 
  filter(n1>10) %>% pivot_wider(values_from="not_home", names_from="SBT", id_cols="AD8", names_prefix="SBT")  -> raw_rates

raw_rates %>% write_csv("/research/outputs/sup_nonlinear_raw_epic.csv")  

png("/research/outputs/sup_nonlinear_raw_epic.png", res=300, width=4, height=4, units="in")
heatmap(raw_rates[,-1] %>% as.matrix, Colv = NA, Rowv = NA, labRow=raw_rates[,1] %>% unlist, revC=T, ylab="AD8", scale="none")
dev.off()

events_rates %>% 
  pivot_wider(values_from="n1", names_from="SBT", id_cols="AD8", names_prefix="SBT")  -> raw_rates

raw_rates %>% write_csv("/research/outputs/sup_nonlinear_raw_n_epic.csv")  

png("/research/outputs/sup_nonlinear_raw_n_epic.png", res=300, width=4, height=4, units="in")
heatmap(raw_rates[,-1] %>% as.matrix %>% (function(x){x/sum(x,na.rm=T)}), Colv = NA, Rowv = NA, labRow=raw_rates[,1] %>% unlist, revC=T, ylab="AD8", scale="none")
dev.off()


## two ways to handle unmeasured -> treated like a low number -> move to top
events_rates %>% pivot_wider(values_from="not_home", names_from="SBT", id_cols="AD8", names_prefix="SBT") %>% select(AD8, `SBTNA`, everything() ) %>% arrange(!is.na(AD8), AD8)   -> numerator 

events_rates %>% pivot_wider(values_from="n1", names_from="SBT", id_cols="AD8", names_prefix="SBT") %>% select(AD8, `SBTNA`, everything() ) %>% arrange(!is.na(AD8), AD8)  -> denominator 

round((numerator %>% select(-AD8) %>% as.matrix %>% single_thresh_sum) / (denominator %>% select(-AD8)%>% as.matrix %>% single_thresh_sum), 2)  %>% as_tibble %>% mutate(AD8=c(NA,0, seq.int(3))) %>% relocate(AD8) %>% write_csv("/research/outputs/sup_nonlinear_raw_cum_or_thresh_epic.csv")  

png("/research/outputs/sup_nonlinear_raw_cum_thresh_or_epic.png", res=300, width=4, height=4, units="in")
((numerator %>% select(-AD8) %>% as.matrix %>% single_thresh_sum) / (denominator %>% select(-AD8)%>% as.matrix %>% single_thresh_sum)) %>%heatmap( Colv = NA, Rowv = NA  %>% unlist, revC=T, ylab="AD8", scale="none", labRow= c(0, seq.int(3)),NA)
dev.off()

((numerator %>% as.matrix %>% `[`(,-1) %>% double_thresh_sum ) /  (denominator %>% as.matrix %>% `[`(,-1) %>% double_thresh_sum) ) %>% round(2) %>% as_tibble %>% mutate(AD8=c(NA,0, seq.int(3))) %>% relocate(AD8) %>% write_csv("/research/outputs/sup_nonlinear_raw_cum_thresh_epic.csv") 


png("/research/outputs/sup_nonlinear_raw_cum_thresh_epic.png", res=300, width=4, height=4, units="in")
((numerator %>% as.matrix %>% `[`(,-1) %>% double_thresh_sum) /  (denominator %>% as.matrix %>% `[`(,-1) %>%double_thresh_sum ) )  %>%heatmap( Colv = NA, Rowv = NA  %>% unlist, revC=T, ylab="AD8", scale="none", labRow= c(NA,0, seq.int(3)))
dev.off()



hosp_proc %>% mutate( AD8 = pmin(AD8, 3) %>% as.integer, SBT = pmin(SBT, 11)%>% as.integer, not_home = dc_status!="home") %>%
  mutate(AD8 = if_else(is.finite(AD8), AD8, NA_integer_ ) , SBT = if_else(is.finite(SBT), SBT, NA_integer_ ) )  %>% group_by(AD8, SBT) %>% summarize(not_home=sum(not_home), n1=n() ) %>% ungroup  -> events_rates
  
## drop out rate with less than 10 denominator
events_rates %>% mutate(not_home= not_home/n1)%>% 
  filter(n1>10) %>% pivot_wider(values_from="not_home", names_from="SBT", id_cols="AD8", names_prefix="SBT")  -> raw_rates

raw_rates %>% write_csv("/research/outputs/sup_nonlinear_raw.csv")  

png("/research/outputs/sup_nonlinear_raw.png", res=300, width=4, height=4, units="in")
heatmap(raw_rates[,-1] %>% as.matrix, Colv = NA, Rowv = NA, labRow=raw_rates[,1] %>% unlist, revC=T, ylab="AD8", scale="none")
dev.off()

events_rates %>% 
  pivot_wider(values_from="n1", names_from="SBT", id_cols="AD8", names_prefix="SBT")  -> raw_rates

raw_rates %>% write_csv("/research/outputs/sup_nonlinear_raw_n.csv")  

png("/research/outputs/sup_nonlinear_raw_n.png", res=300, width=4, height=4, units="in")
heatmap(raw_rates[,-1] %>% as.matrix %>% (function(x){x/sum(x,na.rm=T)}), Colv = NA, Rowv = NA, labRow=raw_rates[,1] %>% unlist, revC=T, ylab="AD8", scale="none")
dev.off()

   
# 
# events_rates %>% pivot_wider(values_from="imputedn", names_from="SBT", id_cols="AD8", names_prefix="SBT") %>% select(AD8, `SBTNA`, everything() ) %>% arrange(!is.na(AD8), AD8)   -> numerator 
# 
# events_rates %>% pivot_wider(values_from="n1", names_from="SBT", id_cols="AD8", names_prefix="SBT") %>% select(AD8, `SBTNA`, everything() ) %>% arrange(!is.na(AD8), AD8)  -> denominator 
# 
# round((numerator %>% select(-AD8) %>% as.matrix %>% single_thresh_sum) / (denominator %>% select(-AD8)%>% as.matrix %>% single_thresh_sum), 2)  %>% as_tibble %>% mutate(AD8=c(NA,0, seq.int(3))) %>% relocate(AD8) %>% filter(is.finite(AD8)) %>% select(-SBTNA) %>% write_csv("/research/outputs/sup_nonlinear_marginal_effect_epic.csv") 



events_rates$imputed_rate <- NA_real_
for( i in seq_len(nrow(events_rates))) {
  temp_data <- train_set %>% mutate(AD8=as.numeric(events_rates[i,"AD8"] ), SBT = as.numeric(events_rates[i,"SBT"] ) ) %>% mutate_all(as.numeric) %>% as.matrix
  temp_data[!is.finite(temp_data)] <- NA
  events_rates$imputed_rate[i] <- mean(predict(whole_data_classifier, temp_data))
}

events_rates %<>% mutate( imputedn = imputed_rate*n1)
events_rates %>% pivot_wider(values_from="imputed_rate", names_from="SBT", id_cols="AD8", names_prefix="SBT")%>% write_csv("/research/outputs/sup_nonlinear_imputed_rates_epic.csv") 

events_rates %>% select(AD8, SBT, n1, not_home= imputedn) %>% thresh_rate_f %>% filter(is.finite(AD8) & is.finite(SBT)) %>% pivot_wider(values_from="thresh_rate", names_from="SBT", id_cols="AD8", names_prefix="SBT") %>% write_csv("/research/outputs/sup_nonlinear_marginal_effect_epic.csv") 

events_rates %>% select(AD8, SBT, n1, not_home= imputedn) %>% thresh_rate_and_f %>% filter(is.finite(AD8) & is.finite(SBT)) %>% pivot_wider(values_from="thresh_rate", names_from="SBT", id_cols="AD8", names_prefix="SBT") %>% write_csv("/research/outputs/sup_nonlinear_marginal_effect_and_epic.csv") 


## bootstrap accuracy with xgb
## returns a list containing predictions, true values, SBT, AD8 -> post processing for things like PPV
## because xgb is multicore, there is minimal gain to further multicoring the outer procecess
n_boot <- 500
folds <- replicate((n_boot), sample.int(nrow(merged_data2), replace=TRUE ) )
transformed_params <- cv_results_mv[[1]]

train_set <- hosp_proc %>% select(one_of(comborbid_vars, factor_vars, surg_vars, year_vars, "AD8", "SBT", "Age_at_CPAP") )

temp <- train_set %>% mutate_all(as.numeric) %>% as.matrix
temp[!is.finite(temp)] <- NA
