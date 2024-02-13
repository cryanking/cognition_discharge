library("magrittr")
library("tidyverse")
library(modelr)
library(xgboost)
source("/code/xbg_cv.R")
library(pROC)
library(splines)


## this is getting unexpectedly near the upper bounds on many params (max depth, eta, nrounds, gamma). high eta, nrounds, depth -> want a bigger and more aggressive tree, high gamma -> only take splits closer to median (max of 5 pretty arbitrary)
## this has been extremely unstable in the HP it finds, usually finding parameters that are much more conservative that default

## what is the argument for CV here? For regularization?
## - As long as HP are consistent, the added value of feature for prediction can be validly compared
## - However, some HP will tend to minimize the predictive value of the feature from over-regularization (feature not allowed to work) or under-regularization (predictor model has such bad over-fitting that predictive performance is nill)
## - HP will also affect the degree of adjustment by covariables
## - for the purpose of displaying the average effect, any set of HP will work, but high levels of regularization will tend to reduce the impact. lowest level of regularization -> what is the local rate

## I reduced to range on unbounded params to be a factor of 10 from the default, which doesn't have a huge impact. You can imagine that some params are jointly thrown off because they counteract e.g. wildly conservative implcations of others
## -- the default params result in very substantial over-fitting (split sample auc 0.69 vs whole sample 0.93)
## -- the cv results is much more conservative parameters: shallower trees (2), low learning rate (0.1)
## -- This commonly results in null models wrt AD8 and SBT
## There are two possible directions
## -- the optimal model for prediction may not be the optimal model for adjustment
## -- the optimal hp for the covariates may not be the optimal hp for main variable -> suggests a two-stage model
## -- it is not easy to constrain new trees to use only some features, and "looser" hp will apply to all
## --  -- colsample_bytree = 2/ncol, feature_weights = (1,1,1e-6) could do it, but disallows interaction terms
## -- using the colsample mechanism seems to allow forcing the inclusion of those variables
## -- with 50 trees and a low depth, it isn't suprising that not all features are included
## -- TODO: initial the model on CIvar = 0, then update with a high colsample. The outcomes will be optimally adjusted, and the HP won't be over tuned



na_zero <- function(x) {  replace_na(x,0) }
na_false <- function(x) {  replace_na(x,FALSE) }

encode_onehot <- function(x, colname_prefix = "", colname_suffix = "") {
  if (!is.factor(x)) {
      x <- as.factor(x)
  }
  encoding_matrix <- contrasts(x, contrasts = FALSE)
  encoded_data <- encoding_matrix[as.integer(x),]
  colnames(encoded_data) <- paste0(colname_prefix, colnames(encoded_data), colname_suffix)
  encoded_data
}

## functions to generate the cumulative rates
## upper square - the AND criteria (meeting both thresholds)
# double_thresh_sum<- . %>% na_zero %>% apply(1, . %>% rev %>% cumsum %>% rev) %>% apply(1, . %>% rev %>% cumsum %>% rev)

## upper square - the or criteria (meeting either thresholds)
# single_thresh_sum <- function(x) {x<- na_zero(x); sum(x, na.rm=T) - apply(apply(x, 1, cumsum), 1, cumsum)  }

## the cleverness with cumsum is hard to validate, just write a loop
thresh_rate_f <- function(x) {
x$thresh_rate <- NA
  for(i in na.omit(unique(x$AD8 ) ) ) {
  for(j in na.omit(unique(x$SBT ))) {
    x[ intersect( which(x$AD8 == i) , which(x$SBT==j) ),  "thresh_rate" ] <- x %>% filter((AD8 >= i) | (SBT >= j) ) %>% summarize(sum(not_home)/sum(n1)) %>% unlist  ## TRUE | NA is TRUE, so this criteria works appropriately
  }}
  return(x)
}

thresh_rate_and_f <- function(x) {
x$thresh_rate <- NA
  for(i in na.omit(unique(x$AD8 ) ) ) {
  for(j in na.omit(unique(x$SBT ))) {
    x[ intersect( which(x$AD8 == i) , which(x$SBT==j) ),  "thresh_rate" ] <- x %>% filter((AD8 >= i) & (SBT >= j) ) %>% summarize(sum(not_home)/sum(n1)) %>% unlist  
  }}
  return(x)
}



merged_data2 <- readRDS("/research/merged_data2.RDS" )

hosp_proc <- readRDS("/research/merged_data.RDS" )
hosp_proc %<>% mutate(year=lubridate::year(AnestStart) )


comborbid_vars_mv <- c("COPD" , "Congestive heart failure" , "Diabetes mellitus" , "Current cancer", "Cerebrovascular disease" , "Cerebrovascular disease, stroke, or TIA" , "CVA" , "TIA", "CAD", "CKD")
factor_vars_mv <- c( "sex", "low_functional_capacity" )
year_vars_mv <- colnames(hosp_proc) %>% grep(pattern = "year_", value=T)
comborbid_vars_mv <- setdiff(comborbid_vars_mv, c("Cerebrovascular disease, stroke, or TIA", "TIA","Cerebrovascular disease" ))
surg_vars_mv <- colnames(hosp_proc) %>% grep(pattern="SType_", value=T)
comborbid_vars_mv %<>% gsub(pattern=" +", replacement="_")


train_set_mv <- hosp_proc %>% select(one_of(comborbid_vars_mv, factor_vars_mv, surg_vars_mv, year_vars_mv, "Age_at_CPAP", "AD8", "SBT" ) ) %>%   mutate(AD8 = if_else(is.finite(AD8), AD8, NA_real_ ) , SBT = if_else(is.finite(SBT), SBT, NA_real_ ) ) %>% mutate_all(as.numeric)


#############
## GOAL: 
##  --Estimate the "local rate" keeping confounders constant
##  --Use that to display the estimated value of different thresholds
##  --Estimate (with CI) the difference setting congnitive variables to threshold +1 (AD8=3, SBT=6) - rate (0,0)
##  --Estimate with CI the difference using the threshold indicator variable
#############

if(FALSE) {
## Naively fitting XGB had very substantial over-fitting (but made a nice plot)
  whole_data_classifier_default <- xgboost( label=(hosp_proc$dc_status!="home" )%>% as.numeric, data=train_set_mv %>% as.matrix , nrounds=50)
}

## Use crossvalidation to determine the optimal parameters
## subsample makes sobol search less reliable, gamma (min_split_loss) was difficult to get to fit
## Even shorter trees (max_depth = 2) can work about as well, but end up not including most variables
cv_results_mv <- xgboost_cv(label=(hosp_proc$dc_status!="home" )%>% as.numeric, data=train_set_mv %>%   mutate(AD8 = 0., SBT=0.) %>% as.matrix %>% xgb.DMatrix , sobol_size=100, nrounds=50 , 
upper_bounds= c(10L, .6, 15L, log(5), 8, .99 , log(10), 1L) , 
lower_bounds = c(4L, .1,  5L, log(0.01), 0, .7  , log(.1) , 1L) , 
gamma=0 , subsample=1)

confounder_only_classifier_mv <- cv_results_mv[[3]]


transformed_params  <- set_names(cv_results_mv[[1]], c("max_depth","eta","nrounds","min_child_weight","gamma","subsample","lambda","num_parallel_tree"  ) )
transformed_params ["min_child_weight"] %<>% exp
transformed_params ["lambda"]%<>% exp

## To get a high validity function of (outcome)  ~ (AD8, SBT) without many difficult to interpret interaction terms, use the double-debiased approach: cross validate an (outcome~confounders) and (exposure~confounders) model, then on the held out data compute (residual~residual)

## TODO compare with the obscure-then-colsample approach, which would allow doing the final calculation on the log-odds scale
## TODO: consider a second set of params for the residual~residual model
ddml_estimator <- function(data, exposure, label, nfold, params, exposure_objective="reg:squarederror") {
## unlike generic xgb or single adjustment, ddl can not natively handle NA in exposure (the residual is also NA, which breaks the orthogonolization assumption)
  nrounds <- params[["nrounds"]]
  params <- params[names(params) != "nrounds"]
  keep_these <- exposure %>% as.data.frame %>%   as.matrix %>% apply(1, .%>% is.finite %>% all)
  label <- label[keep_these]
  data<- data[keep_these,]
  exposure <- lapply(exposure, function(x){x[keep_these]} )

## set up folds
#   folds <- replicate(nfold, sample.int(n=nrow(data), replace=FALSE ) )
  folds <- split( seq_len(nrow(data)) , sample(rep(seq_len(nfold), length.out = nrow(data))))
  predicted_exposure <- bit::clone(exposure)
  predicted_label <- bit::clone(label)
  label_model <-vector('list', nfold)
  exposure_model <-vector('list', nfold)
  
  for( fold_index in seq_len(nfold)){
    ## train the label model 
    label_model[[fold_index]] <- xgb.train(data= xgb.DMatrix(data[as.integer(unlist(folds[-fold_index])) , ], label=label[as.integer(unlist(folds[-fold_index])) ] ), params=c(params, objective="binary:logistic"), nrounds)
    
    predicted_label[as.integer(folds[[fold_index]]) ] <- predict(label_model[[fold_index]],  xgb.DMatrix(data[as.integer(folds[[fold_index]]),] )  )
    ## train the (multid) exposure model
    exposure_model[[fold_index]] <- vector('list', length(exposure)) 
    
    for( exposure_index in seq_len(length(exposure))) {
      exposure_model[[fold_index]][[exposure_index]] <- xgb.train(data= xgb.DMatrix(data[as.integer(unlist(folds[-fold_index])), ], label=exposure[[exposure_index]][as.integer(unlist(folds[-fold_index]))] ), params=c(params, objective=exposure_objective) , nrounds)
      
      predicted_exposure[[exposure_index]][as.integer(folds[[fold_index]]) ] <- predict(exposure_model[[fold_index]][[exposure_index]],  xgb.DMatrix(data[as.integer(folds[[fold_index]]),] )  )
    }
  }
  ## train the low-d model of residual outcomes vs residual exposure
  residual_exposure <- (exposure %>% as.data.frame %>% as.matrix) - (predicted_exposure %>% as.data.frame %>% as.matrix)
  colnames(residual_exposure ) <- names(exposure)
  residual_model <- xgb.train(data= xgb.DMatrix( residual_exposure , label = label-predicted_label), params=params,  nrounds) 
  
  ## for the possible values of exposure
  exposure_levels <- exposure %>% as.data.frame %>% unique %>% arrange %>%  as.matrix 
  predicted_frequency <- vector('numeric', nrow(exposure_levels))
#   exposure_levels <- exposure_levels [ apply(exposure_levels,1, .%>% is.finite %>% all) , , drop=FALSE]
  ## calculate the offet and fit
  for (i in seq_len(nrow(exposure_levels))) {
    predicted_frequency[i] <-  mean( pmax(0,pmin(1, predict( residual_model, xgb.DMatrix( rep(as.numeric(exposure_levels[i,]) , each=nrow(residual_exposure) ) - (predicted_exposure %>% as.data.frame %>% as.matrix)  )) + predicted_label ) ) )
  
  }
  
  exposure_levels <- cbind(exposure_levels,predicted_frequency )
  ## I have no actual use for the rest of these
  return(list(exposure_map=exposure_levels  ))
}


ddl_fit <- ddml_estimator(
  data=train_set_mv %>% select(-AD8) %>% select(-SBT) %>% as.matrix , 
  label=(hosp_proc$dc_status!="home" )%>% as.numeric , 
  exposure = train_set_mv %>% select(AD8 , SBT)%>% mutate( AD8 = pmin(AD8, 3) %>% as.integer, SBT = pmin(SBT, 11)%>% as.integer) %>% as.list  , 
  nfold=5, params=transformed_params %>% as.list )

ddl_fit[[1]] %>% as_tibble %>% 
  mutate(ADB=as.integer(AD8), SBT=as.integer(SBT)) %>% 
  pivot_wider(values_from="predicted_frequency", names_from="SBT", id_cols="AD8", names_prefix="SBT") %>% 
  arrange(AD8) %>% 
  (function(x) {x[,order(colnames(x) %>% sub(pattern="[A-Z]+",replacement="" ) %>% as.integer )]}) %>%
  relocate(AD8) %>% 
  mutate(across(starts_with("SBT"), . %>% round(3) ))  %>% 
  write_csv("/research/outputs/sup_nonlinear_imputed_rates.csv") 

## transform this to thresholds
events_rates <- hosp_proc %>% mutate( AD8 = pmin(AD8, 3) %>% as.integer, SBT = pmin(SBT, 11)%>% as.integer, not_home = dc_status!="home") %>%
  mutate(AD8 = if_else(is.finite(AD8), AD8, NA_integer_ ) , SBT = if_else(is.finite(SBT), SBT, NA_integer_ ) )  %>%
  group_by(AD8, SBT) %>% 
  summarize(not_home=sum(not_home), n1=n() ) %>% ungroup  
  
ddl_fit[[1]] %>% as_tibble %>% 
  mutate(ADB=as.integer(AD8), SBT=as.integer(SBT)) %>% 
  left_join( events_rates, by=c("AD8", "SBT") ) %>%
  mutate( not_home = predicted_frequency*n1) %>%
  thresh_rate_f %>% 
  pivot_wider(values_from="thresh_rate", names_from="SBT", id_cols="AD8", names_prefix="SBT") %>% 
  arrange(AD8) %>% 
  (function(x) {x[,order(colnames(x) %>% sub(pattern="[A-Z]+",replacement="" ) %>% as.integer )]}) %>%
  relocate(AD8) %>%  
  write_csv("/research/outputs/sup_nonlinear_marginal_effect_thresholds.csv") 
  
## unadjusted rates
events_rates %>% mutate(not_home= not_home/n1) %>% 
  filter(n1>10) %>% 
  pivot_wider(values_from="not_home", names_from="SBT", id_cols="AD8", names_prefix="SBT")  %>% 
  write_csv("/research/outputs/sup_nonlinear_raw.csv")  

## unadjusted thresholds
events_rates %>% select(AD8, SBT, n1, not_home) %>% 
thresh_rate_f %>% 
filter(is.finite(AD8) & is.finite(SBT)) %>% 
pivot_wider(values_from="thresh_rate", names_from="SBT", id_cols="AD8", names_prefix="SBT") %>% 
write_csv("/research/outputs/sup_nonlinear_marginal_effect_thresholds_raw.csv") 

events_rates %>% 
  pivot_wider(values_from="n1", names_from="SBT", id_cols="AD8", names_prefix="SBT") %>%
  write_csv("/research/outputs/raw_n_mv.csv")  

##
## pure cross validation tended not to always include many features, and also is less valid
## 

if(FALSE) {
  whole_data_classifier_default <- xgb.train( data=xgb.DMatrix(temp_data , label=(hosp_proc$dc_status!="home" )%>% as.numeric) , nrounds=5)

#   whole_data_classifier_default <- xgb.train( data=xgb.DMatrix( train_set_mv %>% mutate(AD8=0, SBT=0) %>% as.matrix, label=(hosp_proc$dc_status!="home" )%>% as.numeric) , nrounds=50)
  # xgb_half1 <- xgboost(label=(hosp_proc$dc_status!="home" )[seq.int(3000)] %>% as.numeric , data=temp_data[seq.int(3000),]  ,nrounds=30, objective = "binary:logistic" )

  # auc(response=(hosp_proc$dc_status!="home" )[-seq.int(3000)] %>% as.numeric, predictor=predict(whole_data_classifier_mv,temp_data[-seq.int(3000),]) )
  # auc(response=(hosp_proc$dc_status!="home" )[-seq.int(3000)] %>% as.numeric, predictor=predict(whole_data_classifier_default,temp_data[-seq.int(3000),]) )
  auc(response=(hosp_proc$dc_status!="home" )[-seq.int(3000)] %>% as.numeric, predictor=predict(xgb_half1,temp_data[-seq.int(3000),]) )

  events_rates$imputed_rate <- NA_real_
  for( i in seq_len(nrow(events_rates))) {
    temp_data <- train_set_mv %>% mutate(AD8=as.numeric(events_rates[i,"AD8"] ), SBT = as.numeric(events_rates[i,"SBT"] ) ) %>% mutate_all(as.numeric) %>% as.matrix
    temp_data[!is.finite(temp_data)] <- NA
    events_rates$imputed_rate[i] <- mean(predict(whole_data_classifier_default, temp_data))
  }
  events_rates %<>% mutate( imputedn = imputed_rate*n1)
events_rates %>% pivot_wider(values_from="imputed_rate", names_from="SBT", id_cols="AD8", names_prefix="SBT") %>% write_csv("/research/outputs/sup_nonlinear_imputed_rates.csv") 
events_rates %>% select(AD8, SBT, n1, not_home= imputedn) %>% thresh_rate_f %>% filter(is.finite(AD8) & is.finite(SBT)) %>% pivot_wider(values_from="thresh_rate", names_from="SBT", id_cols="AD8", names_prefix="SBT") %>% write_csv("/research/outputs/sup_nonlinear_marginal_effect.csv") 

events_rates %>% select(AD8, SBT, n1, not_home= imputedn) %>% thresh_rate_and_f %>% filter(is.finite(AD8) & is.finite(SBT)) %>% pivot_wider(values_from="thresh_rate", names_from="SBT", id_cols="AD8", names_prefix="SBT") %>% write_csv("/research/outputs/sup_nonlinear_marginal_effect_and.csv") 

}


####
## bootstrap the prediction delta using thresholds
## this has a nice clt on the parameter, so the predictions likely are smooth for simple boot methods


test_stat <-function(data_in, indicies , labels_in) {
 ddml_estimator(
  data= (data_in %>% select(-AD8) %>% select(-SBT) %>% as.matrix) [indicies,, drop=FALSE] , 
  label=labels_in[indicies] , 
  exposure = (data_in %>% mutate(cog = na_false(AD8 > 2 | SBT > 5) ) %>% pull("cog"))[indicies]  %>% list %>% set_names("cog")  , 
  nfold=5, params=transformed_params %>% as.list )[[1]][,2] %>% diff %>% return
}

boot_result <- boot(data=train_set_mv, statistic=test_stat, R=5000, labels_in=(hosp_proc$dc_status!="home" )%>% as.numeric)

boot_result %>% saveRDS(file="/research/outputs/mv_ddml_boot.RDS")



c(boot_result$t0, boot_result$t %>% colMeans, boot.ci(boot_result, type="basic")$basic[,c(4,5)]  ) %>% matrix(nrow=1) %>% set_colnames(c("whole_sample_difference","mean boot difference","lower_ci_boot", "upper_ci_boot")) %>% round(3) %>% as.data.frame %>% write_csv("/research/outputs/mv_ddml_boot.csv")

## the improved predictive performance

base_form <- "thisout ~ 0" %>% formula
surg_form <- paste0(surg_vars_mv, collapse=" + ")
year_form <- paste0(year_vars_mv, collapse = " + ")
global_age_spline <- splines::bs(hosp_proc$Age_at_CPAP, 8)
comorbid_form <- paste0(c(comborbid_vars_mv, factor_vars_mv) ,  collapse=" + ")

myform <- base_form %>% 
  update( paste0("~.+", year_form) ) %>%
  update( paste0("~.+", surg_form) ) %>% 
  update( paste0("~.+", comorbid_form) ) %>%
  update( "~.+predict(global_age_spline,Age_at_CPAP)" ) 

cogform <- myform %>%  update( "~.+AbnCog" ) 

#   dc_home_glm <- hosp_proc %>% mutate(thisout=dc_status!="home") %>% mutate(AbnCog= as.numeric(AbnCog)) %>% glm(data=., formula=myform,  family=binomial() ) 
#   predict_null <- predict(dc_home_glm, newdata=hosp_proc %>% mutate(AbnCog= as.numeric(AbnCog)) , type="response")
#   
#   dc_home_glm_alt <- hosp_proc %>% mutate(thisout=dc_status!="home") %>% mutate(AbnCog= as.numeric(AbnCog)) %>% glm(data=., formula=update(myform , formula("~.+AbnCog") ) ,  family=binomial() ) 
#   predict_alt <- predict(dc_home_glm_alt,hosp_proc %>% mutate(AbnCog= as.numeric(AbnCog))  , type="response" )
#   
#   cutpointr(x=predict_alt, class=hosp_proc$dc_status!="home" , metric=youden)
#   cutpointr(x=predict_null, class=hosp_proc$dc_status!="home" , metric=youden)
  
test_stat_acc <- function(data, indicies)  {
  capture.output( 
  {
#   sink(file="/dev/null", type="message")
  if(length(setdiff(indicies, seq.int(nrow(data)))) ==0 ) {
  holdout <- data
  } else {
  holdout <- data[-unique(indicies),]  
  }

  dc_home_glm <- data[indicies,] %>% glm(data=., formula=myform,  family=binomial() ) 
  predict_null <- predict(dc_home_glm,holdout)

  dc_home_glm_alt <- glm(data=data[indicies,] , formula=cogform,  family=binomial() ) 
  predict_alt <- predict(dc_home_glm_alt,holdout)
  
  cutpoint_null <- cutpointr(x=predict_null, class=holdout$thisout , metric=youden)
  cutpoint_null %<>% add_metric( list(ppv, npv, precision))
  
#   sapply( list(sensitivity,  specificity, ppv, npv, precision),  function(X)  do.call(X, as.list(set_names(as.vector(table(predict(cutpoint_null, newdata=data.frame(x=predict_alt)), hosp_proc$dc_status!="home")), c("tn","fp","fn","tp") ) ) ) )

  null_stats <- cutpoint_null %>% select(sensitivity,  specificity, ppv, npv, precision) %>% as.numeric
  
  cutpoint_alt <- cutpointr(x=predict_alt, class=holdout$thisout , metric=youden)
  cutpoint_alt %<>% add_metric( list(ppv, npv, precision))
  
#   alt_stats <- sapply( list(sensitivity,  specificity, ppv, npv, precision),  function(X)  do.call(X, as.list(set_names(as.vector(table(as.numeric(cutpoint_null$optimal_cutpoint) < predict_alt , holdout$thisout ) ), c("tn","fp","fn","tp") ) ) ) )
  alt_stats <- cutpoint_alt %>% select(sensitivity,  specificity, ppv, npv, precision) %>% as.numeric
  
  results <- c( 
    pROC::auc(response=holdout$thisout,predictor=predict_alt, levels=c(FALSE, TRUE) ) ,
    pROC::auc(response=holdout$thisout,predictor=predict_null, levels=c(FALSE, TRUE) ) , 
    pROC::auc(response=holdout$thisout,predictor=predict_alt, levels=c(FALSE, TRUE) ) - pROC::auc(response=holdout$thisout,predictor=predict_null, levels=c(FALSE, TRUE) ) , 
    null_stats,
    alt_stats,
    alt_stats - null_stats
  )
  } 
  , type="message")
#   sink( type="message")
  return( results)
}

boot_result_simple <- boot(data=hosp_proc %>% mutate(thisout=(dc_status!="home") ) %>% mutate(AbnCog= as.numeric(AbnCog)),  statistic=test_stat_acc, R=5000)

boot_result_simple %>% saveRDS(file="/research/outputs/mv_binary_acc_boot.RDS")

accuracy_summary <- cbind(boot_result_simple$t0[c(1,2, 3,14:18)],
boot_result_simple$t[,c(1,2,3,14:18)] %>% colMeans,
rbind(
boot.ci(boot_result_simple, index=1, type="basic")$basic[,c(4,5)] ,
boot.ci(boot_result_simple, index=2, type="basic")$basic[,c(4,5)] ,
boot.ci(boot_result_simple, index=3, type="basic")$basic[,c(4,5)] ,
boot.ci(boot_result_simple, index=14, type="basic")$basic[,c(4,5)] ,
boot.ci(boot_result_simple, index=15, type="basic")$basic[,c(4,5)] ,
boot.ci(boot_result_simple, index=16, type="basic")$basic[,c(4,5)] ,
boot.ci(boot_result_simple, index=17, type="basic")$basic[,c(4,5)] ,
boot.ci(boot_result_simple, index=18, type="basic")$basic[,c(4,5)] )
) %>% set_colnames(c("whole_sample_difference","mean out-of-bag difference","lower_ci_oob", "upper_ci_oob")) %>% round(3) %>% as.data.frame %>% set_rownames( c("auc",'sensitivity',  'specificity', 'ppv', 'npv', 'precision'))

accuracy_summary %>% write_csv("/research/outputs/mv_bin_acc_summary.csv")

## sandwich SE over surgeries

## are a meaningful fraction multiple surgeries? Will have to sub-sample
## MV has already been sub-sampled (assigned in order of procedure code ranking in billing)
hosp_proc %>% select(one_of(surg_vars_mv)) %>% as.matrix %>% rowSums %>% table

temp_data <- hosp_proc %>% mutate(thisout=(dc_status!="home") ) %>% mutate(AbnCog= as.numeric(AbnCog)) %>% as.data.frame
glm(data=temp_data, formula=cogform , family=binomial() ) -> ci_glm_mv

surgery_v <- vcovCL(ci_glm_mv, cluster = ~SurgeryType , cadjust = FALSE, type="HC1")
thistest <- coeftest(ci_glm_mv, surgery_v)
cbind(thistest["AbnCog",, drop=FALSE] , confint(thistest,parm="AbnCog") ) %>% as.data.frame %>% write_csv("/research/outputs/mv_surg_sandwich.csv")




#######################
#### cohort 2
#######################


# merged_data2 %>% select(one_of(surg_vars)) %>% as.matrix %>% rowSums %>% table
## undo the one hot encoding, prefer the least common surgery type
## could do this with an apply, slightly more clever with coalesce 
surg_type <- merged_data2 %>% select(ends_with("_codes"))
## order by column freq
surg_type<-relocate(surg_type,names(sort(colSums(surg_type %>% as.matrix))))
for(i in colnames(surg_type)) {
  surg_type[[i]] <- if_else(surg_type[[i]], i, NA_character_ ) 
}
merged_data2[["surgType"]] <- do.call(coalesce,surg_type) 

merged_data2 %<>% mutate( low_functional_capacity = na_false(low_functional_capacity) , ESRD=na_zero(ESRD) )

comborbid_vars <- c("COPD" , "CAD" , "CKD" , "CHF" , "CVA_TIA" , "cancerStatus", "Diabetes","Sex", "ESRD","low_functional_capacity" )

merged_data2 %<>% rename(CVA_TIA = `CVA(TIA)`)

surg_vars <- colnames(merged_data2) %>% grep(pattern="_code", value=T)
year_vars <- colnames(merged_data2) %>% grep(pattern = "year_", value=T)

train_set <- merged_data2 %>% select(one_of(comborbid_vars, surg_vars, year_vars, "AD8", "SBT", "age") ) %>%   mutate(AD8 = if_else(is.finite(AD8), AD8, NA_real_ ) , SBT = if_else(is.finite(SBT), SBT, NA_real_ ) ) %>% mutate_all(as.numeric)

label_in <- (merged_data2$dc_home )%>% as.numeric

cv_results_epic <- xgboost_cv(label=label_in, data=train_set %>%   mutate(AD8 = 0., SBT=0.) %>% as.matrix %>% xgb.DMatrix , sobol_size=100, nrounds=50 , 
upper_bounds= c(10L, .6, 15L, log(5), 8, .99 , log(10), 1L) , 
lower_bounds = c(4L, .1,  5L, log(0.01), 0, .7  , log(.1) , 1L) , 
gamma=0 , subsample=1)

transformed_params  <- set_names(cv_results_epic[[1]], c("max_depth","eta","nrounds","min_child_weight","gamma","subsample","lambda","num_parallel_tree"  ) )
transformed_params ["min_child_weight"] %<>% exp
transformed_params ["lambda"]%<>% exp



ddl_fit <- ddml_estimator(
  data=train_set %>% select(-AD8) %>% select(-SBT) %>% as.matrix , 
  label=label_in , 
  exposure = train_set %>% select(AD8 , SBT)%>% mutate( AD8 = pmin(AD8, 3) %>% as.integer, SBT = pmin(SBT, 11)%>% as.integer) %>% as.list  , 
  nfold=5, params=transformed_params %>% as.list )

ddl_fit[[1]] %>% as_tibble %>% 
  mutate(ADB=as.integer(AD8), SBT=as.integer(SBT)) %>% 
  pivot_wider(values_from="predicted_frequency", names_from="SBT", id_cols="AD8", names_prefix="SBT") %>% 
  arrange(AD8) %>% 
  (function(x) {x[,order(colnames(x) %>% sub(pattern="[A-Z]+",replacement="" ) %>% as.integer )]}) %>%
  relocate(AD8) %>% 
  mutate(across(starts_with("SBT"), . %>% round(3) ))  %>% 
  write_csv("/research/outputs/sup_nonlinear_imputed_rates_epic.csv") 


merged_data2%>% mutate( AD8 = pmin(AD8, 3) %>% as.integer, SBT = pmin(SBT, 11)%>% as.integer, not_home = dc_home) %>%
  mutate(AD8 = if_else(is.finite(AD8), AD8, NA_integer_ ) , SBT = if_else(is.finite(SBT), SBT, NA_integer_ ) ) %>%
  group_by(AD8, SBT) %>% summarize(not_home=sum(not_home), n1=n() ) %>% ungroup -> events_rates

ddl_fit[[1]] %>% as_tibble %>% 
  mutate(ADB=as.integer(AD8), SBT=as.integer(SBT)) %>% 
  left_join( events_rates, by=c("AD8", "SBT") ) %>%
  mutate( not_home = predicted_frequency*n1) %>%
  thresh_rate_f %>% 
  pivot_wider(values_from="thresh_rate", names_from="SBT", id_cols="AD8", names_prefix="SBT") %>% 
  arrange(AD8) %>% 
  (function(x) {x[,order(colnames(x) %>% sub(pattern="[A-Z]+",replacement="" ) %>% as.integer )]}) %>%
  relocate(AD8)  %>%  
  write_csv("/research/outputs/sup_nonlinear_marginal_effect_thresholds_epic.csv") 
  
## unadjusted rates
events_rates %>% mutate(not_home= not_home/n1) %>% 
  filter(n1>10) %>% 
  pivot_wider(values_from="not_home", names_from="SBT", id_cols="AD8", names_prefix="SBT")  %>% 
  write_csv("/research/outputs/sup_nonlinear_raw_epic.csv")  

 ## unadjusted thresholds
events_rates %>% select(AD8, SBT, n1, not_home) %>% 
thresh_rate_f %>% 
filter(is.finite(AD8) & is.finite(SBT)) %>% 
pivot_wider(values_from="thresh_rate", names_from="SBT", id_cols="AD8", names_prefix="SBT") %>% 
write_csv("/research/outputs/sup_nonlinear_marginal_effect_thresholds_raw_epic.csv") 
 
events_rates %>% 
  pivot_wider(values_from="n1", names_from="SBT", id_cols="AD8", names_prefix="SBT") %>%
  write_csv("/research/outputs/raw_n_epic.csv")  

  
boot_result <- boot(data=train_set, statistic=test_stat, R=5000, labels_in=label_in)

boot.ci(boot_result, type="basic")
boot_result %>% saveRDS(file="/research/outputs/ep_ddml_boot.RDS")


c(boot_result$t0, boot_result$t %>% colMeans, boot.ci(boot_result, type="basic")$basic[,c(4,5)]  ) %>% matrix(nrow=1) %>% set_colnames(c("whole_sample_difference","mean boot difference","lower_ci_boot", "upper_ci_boot")) %>% round(3) %>% as.data.frame %>% write_csv("/research/outputs/ep_ddml_boot.csv")


base_form <- "thisout ~ 0" %>% formula
surg_form <- paste0(surg_vars, collapse=" + ")
year_form <- paste0(year_vars, collapse = " + ")
global_age_spline <- splines::bs(merged_data2$age, 8)
comorbid_form <- paste0(c(comborbid_vars) ,  collapse=" + ")

myform <- base_form %>% 
  update( paste0("~.+", year_form) ) %>%
  update( paste0("~.+", surg_form) ) %>% 
  update( paste0("~.+", comorbid_form) ) %>%
  update( "~.+predict(global_age_spline,age)" ) 

cogform <- myform %>%  update( "~.+AbnCog" ) 

boot_result_simple <- boot(data=merged_data2 %>% mutate(thisout=dc_home  ) %>% mutate(AbnCog= as.numeric(AbnCog)),  statistic=test_stat_acc, R=5000)


boot_result_simple %>% saveRDS(file="/research/outputs/ep_binary_acc_boot.RDS")

accuracy_summary <- cbind(boot_result_simple$t0[c(1,2,3,14:18)],
boot_result_simple$t[,c(1,2,3,14:18)] %>% colMeans,
rbind(
boot.ci(boot_result_simple, index=1, type="basic")$basic[,c(4,5)] ,
boot.ci(boot_result_simple, index=2, type="basic")$basic[,c(4,5)] ,
boot.ci(boot_result_simple, index=3, type="basic")$basic[,c(4,5)] ,
boot.ci(boot_result_simple, index=14, type="basic")$basic[,c(4,5)] ,
boot.ci(boot_result_simple, index=15, type="basic")$basic[,c(4,5)] ,
boot.ci(boot_result_simple, index=16, type="basic")$basic[,c(4,5)] ,
boot.ci(boot_result_simple, index=17, type="basic")$basic[,c(4,5)] ,
boot.ci(boot_result_simple, index=18, type="basic")$basic[,c(4,5)] )
) %>% set_colnames(c("whole_sample_difference","mean out-of-bag difference","lower_ci_oob", "upper_ci_oob")) %>% round(3) %>% as.data.frame %>% set_rownames( c("auc",'sensitivity',  'specificity', 'ppv', 'npv', 'precision'))

accuracy_summary %>% write_csv("/research/outputs/ep_bin_acc_summary.csv")

## sandwich SE over surgeries


temp_data <- merged_data2 %>% mutate(thisout=dc_home ) %>% mutate(AbnCog= as.numeric(AbnCog)) %>% as.data.frame
glm(data=temp_data, formula=cogform , family=binomial() ) -> ci_glm_ep

surgery_v <- vcovCL(ci_glm_ep, cluster = ~surgType , cadjust = FALSE, type="HC1")
thistest <- coeftest(ci_glm_ep, surgery_v)
cbind(thistest["AbnCog",, drop=FALSE] , confint(thistest,parm="AbnCog") ) %>% as.data.frame %>% write_csv("/research/outputs/ep_surg_sandwich.csv")






















