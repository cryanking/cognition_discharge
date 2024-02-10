## Format for bounds:
## 1: max_depth (int)
## 2: eta
## 3: nrounds (int) - because the model stores CV loss at each round, this is an upper limit only and is only used that way. 
## 4: log of min_child_weight
## 5: gamma
## 6: subsample fraction
## 7: log of lambda
## 8: num_parallel_tree (int)
xgboost_cv <- function(data
  , label=NULL
  , objective = "binary:logistic"
  , max_depth=NA_real_
  , eta=NA_real_
  , nrounds=NA_real_
  , min_child_weight=NA_real_
  , gamma=NA_real_
  , subsample=NA_real_
  , lambda=NA_real_
  , num_parallel_tree = NA_real_
  , lower_bounds = c(2L, .1,  5L, log(0.3), 0, .7  , log(.02) , 1L)
  , upper_bounds = c(6L, 1., 15L, log(200), 5, .95 , log(2000), 1L)
  , nthread=1L
  , sobol_size=15L
  , nfold=5L
  , tree_method="auto"
  , eval_metric="logloss"
  , other_params=NULL) {
  if(any( c("matrix", "dgRMatrix", "dgeMatrix", "dgTMatrix", "dgCMatrix") %in% class(data) )) {
    data <- xgb.DMatrix(data=data, info = list(label=label))
  } else if(!is.null(label)) {
    setinfo(data, "label" , label)
  }

  lower_corner <- dplyr:::coalesce(lower_bounds, c(2L, .1,  5L, log(0.3), 0, .7  , log(.02)  , 1L) ) 
  upper_corner <- dplyr:::coalesce(upper_bounds,  c(6L, 1., 15L, log(200), 5, .95 , log(2000), 1L) )
  
  evaluation_points <- randtoolbox:::sobol(n = sobol_size, dim = length(lower_corner), scrambling=FALSE)
  eval_res <- matrix(NA, nrow=sobol_size, ncol=2L)
  
  fixed_params <- c(max_depth, eta, nrounds, min_child_weight, gamma, subsample, lambda, num_parallel_tree )
  nrounds <- as.integer(ifelse(is.na(nrounds), upper_corner[3] , nrounds))
  
  for(sobol_index in seq.int(sobol_size)) {
    transformed_params <- lower_corner + (upper_corner-lower_corner)*evaluation_points[ sobol_index,, drop=TRUE]
    transformed_params <- dplyr:::coalesce(fixed_params,  transformed_params)
    transformed_params[c(1,3,8)] <- round(transformed_params[c(1,3,8)])
    
    local.model <- xgboost:::xgb.cv(data=data, nrounds=nrounds, params=c(other_params, list(
    max_depth=transformed_params[1] 
    , eta=transformed_params[2]
    ,  min_child_weight=exp(transformed_params[4])
    , gamma=transformed_params[5]
    , subsample=transformed_params[6] 
    , lambda=exp(transformed_params[7])
    , num_parallel_tree=transformed_params[8] 
    , objective = objective
    , tree_method=tree_method
    , eval_metric="logloss"
    ) ), verbose=0L, nfold= nfold, predictor="cpu_predictor")

    get_this <- grep(local.model %>% extract2("evaluation_log") %>% colnames, pattern="test_logloss_mean")[1]
    eval_res[sobol_index,1] <- local.model %>% extract2("evaluation_log") %>% extract2(get_this)  %>% which.min 
    eval_res[sobol_index,2] <- local.model %>% extract2("evaluation_log") %>% extract2(get_this)  %>% min
    rm(local.model)
    gc()
  }
  
  transformed_params <- lower_corner + (upper_corner-lower_corner)*evaluation_points[ which.min(eval_res[,2]), drop=TRUE]
  transformed_params <- dplyr:::coalesce(fixed_params,  transformed_params)
  transformed_params[c(1,3,8)] <- round(transformed_params[c(1,3,8)])
  transformed_params[3] <- nrounds <- eval_res[which.min(eval_res[,2]),1]


    local.model <- xgboost:::xgb.train(data=data, params=c(other_params, list(
    max_depth=transformed_params[1] 
    , eta=transformed_params[2]
    ,  min_child_weight=exp(transformed_params[4])
    , gamma=transformed_params[5]
    , subsample=transformed_params[6] 
    , lambda=exp(transformed_params[7])
    , num_parallel_tree=transformed_params[8] 
    , objective = objective
    , tree_method=tree_method
    , eval_metric=eval_metric
    ) ) , nrounds=nrounds, verbose=0L,  watchlist=list(train=data), predictor="cpu_predictor")  
    

  return(list(transformed_params, eval_res, local.model))

}
