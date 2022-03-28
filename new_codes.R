load(file="matched_proc_codes.rdata") 


all_codes <- unique(c(unique(actfast_proc_late$ICDX_PROCEDURE_CODE) , unique(Actfast_proc2$ICDX_PROCEDURE_CODE)  ) )

code_patterns <- list(
gut_codes = c("^0D[BTVD5][89ABEFGHKLMNPQ]" , "45\\.[6789]", "^46\\.9[34]", "^48\\.[456]" )
, stomach_codes = c("^0D[1BVTD5]6" , "^43\\.[56789]")
, chole_codes = c("^0F[DT]44" , "51\\.2[34]")
, panc_codes = c("^0F[BT]G", "^52\\.[256]", "52\\.7")
, hyster_codes = c("^0U[TD][4579]" , "^68\\.[345679]")
, lumbar_codes = c("^0SG[01]" ,"^81\\.0[678]" , "81\\.3[678]")
, shoulder_codes = c("^0RR[JK]" , "81\\.8[08]")
, hiatalHernia_codes = c("^0BU[RST][34]" , "53\\.71")
, knee_codes =  c("^0S[UR][CD]", "81\\.54")
, totalHip_codes = c("^0SR[9B]" ,"81\\.51")
, neph_codes = c( "^0TT[012]", "^55\\.[45]")
, prost_codes = c("^0VT0" ,"^60\\.[2346]")
, bladder_codes = c("^0TTB", "^57\\.[67]")
, ueavfist_codes = c("39\\.42" ,  "39\\.53" , "39\\.27" , "39\\.29" , "^031[345678569ABC]0[A-Z0-9][DF]")
, vats_codes = c("0BTC4ZZ" , "0BTD4ZZ" , "0BTF4ZZ" , "0BTG4ZZ" , "0BTH4ZZ" , "0BTJ4ZZ" , "0BTK4ZZ" , "0BTL4ZZ" , "32\\.20" , "32\\.24" , "32\\.25" , "32\\.41" )
)
observed_codes <- lapply(code_patterns, function(y) unique(unname(unlist(sapply(y, function(x) grep(pattern=x, all_codes, value=T)) )  ) ))

make_surg_categories <- function(ICD_PROCEDURE_CODE) { data.table::chmatch(ICD_PROCEDURE_CODE, unlist(observed_codes) ) %>% cut( breaks = c(0L,cumsum(sapply(observed_codes, length) ) )) %>% lvls_revalue( c(names(observed_codes) %>% sub(pattern="_codes", replacement="") ) ) }

## looking at the added codes
test_codes_old <- actfast_proc_late  %>% filter( ICDX_PROCEDURE_CODE %in% included_proc_codes) %>% select(REG_NO, ICDX_PROCEDURE_CODE) %>% distinct
test_codes_new <- actfast_proc_late  %>% filter( ICDX_PROCEDURE_CODE %in% unlist(observed_codes)) %>% select(REG_NO, ICDX_PROCEDURE_CODE,DESCRIPTION) %>% distinct
added_proc <- test_codes_new %>% select(ICDX_PROCEDURE_CODE,DESCRIPTION) %>% anti_join(test_codes_old,by="ICDX_PROCEDURE_CODE" ) %>% mutate(proc_type=make_surg_categories(ICDX_PROCEDURE_CODE)) %>% distinct %>% arrange(proc_type) 

test_codes_old <- Actfast_proc2  %>% filter( ICDX_PROCEDURE_CODE %in% included_proc_codes) %>% select(PAN, ICDX_PROCEDURE_CODE) %>% distinct
test_codes_new <- Actfast_proc2  %>% filter( ICDX_PROCEDURE_CODE %in% unlist(observed_codes)) %>% select(PAN, ICDX_PROCEDURE_CODE,DESCRIPTION) %>% distinct
added_proc <- test_codes_new %>% select(ICDX_PROCEDURE_CODE,DESCRIPTION) %>% anti_join(test_codes_old,by="ICDX_PROCEDURE_CODE" ) %>% mutate(proc_type=make_surg_categories(ICDX_PROCEDURE_CODE)) %>% distinct %>%bind_rows(added_proc) %>% distinct %>% arrange(proc_type) 

## how many patients are added?
test_codes_old <- actfast_proc_late  %>% filter( ICDX_PROCEDURE_CODE %in% included_proc_codes) %>% select(REG_NO, ICDX_PROCEDURE_CODE) %>% distinct
test_codes_new <- actfast_proc_late  %>% filter( ICDX_PROCEDURE_CODE %in% unlist(observed_codes)) %>% select(REG_NO, ICDX_PROCEDURE_CODE,DESCRIPTION) %>% distinct
added_pt <- test_codes_new %>% select(REG_NO) %>% distinct %>% anti_join(test_codes_old,by="REG_NO" ) 

test_codes_old <- Actfast_proc2  %>% filter( ICDX_PROCEDURE_CODE %in% included_proc_codes) %>% select(PAN) %>% distinct
test_codes_new <- Actfast_proc2  %>% filter( ICDX_PROCEDURE_CODE %in% unlist(observed_codes)) %>% select(PAN) %>% distinct
added_pt <- test_codes_new %>% distinct %>% anti_join(test_codes_old,by="PAN" ) %>% rename(REG_NO=PAN )  %>% bind_rows(added_pt) %>% distinct



