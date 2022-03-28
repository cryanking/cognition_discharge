vats_codes <- c(
"0BTC4ZZ" , 
"0BTD4ZZ" , 
"0BTF4ZZ" , 
"0BTG4ZZ" , 
"0BTH4ZZ" , 
"0BTJ4ZZ" , 
"0BTK4ZZ" , 
"0BTL4ZZ" ,
"32.20" ,
"32.24" ,
"32.25" ,
"32.41" )

gut_codes <- grep(allcodes, "^0D[BTVD5][89ABEFGHKLMNPQ]" )
stomach_codes <- grep(allcodes, "^0D[1BVTD5]6" )
chole_codes <- "^0FD4"
panc_codes <- "^0F[BT]G"
hyster_codes <- "^0U[TD][4579]"
lumbar_codes <- "^0SG[01]"
shoulder_codes <- "^0RR[JK]"
hiatalHernia_codes <- "^0BU[RST]4" , "^0BQ[RST]"
knee_codes <-  "^0S[UR][CD]"
totalHip_codes <- "^0SR[9B]"
neph_codes <- "^0TT[012]"
prost_codes <- "^0VT0"
bladder_codes <- "^0TTB"
ueavfist_codes <- "^031[345678569ABC]0[A-Z0-9][DF]"

included_proc_codes <- c(included_proc_codes, vats_codes)
