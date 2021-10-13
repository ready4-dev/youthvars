## ----warning=FALSE, message=FALSE---------------------------------------------
library(youthvars)

## -----------------------------------------------------------------------------
data("replication_popl_tb", package = "youthvars")

## ----results='hide', warning=F, message=FALSE---------------------------------
scored_data_tb <- add_adol6d_scores(replication_popl_tb,
                                              prefix_1L_chr =  "aqol6d_q",
                                              id_var_nm_1L_chr = "fkClientID",
                                              wtd_aqol_var_nm_1L_chr = "aqol6d_total_w") 

