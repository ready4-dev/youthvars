make_adol_aqol6d_disv_lup <- function ()
{
  utils::data("aqol6d_adult_disv_lup_tb", package = "youthvars", envir = environment())
  adol_aqol6d_disv_lup <- aqol6d_adult_disv_lup_tb %>%
    dplyr::mutate(Answer_4_dbl = dplyr::case_when(Question_chr == "Q18" ~ 0.622,
                                                  TRUE ~ Answer_4_dbl),
                  Answer_5_dbl = dplyr::case_when(Question_chr == "Q3" ~ 0.827,
                                                  TRUE ~ Answer_5_dbl),
                  Answer_6_dbl = dplyr::case_when(Question_chr == "Q1" ~ 0.073,
                                                  TRUE ~ Answer_5_dbl))
  return(adol_aqol6d_disv_lup)
}
make_tfd_repln_ds_dict_r3 <- function(repln_ds_dict_r3 = NULL){
  if(is.null(repln_ds_dict_r3)){
    data("repln_ds_dict_r3", package =  "youthvars", envir = environment())
  }
  tfd_repln_ds_dict_r3 <- repln_ds_dict_r3 %>%
    dplyr::mutate(dplyr::across(.fns = as.character)) %>%
    dplyr::mutate(var_nm_chr = dplyr::case_when(var_nm_chr == "phq9_total" ~ "PHQ9",
                                                var_nm_chr == "bads_total" ~ "BADS",
                                                var_nm_chr == "gad7_total" ~ "GAD7",
                                                var_nm_chr == "oasis_total" ~ "OASIS",
                                                var_nm_chr == "scared_total" ~ "SCARED",
                                                var_nm_chr == "k6_total" ~ "K6",
                                                var_nm_chr == "c_sofas" ~ "SOFAS",
                                                T ~ var_nm_chr))
  Hmisc::label(tfd_repln_ds_dict_r3) = as.list(c("Variable","Category", "Description", "Class"))
  return(tfd_repln_ds_dict_r3)
}
