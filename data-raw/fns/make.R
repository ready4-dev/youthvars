make_adol_aqol6d_disv_lup <- function ()
{
  utils::data("aqol6d_adult_disv_lup_tb", envir = environment())
  adol_aqol6d_disv_lup <- aqol6d_adult_disv_lup_tb %>%
    dplyr::mutate(Answer_4_dbl = dplyr::case_when(Question_chr == "Q18" ~ 0.622,
                                                  TRUE ~ Answer_4_dbl),
                  Answer_5_dbl = dplyr::case_when(Question_chr == "Q3" ~ 0.827,
                                                  TRUE ~ Answer_5_dbl),
                  Answer_6_dbl = dplyr::case_when(Question_chr == "Q1" ~ 0.073,
                                                  TRUE ~ Answer_5_dbl))
  return(adol_aqol6d_disv_lup)
}
