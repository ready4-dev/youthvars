transform_raw_ds_for_analysis <- function (raw_ds_tb) # Previously transform_raw_aqol_tb_to_aqol6d_tb
{
  transformed_ds_tb <- raw_ds_tb %>% dplyr::mutate(d_agegroup = cut(d_age,
                                                              breaks = c(11, 17, 30), labels = c("Age 12-17", "Age 18-26"))) %>%
    dplyr::mutate(round = factor(round, labels = c("Baseline",
                                                   "Follow-up"))) %>%
    dplyr::mutate(Gender = factor(ifelse(d_gender == "Genderqueer/gender nonconforming/agender" |
                                           d_gender == "Transgender", "Other", as.character(d_gender)))) %>%
    dplyr::mutate(Region = as.factor(ifelse(s_centre == "Canberra" |
                                              s_centre == "Southport" | s_centre == "Knox", "Metro",
                                            "Regional"))) %>%
    dplyr::mutate(CALD = factor(ifelse(d_country_bir_s == "Other" | d_english_home == "No" | d_english_native == "No",
                                       "Yes",
                                       "No"
                                       ))) %>%
    dplyr::mutate(d_sexual_ori_s = dplyr::case_when(d_sexual_ori_s == "Straight" ~ "Heterosexual",
                                                    T ~ as.character(d_sexual_ori_s)) %>% factor()) %>%
    dplyr::select(fkClientID, round, d_interview_date,
                  d_age, d_agegroup, Gender, d_gender,
                  d_sex_birth_s, d_sexual_ori_s,
                  d_ATSI,
                  CALD, d_country_bir_s, d_english_home, d_english_native,
                  Region,
                  d_studying_working,
                  d_relation_s,
                  s_centre,
                  c_p_diag_s, c_clinical_staging_s,
                  k6_total, phq9_total, bads_total, gad7_total, oasis_total, scared_total,
                  c_sofas,
                  dplyr::contains("aqol6d")) %>%
    dplyr::rename(PHQ9 = phq9_total,
                  BADS = bads_total,
                  GAD7 = gad7_total,
                  OASIS = oasis_total,
                  SCARED = scared_total,
                  K6 = k6_total,
                  SOFAS = c_sofas)
  transformed_ds_tb <- transformed_ds_tb %>%
    dplyr::select(names(transformed_ds_tb)[!(names(transformed_ds_tb) %>% purrr::map_lgl(~ startsWith(.x,"aqol6d_sub") | startsWith(.x,"aqol6d_tot") | startsWith(.x,"aqol6d_fla")))])
  # transformed_ds_tb <- transformed_ds_tb %>%
  #   dplyr::select(c(c("fkClientID","round"),names(transformed_ds_tb)[!startsWith(names(transformed_ds_tb),"aqol6d_q") & !(names(transformed_ds_tb) %in% c("fkClientID","round"))],names(transformed_ds_tb)[startsWith(names(transformed_ds_tb),"aqol6d_q")]))
  return(transformed_ds_tb)
}