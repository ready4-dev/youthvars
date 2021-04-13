#' Transform dataset for item
#' @description transform_ds_for_item_plt() is a Transform function that edits an object in such a way that core object attributes - e.g. shape, dimensions, elements, type - are altered. Specifically, this function implements an algorithm to transform dataset for item plot. Function argument data_tb specifies the object to be updated. Argument var_nm_1L_chr provides the object to be updated. The function returns Transformed data (a tibble).
#' @param data_tb Data (a tibble)
#' @param var_nm_1L_chr Variable name (a character vector of length one)
#' @param round_var_nm_1L_chr Round variable name (a character vector of length one), Default: 'round'
#' @return Transformed data (a tibble)
#' @rdname transform_ds_for_item_plt
#' @export 
#' @importFrom dplyr filter group_by summarise n mutate
#' @importFrom rlang sym
#' @keywords internal
transform_ds_for_item_plt <- function (data_tb, var_nm_1L_chr, round_var_nm_1L_chr = "round") 
{
    tfd_data_tb <- data_tb %>% dplyr::filter(!is.na(!!as.name(var_nm_1L_chr))) %>% 
        dplyr::group_by(!!rlang::sym(round_var_nm_1L_chr), !!as.name(var_nm_1L_chr)) %>% 
        dplyr::summarise(n = dplyr::n()) %>% dplyr::group_by(!!rlang::sym(round_var_nm_1L_chr)) %>% 
        dplyr::mutate(y = n/sum(n))
    return(tfd_data_tb)
}
#' Transform raw dataset for analysis
#' @description transform_raw_ds_for_analysis() is a Transform function that edits an object in such a way that core object attributes - e.g. shape, dimensions, elements, type - are altered. Specifically, this function implements an algorithm to transform raw dataset for analysis. Function argument raw_ds_tb specifies the object to be updated. The function returns Transformed dataset (a tibble).
#' @param raw_ds_tb Raw dataset (a tibble)
#' @return Transformed dataset (a tibble)
#' @rdname transform_raw_ds_for_analysis
#' @export 
#' @importFrom dplyr mutate case_when select contains rename
#' @importFrom purrr map_lgl
#' @keywords internal
transform_raw_ds_for_analysis <- function (raw_ds_tb) 
{
    transformed_ds_tb <- raw_ds_tb %>% dplyr::mutate(d_agegroup = cut(d_age, 
        breaks = c(11, 17, 30), labels = c("Age 12-17", "Age 18-26"))) %>% 
        dplyr::mutate(round = factor(round, labels = c("Baseline", 
            "Follow-up"))) %>% dplyr::mutate(Gender = factor(ifelse(d_gender == 
        "Genderqueer/gender nonconforming/agender" | d_gender == 
        "Transgender", "Other", as.character(d_gender)))) %>% 
        dplyr::mutate(Region = as.factor(ifelse(s_centre == "Canberra" | 
            s_centre == "Southport" | s_centre == "Knox", "Metro", 
            "Regional"))) %>% dplyr::mutate(CALD = factor(ifelse(d_country_bir_s == 
        "Other" | d_english_home == "No" | d_english_native == 
        "No", "Yes", "No"))) %>% dplyr::mutate(d_sexual_ori_s = dplyr::case_when(d_sexual_ori_s == 
        "Straight" ~ "Heterosexual", T ~ as.character(d_sexual_ori_s)) %>% 
        factor()) %>% dplyr::select(fkClientID, round, d_interview_date, 
        d_age, d_agegroup, Gender, d_gender, d_sex_birth_s, d_sexual_ori_s, 
        d_ATSI, CALD, d_country_bir_s, d_english_home, d_english_native, 
        Region, d_studying_working, d_relation_s, s_centre, c_p_diag_s, 
        c_clinical_staging_s, k6_total, phq9_total, bads_total, 
        gad7_total, oasis_total, scared_total, c_sofas, dplyr::contains("aqol6d")) %>% 
        dplyr::rename(PHQ9 = phq9_total, BADS = bads_total, GAD7 = gad7_total, 
            OASIS = oasis_total, SCARED = scared_total, K6 = k6_total, 
            SOFAS = c_sofas)
    transformed_ds_tb <- transformed_ds_tb %>% dplyr::select(names(transformed_ds_tb)[!(names(transformed_ds_tb) %>% 
        purrr::map_lgl(~startsWith(.x, "aqol6d_sub") | startsWith(.x, 
            "aqol6d_tot") | startsWith(.x, "aqol6d_fla")))])
    return(transformed_ds_tb)
}
