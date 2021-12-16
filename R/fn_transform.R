#' Transform dataset for item plot
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
#' Transform dataset for testing
#' @description transform_ds_for_tstng() is a Transform function that edits an object in such a way that core object attributes - e.g. shape, dimensions, elements, type - are altered. Specifically, this function implements an algorithm to transform dataset for testing. Function argument data_tb specifies the object to be updated. Argument depnt_var_nm_1L_chr provides the object to be updated. The function returns Transformed data (a tibble).
#' @param data_tb Data (a tibble)
#' @param depnt_var_nm_1L_chr Dependent variable name (a character vector of length one), Default: 'aqol6d_total_w'
#' @param depnt_var_max_val_1L_dbl Dependent variable maximum value (a double vector of length one), Default: 0.999
#' @param candidate_predrs_chr Candidate predictors (a character vector), Default: 'NA'
#' @param covar_var_nms_chr Covariate variable names (a character vector), Default: 'NA'
#' @param round_var_nm_1L_chr Round variable name (a character vector of length one), Default: 'round'
#' @param round_val_1L_chr Round value (a character vector of length one), Default: 'Baseline'
#' @param remove_all_msng_1L_lgl Remove all missing (a logical vector of length one), Default: F
#' @return Transformed data (a tibble)
#' @rdname transform_ds_for_tstng
#' @export 
#' @importFrom purrr discard
#' @importFrom dplyr filter select mutate
#' @importFrom rlang sym syms
#' @importFrom stats na.omit
transform_ds_for_tstng <- function (data_tb, depnt_var_nm_1L_chr = "aqol6d_total_w", depnt_var_max_val_1L_dbl = 0.999, 
    candidate_predrs_chr = NA_character_, covar_var_nms_chr = NA_character_, 
    round_var_nm_1L_chr = "round", round_val_1L_chr = "Baseline", 
    remove_all_msng_1L_lgl = F) 
{
    vars_to_keep_chr <- c(depnt_var_nm_1L_chr, candidate_predrs_chr, 
        covar_var_nms_chr) %>% purrr::discard(is.na)
    tfd_data_tb <- data_tb
    if (!is.na(round_var_nm_1L_chr) & !is.na(round_val_1L_chr)) 
        tfd_data_tb <- tfd_data_tb %>% dplyr::filter(!!rlang::sym(round_var_nm_1L_chr) == 
            round_val_1L_chr)
    tfd_data_tb <- tfd_data_tb %>% dplyr::select(!!!rlang::syms(vars_to_keep_chr)) %>% 
        dplyr::mutate(`:=`(!!rlang::sym(depnt_var_nm_1L_chr), 
            ifelse(!!rlang::sym(depnt_var_nm_1L_chr) > depnt_var_max_val_1L_dbl, 
                depnt_var_max_val_1L_dbl, !!rlang::sym(depnt_var_nm_1L_chr))))
    if (remove_all_msng_1L_lgl) 
        tfd_data_tb <- tfd_data_tb %>% stats::na.omit()
    return(tfd_data_tb)
}
#' Transform dataset with rename lookup table
#' @description transform_ds_with_rename_lup() is a Transform function that edits an object in such a way that core object attributes - e.g. shape, dimensions, elements, type - are altered. Specifically, this function implements an algorithm to transform dataset with rename lookup table. Function argument ds_tb specifies the object to be updated. Argument rename_lup provides the object to be updated. The function returns Tfmd dataset (a tibble).
#' @param ds_tb Dataset (a tibble)
#' @param rename_lup Rename (a lookup table)
#' @param target_var_nms_chr Target variable names (a character vector), Default: NULL
#' @return Tfmd dataset (a tibble)
#' @rdname transform_ds_with_rename_lup
#' @export 
#' @importFrom dplyr rename_with
#' @importFrom ready4 get_from_lup_obj
#' @keywords internal
transform_ds_with_rename_lup <- function (ds_tb, rename_lup, target_var_nms_chr = NULL) 
{
    if (is.null(target_var_nms_chr)) 
        target_var_nms_chr <- intersect(names(ds_tb), rename_lup$old_nms_chr)
    tfmd_ds_tb <- dplyr::rename_with(ds_tb, .cols = target_var_nms_chr, 
        ~ready4::get_from_lup_obj(rename_lup, match_value_xx = .x, 
            match_var_nm_1L_chr = "old_nms_chr", target_var_nm_1L_chr = "new_nms_chr", 
            evaluate_1L_lgl = F))
    return(tfmd_ds_tb)
}
#' Transform raw dataset for analysis
#' @description transform_raw_ds_for_analysis() is a Transform function that edits an object in such a way that core object attributes - e.g. shape, dimensions, elements, type - are altered. Specifically, this function implements an algorithm to transform raw dataset for analysis. Function argument raw_ds_tb specifies the object to be updated. The function returns Transformed dataset (a tibble).
#' @param raw_ds_tb Raw dataset (a tibble)
#' @return Transformed dataset (a tibble)
#' @rdname transform_raw_ds_for_analysis
#' @export 
#' @importFrom dplyr mutate case_when select contains rename
#' @importFrom purrr map_lgl
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
#' Transform tibble for merged column 1
#' @description transform_tb_for_merged_col_1() is a Transform function that edits an object in such a way that core object attributes - e.g. shape, dimensions, elements, type - are altered. Specifically, this function implements an algorithm to transform tibble for merged column 1. Function argument df specifies the object to be updated. Argument output_type_1L_chr provides the object to be updated. The function returns Data.frame (a data.frame).
#' @param df Data.frame (a data.frame)
#' @param output_type_1L_chr Output type (a character vector of length one), Default: 'PDF'
#' @return Data.frame (a data.frame)
#' @rdname transform_tb_for_merged_col_1
#' @export 
#' @keywords internal
transform_tb_for_merged_col_1 <- function (df, output_type_1L_chr = "PDF") 
{
    df[[1]] <- as.character(df[[1]])
    rle.lengths <- rle(df[[1]])$lengths
    first <- !duplicated(df[[1]])
    df[[1]][!first] <- ""
    if (output_type_1L_chr == "PDF") 
        df[[1]][first] <- paste0("\\midrule\\multirow{", rle.lengths, 
            "}{*}{\\textbf{", df[[1]][first], "}}")
    return(df)
}
