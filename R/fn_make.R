#' Make adolescent Assessment of Quality of Life Six Dimension disvalue
#' @description make_adol_aqol6d_disv_lup() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make adolescent assessment of quality of life six dimension disvalue lookup table. The function returns Adolescent Assessment of Quality of Life Six Dimension disvalue (a lookup table).

#' @return Adolescent Assessment of Quality of Life Six Dimension disvalue (a lookup table)
#' @rdname make_adol_aqol6d_disv_lup
#' @export 
#' @importFrom utils data
#' @importFrom dplyr mutate case_when
#' @keywords internal
make_adol_aqol6d_disv_lup <- function () 
{
    utils::data("aqol6d_adult_disv_lup_tb", package = "youthvars", 
        envir = environment())
    adol_aqol6d_disv_lup <- aqol6d_adult_disv_lup_tb %>% dplyr::mutate(Answer_4_dbl = dplyr::case_when(Question_chr == 
        "Q18" ~ 0.622, TRUE ~ Answer_4_dbl), Answer_5_dbl = dplyr::case_when(Question_chr == 
        "Q3" ~ 0.827, TRUE ~ Answer_5_dbl), Answer_6_dbl = dplyr::case_when(Question_chr == 
        "Q1" ~ 0.073, TRUE ~ Answer_5_dbl))
    return(adol_aqol6d_disv_lup)
}
#' Make transformed replication dataset dictionary
#' @description make_tfd_repln_ds_dict_r3() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make transformed replication dataset dictionary ready4 s3. The function returns Transformed replication dataset dictionary (a ready4 S3).
#' @param repln_ds_dict_r3 Replication dataset dictionary (a ready4 S3), Default: NULL
#' @return Transformed replication dataset dictionary (a ready4 S3)
#' @rdname make_tfd_repln_ds_dict_r3
#' @export 
#' @importFrom dplyr mutate across case_when
#' @importFrom Hmisc label
make_tfd_repln_ds_dict_r3 <- function (repln_ds_dict_r3 = NULL) 
{
    if (is.null(repln_ds_dict_r3)) {
        data("repln_ds_dict_r3", package = "youthvars", envir = environment())
    }
    tfd_repln_ds_dict_r3 <- repln_ds_dict_r3 %>% dplyr::mutate(dplyr::across(.fns = as.character)) %>% 
        dplyr::mutate(var_nm_chr = dplyr::case_when(var_nm_chr == 
            "phq9_total" ~ "PHQ9", var_nm_chr == "bads_total" ~ 
            "BADS", var_nm_chr == "gad7_total" ~ "GAD7", var_nm_chr == 
            "oasis_total" ~ "OASIS", var_nm_chr == "scared_total" ~ 
            "SCARED", var_nm_chr == "k6_total" ~ "K6", var_nm_chr == 
            "c_sofas" ~ "SOFAS", T ~ var_nm_chr))
    Hmisc::label(tfd_repln_ds_dict_r3) = as.list(c("Variable", 
        "Category", "Description", "Class"))
    return(tfd_repln_ds_dict_r3)
}
