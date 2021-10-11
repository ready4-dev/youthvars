#' Calculate adolescent Assessment of Quality of Life Six Dimension Health Utility
#' @description calculate_adol_aqol6dU() is a Calculate function that performs a numeric calculation. Specifically, this function implements an algorithm to calculate adolescent assessment of quality of life six dimension health utility. The function returns Adolescent Assessment of Quality of Life Six Dimension (a double vector).
#' @param unscored_aqol_tb Unscored Assessment of Quality of Life (a tibble)
#' @param prefix_1L_chr Prefix (a character vector of length one), Default: 'aqol6d_q'
#' @param id_var_nm_1L_chr Identity variable name (a character vector of length one), Default: 'fkClientID'
#' @param wtd_aqol_var_nm_1L_chr Weighted Assessment of Quality of Life variable name (a character vector of length one), Default: 'aqol6d_total_w'
#' @return Adolescent Assessment of Quality of Life Six Dimension (a double vector)
#' @rdname calculate_adol_aqol6dU
#' @export 
#' @importFrom dplyr pull
#' @importFrom rlang sym
#' @keywords internal
calculate_adol_aqol6dU <- function (unscored_aqol_tb, prefix_1L_chr = "aqol6d_q", id_var_nm_1L_chr = "fkClientID", 
    wtd_aqol_var_nm_1L_chr = "aqol6d_total_w") 
{
    scored_aqol_tb <- add_adol6d_scores(unscored_aqol_tb, prefix_1L_chr = prefix_1L_chr, 
        id_var_nm_1L_chr = id_var_nm_1L_chr, wtd_aqol_var_nm_1L_chr = wtd_aqol_var_nm_1L_chr)
    adol_aqol6d_dbl <- scored_aqol_tb %>% dplyr::pull(!!rlang::sym(wtd_aqol_var_nm_1L_chr))
    return(adol_aqol6d_dbl)
}
#' Calculate adult Assessment of Quality of Life Six Dimension Health Utility
#' @description calculate_adult_aqol6dU() is a Calculate function that performs a numeric calculation. Specifically, this function implements an algorithm to calculate adult assessment of quality of life six dimension health utility. The function returns Assessment of Quality of Life Six Dimension Health Utility (a double vector).
#' @param aqol6d_items_tb Assessment of Quality of Life Six Dimension items (a tibble)
#' @param prefix_1L_chr Prefix (a character vector of length one)
#' @param coefs_lup_tb Coefficients lookup table (a tibble), Default: NULL
#' @param dim_sclg_con_lup_tb Dimension scaling constant lookup table (a tibble), Default: NULL
#' @param disvalues_lup_tb Disvalues lookup table (a tibble), Default: NULL
#' @param itm_wrst_wts_lup_tb Item worst weights lookup table (a tibble), Default: NULL
#' @return Assessment of Quality of Life Six Dimension Health Utility (a double vector)
#' @rdname calculate_adult_aqol6dU
#' @export 
#' @importFrom utils data
#' @importFrom hutils longest_prefix
#' @keywords internal
calculate_adult_aqol6dU <- function (aqol6d_items_tb, prefix_1L_chr, coefs_lup_tb = NULL, 
    dim_sclg_con_lup_tb = NULL, disvalues_lup_tb = NULL, itm_wrst_wts_lup_tb = NULL) 
{
    if (is.null(coefs_lup_tb)) {
        utils::data("aqol6d_from_8d_coefs_lup_tb", envir = environment())
        coefs_lup_tb <- aqol6d_from_8d_coefs_lup_tb
    }
    if (is.null(dim_sclg_con_lup_tb)) {
        utils::data("aqol6d_dim_sclg_con_lup_tb", envir = environment())
        dim_sclg_con_lup_tb <- aqol6d_dim_sclg_con_lup_tb
    }
    if (is.null(disvalues_lup_tb)) {
        utils::data("aqol6d_adult_disv_lup_tb", envir = environment())
        disvalues_lup_tb <- aqol6d_adult_disv_lup_tb
    }
    if (is.null(itm_wrst_wts_lup_tb)) {
        utils::data("aqol6d_adult_itm_wrst_wts_lup_tb", envir = environment())
        itm_wrst_wts_lup_tb <- aqol6d_adult_itm_wrst_wts_lup_tb
    }
    domains_chr <- dim_sclg_con_lup_tb$Dimension_chr
    item_pfx_1L_chr <- hutils::longest_prefix(disvalues_lup_tb$Question_chr)
    domain_items_ls <- make_domain_items_ls(domain_qs_lup_tb = aqol6d_domain_qs_lup_tb, 
        item_pfx_1L_chr = item_pfx_1L_chr)
    aqol6d_items_tb <- aqol6d_items_tb %>% make_aqol6d_items_tb(old_pfx_1L_chr = prefix_1L_chr, 
        new_pfx_1L_chr = item_pfx_1L_chr) %>% impute_adult_aqol6d_items_tb(domain_items_ls = domain_items_ls) %>% 
        add_itm_disv_to_aqol6d_itms_tb(disvalues_lup_tb = disvalues_lup_tb, 
            pfx_1L_chr = item_pfx_1L_chr) %>% add_dim_disv_to_aqol6d_items_tb(domain_items_ls = domain_items_ls, 
        domains_chr = domains_chr, dim_sclg_con_lup_tb = dim_sclg_con_lup_tb, 
        itm_wrst_wts_lup_tb = itm_wrst_wts_lup_tb) %>% add_dim_scores_to_aqol6d_items_tb(domain_items_ls = domain_items_ls) %>% 
        add_aqol6dU_to_aqol6d_items_tb(coefs_lup_tb = coefs_lup_tb)
    aqol6dU_dbl <- aqol6d_items_tb$aqol6dU
    return(aqol6dU_dbl)
}
#' Calculate Assessment of Quality of Life Six Dimension dimension 1 disvalue
#' @description calculate_aqol6d_dim_1_disv() is a Calculate function that performs a numeric calculation. Specifically, this function implements an algorithm to calculate assessment of quality of life six dimension dimension 1 disvalue. The function returns DvD1 (a double vector).
#' @param dvQs_tb Questionnaire dimension items (a tibble)
#' @param kD_1L_dbl Dimension scaling constant (a double vector of length one)
#' @param w_dbl W (a double vector)
#' @return DvD1 (a double vector)
#' @rdname calculate_aqol6d_dim_1_disv
#' @export 
#' @importFrom purrr pmap_dbl
#' @keywords internal
calculate_aqol6d_dim_1_disv <- function (dvQs_tb, kD_1L_dbl, w_dbl) 
{
    dvD1_dbl <- purrr::pmap_dbl(dvQs_tb, ~{
        (1/kD_1L_dbl) * ((1 + (kD_1L_dbl * w_dbl[1] * ..1)) * 
            (1 + (kD_1L_dbl * w_dbl[2] * ..2)) * (1 + (kD_1L_dbl * 
            w_dbl[3] * ..3)) * (1 + (kD_1L_dbl * w_dbl[4] * ..4)) - 
            1)
    })
    return(dvD1_dbl)
}
#' Calculate Assessment of Quality of Life Six Dimension dimension 2 disvalue
#' @description calculate_aqol6d_dim_2_disv() is a Calculate function that performs a numeric calculation. Specifically, this function implements an algorithm to calculate assessment of quality of life six dimension dimension 2 disvalue. The function returns DvD2 (a double vector).
#' @param dvQs_tb Questionnaire dimension items (a tibble)
#' @param kD_1L_dbl Dimension scaling constant (a double vector of length one)
#' @param w_dbl W (a double vector)
#' @return DvD2 (a double vector)
#' @rdname calculate_aqol6d_dim_2_disv
#' @export 
#' @importFrom purrr pmap_dbl
#' @keywords internal
calculate_aqol6d_dim_2_disv <- function (dvQs_tb, kD_1L_dbl, w_dbl) 
{
    dvD2_dbl <- purrr::pmap_dbl(dvQs_tb, ~{
        (1/kD_1L_dbl) * ((1 + (kD_1L_dbl * w_dbl[1] * ..1)) * 
            (1 + (kD_1L_dbl * w_dbl[2] * ..2)) * (1 + (kD_1L_dbl * 
            w_dbl[3] * ..3)) - 1)
    })
    return(dvD2_dbl)
}
#' Calculate Assessment of Quality of Life Six Dimension dimension 3 disvalue
#' @description calculate_aqol6d_dim_3_disv() is a Calculate function that performs a numeric calculation. Specifically, this function implements an algorithm to calculate assessment of quality of life six dimension dimension 3 disvalue. The function returns DvD3 (a double vector).
#' @param dvQs_tb Questionnaire dimension items (a tibble)
#' @param kD_1L_dbl Dimension scaling constant (a double vector of length one)
#' @param w_dbl W (a double vector)
#' @return DvD3 (a double vector)
#' @rdname calculate_aqol6d_dim_3_disv
#' @export 
#' @importFrom purrr pmap_dbl
#' @keywords internal
calculate_aqol6d_dim_3_disv <- function (dvQs_tb, kD_1L_dbl, w_dbl) 
{
    dvD3_dbl <- purrr::pmap_dbl(dvQs_tb, ~{
        (1/kD_1L_dbl) * ((1 + (kD_1L_dbl * w_dbl[1] * ..1)) * 
            (1 + (kD_1L_dbl * w_dbl[2] * ..2)) * (1 + (kD_1L_dbl * 
            w_dbl[3] * ..3)) * (1 + (kD_1L_dbl * w_dbl[4] * 1 * 
            ..4)) - 1)
    })
    return(dvD3_dbl)
}
#' Calculate Assessment of Quality of Life Six Dimension dimension 4 disvalue
#' @description calculate_aqol6d_dim_4_disv() is a Calculate function that performs a numeric calculation. Specifically, this function implements an algorithm to calculate assessment of quality of life six dimension dimension 4 disvalue. The function returns DvD4 (a double vector).
#' @param dvQs_tb Questionnaire dimension items (a tibble)
#' @param kD_1L_dbl Dimension scaling constant (a double vector of length one)
#' @param w_dbl W (a double vector)
#' @return DvD4 (a double vector)
#' @rdname calculate_aqol6d_dim_4_disv
#' @export 
#' @importFrom purrr pmap_dbl
#' @keywords internal
calculate_aqol6d_dim_4_disv <- function (dvQs_tb, kD_1L_dbl, w_dbl) 
{
    dvD4_dbl <- purrr::pmap_dbl(dvQs_tb, ~{
        (1/kD_1L_dbl) * ((1 + (kD_1L_dbl * w_dbl[1] * ..1)) * 
            (1 + (kD_1L_dbl * w_dbl[2] * ..2)) * (1 + (kD_1L_dbl * 
            w_dbl[3] * ..3)) - 1)
    })
    return(dvD4_dbl)
}
#' Calculate Assessment of Quality of Life Six Dimension dimension 5 disvalue
#' @description calculate_aqol6d_dim_5_disv() is a Calculate function that performs a numeric calculation. Specifically, this function implements an algorithm to calculate assessment of quality of life six dimension dimension 5 disvalue. The function returns DvD5 (a double vector).
#' @param dvQs_tb Questionnaire dimension items (a tibble)
#' @param kD_1L_dbl Dimension scaling constant (a double vector of length one)
#' @param w_dbl W (a double vector)
#' @return DvD5 (a double vector)
#' @rdname calculate_aqol6d_dim_5_disv
#' @export 
#' @importFrom purrr pmap_dbl
#' @keywords internal
calculate_aqol6d_dim_5_disv <- function (dvQs_tb, kD_1L_dbl, w_dbl) 
{
    dvD5_dbl <- purrr::pmap_dbl(dvQs_tb, ~{
        (1/kD_1L_dbl) * ((1 + (kD_1L_dbl * w_dbl[1] * ..1)) * 
            (1 + (kD_1L_dbl * w_dbl[2] * ..2)) * (1 + (kD_1L_dbl * 
            w_dbl[3] * ..3)) - 1)
    })
    return(dvD5_dbl)
}
#' Calculate Assessment of Quality of Life Six Dimension dimension 6 disvalue
#' @description calculate_aqol6d_dim_6_disv() is a Calculate function that performs a numeric calculation. Specifically, this function implements an algorithm to calculate assessment of quality of life six dimension dimension 6 disvalue. The function returns DvD6 (a double vector).
#' @param dvQs_tb Questionnaire dimension items (a tibble)
#' @param kD_1L_dbl Dimension scaling constant (a double vector of length one)
#' @param w_dbl W (a double vector)
#' @return DvD6 (a double vector)
#' @rdname calculate_aqol6d_dim_6_disv
#' @export 
#' @importFrom purrr pmap_dbl
#' @keywords internal
calculate_aqol6d_dim_6_disv <- function (dvQs_tb, kD_1L_dbl, w_dbl) 
{
    dvD6_dbl <- purrr::pmap_dbl(dvQs_tb, ~{
        (1/kD_1L_dbl) * ((1 + (kD_1L_dbl * w_dbl[1] * ..1)) * 
            (1 + (kD_1L_dbl * w_dbl[2] * ..2)) * (1 + (kD_1L_dbl * 
            w_dbl[3] * ..3)) - 1)
    })
    return(dvD6_dbl)
}
