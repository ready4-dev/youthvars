#' Add Assessment of Quality of Life Six Dimension (Adolescent version) scores
#' @description add_adol6d_scores() is an Add function that updates an object by adding data to that object. Specifically, this function implements an algorithm to add assessment of quality of life six dimension (adolescent version) scores. Function argument unscored_aqol_tb specifies the object to be updated. The function returns Transformed Assessment of Quality of Life (a tibble).
#' @param unscored_aqol_tb Unscored Assessment of Quality of Life (a tibble)
#' @param prefix_1L_chr Prefix (a character vector of length one), Default: 'aqol6d_q'
#' @param id_var_nm_1L_chr Identity variable name (a character vector of length one), Default: 'fkClientID'
#' @param wtd_aqol_var_nm_1L_chr Weighted Assessment of Quality of Life variable name (a character vector of length one), Default: 'aqol6d_total_w'
#' @param total_aqol_var_nm_1L_chr Total Assessment of Quality of Life variable name (a character vector of length one), Default: 'aqol6d_total_c'
#' @return Transformed Assessment of Quality of Life (a tibble)
#' @rdname add_adol6d_scores
#' @export 
#' @importFrom dplyr select starts_with rename_all rename group_by mutate n ungroup arrange inner_join across filter
#' @importFrom tidyselect all_of
#' @importFrom stringr str_replace
#' @importFrom tibble as_tibble
#' @importFrom rlang sym
#' @importFrom purrr map
#' @importFrom Hmisc `label<-` label
add_adol6d_scores <- function (unscored_aqol_tb, prefix_1L_chr = "aqol6d_q", id_var_nm_1L_chr = "fkClientID", 
    wtd_aqol_var_nm_1L_chr = "aqol6d_total_w", total_aqol_var_nm_1L_chr = "aqol6d_total_c") 
{
    complete_ds_tb <- unscored_aqol_tb
    unscored_aqol_tb <- unscored_aqol_tb %>% dplyr::select(tidyselect::all_of(id_var_nm_1L_chr), 
        dplyr::starts_with(unname(prefix_1L_chr)))
    old_nms_chr <- names(unscored_aqol_tb)
    names(unscored_aqol_tb) <- c("ID", paste0("Q", 1:20))
    unscored_aqol_tb <- impute_unscrd_adol_aqol6d_ds(unscored_aqol_tb)
    disvals_tb <- unscored_aqol_tb %>% add_itm_disv_to_aqol6d_itms_tb(disvalues_lup_tb = make_adol_aqol6d_disv_lup(), 
        pfx_1L_chr = "Q") %>% dplyr::select(ID, dplyr::starts_with("dv_")) %>% 
        dplyr::rename_all(~stringr::str_replace(.x, "dv_", "dv"))
    scored_aqol_tb <- add_aqol6d_adol_dim_scrg_eqs(disvals_tb) %>% 
        tibble::as_tibble() %>% dplyr::rename(`:=`(!!rlang::sym(id_var_nm_1L_chr), 
        ID), `:=`(!!rlang::sym(wtd_aqol_var_nm_1L_chr), uaqol))
    tbs_ls <- list(complete_ds_tb, scored_aqol_tb) %>% purrr::map(~.x %>% 
        dplyr::group_by(!!rlang::sym(id_var_nm_1L_chr)) %>% dplyr::mutate(match_var_chr = paste0(!!rlang::sym(id_var_nm_1L_chr), 
        "_", 1:dplyr::n())) %>% dplyr::ungroup() %>% dplyr::arrange(!!rlang::sym(id_var_nm_1L_chr)))
    if ("labelled" %in% class(tbs_ls[[1]][[wtd_aqol_var_nm_1L_chr]])) {
        tbs_ls[[2]][[wtd_aqol_var_nm_1L_chr]] <- Hmisc::`label<-`(tbs_ls[[2]][[wtd_aqol_var_nm_1L_chr]], 
            value = Hmisc::label(tbs_ls[[1]][[wtd_aqol_var_nm_1L_chr]]))
    }
    else {
        if ("labelled" %in% class(tbs_ls[[2]][[wtd_aqol_var_nm_1L_chr]])) {
            class(tbs_ls[[2]][[wtd_aqol_var_nm_1L_chr]]) <- setdiff(class(tbs_ls[[2]][[wtd_aqol_var_nm_1L_chr]]), 
                "labelled")
            attr(tbs_ls[[2]][[wtd_aqol_var_nm_1L_chr]], "label") <- NULL
        }
    }
    tfd_aqol_tb <- dplyr::inner_join(tbs_ls[[1]], tbs_ls[[2]]) %>% 
        dplyr::select(-match_var_chr) %>% dplyr::mutate(`:=`(!!rlang::sym(total_aqol_var_nm_1L_chr), 
        rowSums(dplyr::across(dplyr::starts_with(prefix_1L_chr)))))
    tfd_aqol_tb <- tfd_aqol_tb %>% dplyr::filter(!is.na(!!rlang::sym(total_aqol_var_nm_1L_chr)))
    return(tfd_aqol_tb)
}
#' Add Assessment of Quality of Life Six Dimension adolescent dimension scoring equations
#' @description add_aqol6d_adol_dim_scrg_eqs() is an Add function that updates an object by adding data to that object. Specifically, this function implements an algorithm to add assessment of quality of life six dimension adolescent dimension scoring equations. Function argument unscored_aqol_tb specifies the object to be updated. The function returns Unscored Assessment of Quality of Life (a tibble).
#' @param unscored_aqol_tb Unscored Assessment of Quality of Life (a tibble)
#' @return Unscored Assessment of Quality of Life (a tibble)
#' @rdname add_aqol6d_adol_dim_scrg_eqs
#' @export 
#' @importFrom utils data
#' @importFrom dplyr mutate
#' @importFrom rlang parse_expr
#' @importFrom Hmisc label
#' @keywords internal
add_aqol6d_adol_dim_scrg_eqs <- function (unscored_aqol_tb) 
{
    utils::data("adol_dim_scalg_eqs_lup", package = "youthvars", 
        envir = environment())
    for (var in adol_dim_scalg_eqs_lup$Dim_scal) {
        expression = adol_dim_scalg_eqs_lup[adol_dim_scalg_eqs_lup$Dim_scal == 
            var, ]$Equ
        unscored_aqol_tb <- unscored_aqol_tb %>% dplyr::mutate(`:=`(!!var, 
            !!rlang::parse_expr(expression)))
        Hmisc::label(unscored_aqol_tb[, var]) = adol_dim_scalg_eqs_lup[adol_dim_scalg_eqs_lup$Dim_scal == 
            var, ]$Label
    }
    return(unscored_aqol_tb)
}
#' Add Assessment of Quality of Life Six Dimension items to Assessment of Quality of Life Six Dimension tibbles
#' @description add_aqol6d_items_to_aqol6d_tbs_ls() is an Add function that updates an object by adding data to that object. Specifically, this function implements an algorithm to add assessment of quality of life six dimension items to assessment of quality of life six dimension tibbles list. Function argument aqol6d_tbs_ls specifies the object to be updated. The function returns Updated Assessment of Quality of Life Six Dimension tibbles (a list).
#' @param aqol6d_tbs_ls Assessment of Quality of Life Six Dimension tibbles (a list)
#' @param aqol_items_prpns_tbs_ls Assessment of Quality of Life items proportions tibbles (a list)
#' @param prefix_chr Prefix (a character vector)
#' @param aqol_tots_var_nms_chr Assessment of Quality of Life totals variable names (a character vector)
#' @param id_var_nm_1L_chr Identity variable name (a character vector of length one), Default: 'fkClientID'
#' @param scaling_cnst_dbl Scaling cnst (a double vector), Default: 5
#' @return Updated Assessment of Quality of Life Six Dimension tibbles (a list)
#' @rdname add_aqol6d_items_to_aqol6d_tbs_ls
#' @export 
#' @importFrom purrr map2 map reduce map_int
#' @importFrom dplyr select mutate arrange left_join starts_with everything
#' @importFrom stats na.omit
#' @importFrom simstudy defData genData
#' @importFrom rlang sym
#' @importFrom tibble rowid_to_column
#' @keywords internal
add_aqol6d_items_to_aqol6d_tbs_ls <- function (aqol6d_tbs_ls, aqol_items_prpns_tbs_ls, prefix_chr, 
    aqol_tots_var_nms_chr, id_var_nm_1L_chr = "fkClientID", scaling_cnst_dbl = 5) 
{
    updated_aqol6d_tbs_ls <- purrr::map2(aqol6d_tbs_ls, aqol_items_prpns_tbs_ls, 
        ~{
            nbr_obs_1L_int <- nrow(.x) * scaling_cnst_dbl
            transposed_items_props_tb <- .y %>% dplyr::select(-Question) %>% 
                t()
            item_ranges_dbl_ls <- 1:ncol(transposed_items_props_tb) %>% 
                purrr::map(~c(1, length(transposed_items_props_tb[, 
                  .x] %>% stats::na.omit())))
            cat_probs_def_tbl <- purrr::reduce(1:ncol(transposed_items_props_tb), 
                .init = NULL, ~simstudy::defData(.x, varname = paste0("aqol6d_q", 
                  .y), formula = transposed_items_props_tb[, 
                  .y] %>% stats::na.omit() %>% as.vector() %>% 
                  format(digits = 10) %>% paste0(collapse = ";"), 
                  dist = "categorical"))
            items_tb <- simstudy::genData(nbr_obs_1L_int, cat_probs_def_tbl) %>% 
                dplyr::select(-id) %>% dplyr::mutate(`:=`(!!rlang::sym(unname(aqol_tots_var_nms_chr["cumulative"])), 
                rowSums(., na.rm = T))) %>% dplyr::arrange(!!rlang::sym(unname(aqol_tots_var_nms_chr["cumulative"]))) %>% 
                tibble::rowid_to_column("id")
            items_tb <- items_tb %>% dplyr::mutate(aqol6dU = calculate_adol_aqol6dU(items_tb, 
                prefix_1L_chr = prefix_chr["aqol_item"], id_var_nm_1L_chr = "id"))
            .x <- .x %>% dplyr::mutate(id = purrr::map_int(aqol6d_total_w, 
                ~which.min(abs(items_tb$aqol6dU - .x)))) %>% 
                dplyr::left_join(items_tb)
            updated_tb <- .x %>% dplyr::mutate(`:=`(!!rlang::sym(unname(aqol_tots_var_nms_chr["weighted"])), 
                aqol6dU)) %>% dplyr::select(-aqol6dU, -id) %>% 
                dplyr::select(!!rlang::sym(id_var_nm_1L_chr), 
                  dplyr::starts_with(prefix_chr[["aqol_item"]]), 
                  !!rlang::sym(unname(aqol_tots_var_nms_chr["cumulative"])), 
                  !!rlang::sym(unname(aqol_tots_var_nms_chr["weighted"])), 
                  dplyr::everything())
            updated_tb
        })
    return(updated_aqol6d_tbs_ls)
}
#' Add Assessment of Quality of Life Six Dimension Health Utility to Assessment of Quality of Life Six Dimension items
#' @description add_aqol6dU_to_aqol6d_items_tb() is an Add function that updates an object by adding data to that object. Specifically, this function implements an algorithm to add assessment of quality of life six dimension health utility to assessment of quality of life six dimension items tibble. Function argument aqol6d_items_tb specifies the object to be updated. The function returns Assessment of Quality of Life Six Dimension items (a tibble).
#' @param aqol6d_items_tb Assessment of Quality of Life Six Dimension items (a tibble)
#' @param coefs_lup_tb Coefficients lookup table (a tibble), Default: aqol6d_from_8d_coefs_lup_tb
#' @return Assessment of Quality of Life Six Dimension items (a tibble)
#' @rdname add_aqol6dU_to_aqol6d_items_tb
#' @export 
#' @importFrom dplyr pull mutate
#' @importFrom purrr map_dbl
#' @keywords internal
add_aqol6dU_to_aqol6d_items_tb <- function (aqol6d_items_tb, coefs_lup_tb = aqol6d_from_8d_coefs_lup_tb) 
{
    coef_dbl <- coefs_lup_tb[match(c(paste0("vD", 1:6), "Constant"), 
        coefs_lup_tb$var_name_chr), ] %>% dplyr::pull(coef_dbl)
    aqol6d_items_tb <- aqol6d_items_tb %>% dplyr::mutate(aqol6dU = coef_dbl[1] * 
        vD1 + coef_dbl[2] * vD2 + coef_dbl[3] * vD3 + coef_dbl[4] * 
        vD4 + coef_dbl[5] * vD5 + coef_dbl[6] * vD6 + coef_dbl[7]) %>% 
        dplyr::mutate(aqol6dU = aqol6dU %>% purrr::map_dbl(~ifelse(.x > 
            1, 1, .x)))
    return(aqol6d_items_tb)
}
#' Add Assessment of Quality of Life Six Dimension Health Utility to Assessment of Quality of Life Six Dimension tibbles
#' @description add_aqol6dU_to_aqol6d_tbs_ls() is an Add function that updates an object by adding data to that object. Specifically, this function implements an algorithm to add assessment of quality of life six dimension health utility to assessment of quality of life six dimension tibbles list. Function argument aqol6d_tbs_ls specifies the object to be updated. The function returns Assessment of Quality of Life Six Dimension tibbles (a list).
#' @param aqol6d_tbs_ls Assessment of Quality of Life Six Dimension tibbles (a list)
#' @param prefix_1L_chr Prefix (a character vector of length one), Default: 'aqol6d_q'
#' @param id_var_nm_1L_chr Identity variable name (a character vector of length one)
#' @return Assessment of Quality of Life Six Dimension tibbles (a list)
#' @rdname add_aqol6dU_to_aqol6d_tbs_ls
#' @export 
#' @importFrom purrr map
#' @importFrom dplyr mutate
#' @keywords internal
add_aqol6dU_to_aqol6d_tbs_ls <- function (aqol6d_tbs_ls, prefix_1L_chr = "aqol6d_q", id_var_nm_1L_chr) 
{
    aqol6d_tbs_ls <- aqol6d_tbs_ls %>% purrr::map(~.x %>% dplyr::mutate(aqol6dU = calculate_adol_aqol6dU(.x, 
        prefix_1L_chr = prefix_1L_chr, id_var_nm_1L_chr = id_var_nm_1L_chr)))
    return(aqol6d_tbs_ls)
}
#' Add correlations and utilities to Assessment of Quality of Life Six Dimension tibbles
#' @description add_cors_and_utls_to_aqol6d_tbs_ls() is an Add function that updates an object by adding data to that object. Specifically, this function implements an algorithm to add correlations and utilities to assessment of quality of life six dimension tibbles list. Function argument aqol6d_tbs_ls specifies the object to be updated. The function returns Assessment of Quality of Life Six Dimension tibbles (a list).
#' @param aqol6d_tbs_ls Assessment of Quality of Life Six Dimension tibbles (a list)
#' @param aqol_scores_pars_ls Assessment of Quality of Life scores parameters (a list)
#' @param aqol_items_prpns_tbs_ls Assessment of Quality of Life items proportions tibbles (a list)
#' @param temporal_cors_ls Temporal correlations (a list)
#' @param prefix_chr Prefix (a character vector)
#' @param aqol_tots_var_nms_chr Assessment of Quality of Life totals variable names (a character vector)
#' @param id_var_nm_1L_chr Identity variable name (a character vector of length one), Default: 'fkClientID'
#' @return Assessment of Quality of Life Six Dimension tibbles (a list)
#' @rdname add_cors_and_utls_to_aqol6d_tbs_ls
#' @export 

#' @keywords internal
add_cors_and_utls_to_aqol6d_tbs_ls <- function (aqol6d_tbs_ls, aqol_scores_pars_ls, aqol_items_prpns_tbs_ls, 
    temporal_cors_ls, prefix_chr, aqol_tots_var_nms_chr, id_var_nm_1L_chr = "fkClientID") 
{
    aqol6d_tbs_ls <- reorder_tbs_for_target_cors(aqol6d_tbs_ls, 
        cor_dbl = temporal_cors_ls[[1]], cor_var_chr = rep(names(temporal_cors_ls)[1], 
            2), id_var_to_rm_1L_chr = "id") %>% add_uids_to_tbs_ls(prefix_1L_chr = prefix_chr[["uid"]], 
        id_var_nm_1L_chr = id_var_nm_1L_chr)
    aqol6d_tbs_ls <- aqol6d_tbs_ls %>% add_aqol6d_items_to_aqol6d_tbs_ls(aqol_items_prpns_tbs_ls = aqol_items_prpns_tbs_ls, 
        prefix_chr = prefix_chr, aqol_tots_var_nms_chr = aqol_tots_var_nms_chr, 
        id_var_nm_1L_chr = id_var_nm_1L_chr)
    return(aqol6d_tbs_ls)
}
#' Add dimension disvalue to Assessment of Quality of Life Six Dimension items
#' @description add_dim_disv_to_aqol6d_items_tb() is an Add function that updates an object by adding data to that object. Specifically, this function implements an algorithm to add dimension disvalue to assessment of quality of life six dimension items tibble. Function argument aqol6d_items_tb specifies the object to be updated. The function returns Assessment of Quality of Life Six Dimension items (a tibble).
#' @param aqol6d_items_tb Assessment of Quality of Life Six Dimension items (a tibble)
#' @param domain_items_ls Domain items (a list)
#' @param domains_chr Domains (a character vector)
#' @param dim_sclg_con_lup_tb Dimension scaling constant lookup table (a tibble), Default: NULL
#' @param itm_wrst_wghts_lup_tb Item worst wghts lookup table (a tibble), Default: NULL
#' @return Assessment of Quality of Life Six Dimension items (a tibble)
#' @rdname add_dim_disv_to_aqol6d_items_tb
#' @export 
#' @importFrom utils data
#' @importFrom purrr reduce
#' @importFrom dplyr select mutate
#' @importFrom rlang sym exec
#' @keywords internal
add_dim_disv_to_aqol6d_items_tb <- function (aqol6d_items_tb, domain_items_ls, domains_chr, dim_sclg_con_lup_tb = NULL, 
    itm_wrst_wghts_lup_tb = NULL) 
{
    if (is.null(dim_sclg_con_lup_tb)) {
        utils::data("aqol6d_dim_sclg_con_lup_tb", envir = environment())
        dim_sclg_con_lup_tb <- aqol6d_dim_sclg_con_lup_tb
    }
    if (is.null(itm_wrst_wghts_lup_tb)) {
        utils::data("aqol6d_adult_itm_wrst_wghts_lup_tb", envir = environment())
        itm_wrst_wghts_lup_tb <- aqol6d_adult_itm_wrst_wghts_lup_tb
    }
    aqol6d_disu_fn_ls <- make_aqol6d_fns_ls(domain_items_ls)
    kD_dbl <- make_dim_sclg_cons_dbl(domains_chr = domains_chr, 
        dim_sclg_con_lup_tb = dim_sclg_con_lup_tb)
    w_dbl_ls <- make_make_item_wrst_wts_ls_ls(domain_items_ls = domain_items_ls, 
        itm_wrst_wghts_lup_tb = itm_wrst_wghts_lup_tb)
    aqol6d_items_tb <- purrr::reduce(1:length(domain_items_ls), 
        .init = aqol6d_items_tb, ~{
            args_ls <- list(dvQs_tb = .x %>% dplyr::select(domain_items_ls[[.y]] %>% 
                paste0("dv_", .)), kD_1L_dbl = kD_dbl[.y], w_dbl = w_dbl_ls[[.y]])
            .x %>% dplyr::mutate(`:=`(!!rlang::sym(paste0("dvD", 
                .y)), rlang::exec(aqol6d_disu_fn_ls[[.y]], !!!args_ls)))
        })
    return(aqol6d_items_tb)
}
#' Add dimension scores to Assessment of Quality of Life Six Dimension items
#' @description add_dim_scores_to_aqol6d_items_tb() is an Add function that updates an object by adding data to that object. Specifically, this function implements an algorithm to add dimension scores to assessment of quality of life six dimension items tibble. Function argument aqol6d_items_tb specifies the object to be updated. The function returns Assessment of Quality of Life Six Dimension items (a tibble).
#' @param aqol6d_items_tb Assessment of Quality of Life Six Dimension items (a tibble)
#' @param domain_items_ls Domain items (a list)
#' @return Assessment of Quality of Life Six Dimension items (a tibble)
#' @rdname add_dim_scores_to_aqol6d_items_tb
#' @export 
#' @importFrom dplyr mutate across rename_with
#' @importFrom stringr str_replace
#' @keywords internal
add_dim_scores_to_aqol6d_items_tb <- function (aqol6d_items_tb, domain_items_ls) 
{
    aqol6d_items_tb <- aqol6d_items_tb %>% dplyr::mutate(dplyr::across(paste0("dvD", 
        1:length(domain_items_ls)), .fns = list(vD = ~1 - .x), 
        .names = "{fn}_{col}")) %>% dplyr::rename_with(~stringr::str_replace(., 
        "vD_dvD", "vD"))
    return(aqol6d_items_tb)
}
#' Add interval variable
#' @description add_interval_var() is an Add function that updates an object by adding data to that object. Specifically, this function implements an algorithm to add interval variable. Function argument data_tb specifies the object to be updated. The function returns Updated data (a tibble).
#' @param data_tb Data (a tibble)
#' @param id_var_nm_1L_chr Identity variable name (a character vector of length one), Default: 'fkClientID'
#' @param msrmnt_date_var_nm_1L_chr Measurement date variable name (a character vector of length one), Default: 'd_interview_date'
#' @param time_unit_1L_chr Time unit (a character vector of length one), Default: 'days'
#' @param bl_date_var_nm_1L_chr Baseline date variable name (a character vector of length one), Default: 'bl_date_dtm'
#' @param interval_var_nm_1L_chr Interval variable name (a character vector of length one), Default: 'interval_dbl'
#' @param temp_row_nbr_var_nm_1L_chr Temporary row number variable name (a character vector of length one), Default: 'temp_row_nbr_int'
#' @param drop_bl_date_var_1L_lgl Drop baseline date variable (a logical vector of length one), Default: F
#' @return Updated data (a tibble)
#' @rdname add_interval_var
#' @export 
#' @importFrom dplyr ungroup mutate n group_by arrange first select
#' @importFrom rlang sym
#' @importFrom purrr map2_dbl
#' @importFrom lubridate interval time_length
add_interval_var <- function (data_tb, id_var_nm_1L_chr = "fkClientID", msrmnt_date_var_nm_1L_chr = "d_interview_date", 
    time_unit_1L_chr = "days", bl_date_var_nm_1L_chr = "bl_date_dtm", 
    interval_var_nm_1L_chr = "interval_dbl", temp_row_nbr_var_nm_1L_chr = "temp_row_nbr_int", 
    drop_bl_date_var_1L_lgl = F) 
{
    updated_data_tb <- data_tb %>% dplyr::ungroup() %>% dplyr::mutate(`:=`(!!rlang::sym(temp_row_nbr_var_nm_1L_chr), 
        1:dplyr::n())) %>% dplyr::group_by(!!rlang::sym(id_var_nm_1L_chr)) %>% 
        dplyr::arrange(!!rlang::sym(msrmnt_date_var_nm_1L_chr)) %>% 
        dplyr::mutate(`:=`(!!rlang::sym(bl_date_var_nm_1L_chr), 
            !!rlang::sym(msrmnt_date_var_nm_1L_chr) %>% dplyr::first())) %>% 
        dplyr::mutate(interval_dbl = purrr::map2_dbl(!!rlang::sym(bl_date_var_nm_1L_chr), 
            !!rlang::sym(msrmnt_date_var_nm_1L_chr), ~lubridate::interval(.x, 
                .y) %>% lubridate::time_length(unit = time_unit_1L_chr))) %>% 
        dplyr::ungroup() %>% dplyr::arrange(!!rlang::sym(temp_row_nbr_var_nm_1L_chr)) %>% 
        dplyr::select(-!!rlang::sym(temp_row_nbr_var_nm_1L_chr))
    if (drop_bl_date_var_1L_lgl) 
        updated_data_tb <- updated_data_tb %>% dplyr::select(-!!rlang::sym(bl_date_var_nm_1L_chr))
    return(updated_data_tb)
}
#' Add item disvalue to Assessment of Quality of Life Six Dimension items
#' @description add_itm_disv_to_aqol6d_itms_tb() is an Add function that updates an object by adding data to that object. Specifically, this function implements an algorithm to add item disvalue to assessment of quality of life six dimension items tibble. Function argument aqol6d_items_tb specifies the object to be updated. The function returns Assessment of Quality of Life Six Dimension items (a tibble).
#' @param aqol6d_items_tb Assessment of Quality of Life Six Dimension items (a tibble)
#' @param disvalues_lup_tb Disvalues lookup table (a tibble), Default: NULL
#' @param pfx_1L_chr Prefix (a character vector of length one)
#' @return Assessment of Quality of Life Six Dimension items (a tibble)
#' @rdname add_itm_disv_to_aqol6d_itms_tb
#' @export 
#' @importFrom utils data
#' @importFrom purrr reduce
#' @importFrom dplyr mutate across
#' @importFrom tidyselect all_of
#' @keywords internal
add_itm_disv_to_aqol6d_itms_tb <- function (aqol6d_items_tb, disvalues_lup_tb = NULL, pfx_1L_chr) 
{
    if (is.null(disvalues_lup_tb)) {
        utils::data("aqol6d_adult_disv_lup_tb", package = "youthvars", 
            envir = environment())
        disvalues_lup_tb <- aqol6d_adult_disv_lup_tb
    }
    aqol6d_items_tb <- purrr::reduce(1:20, .init = aqol6d_items_tb, 
        ~{
            q_1L_chr <- paste0(pfx_1L_chr, .y)
            disu_dbl <- disvalues_lup_tb[.y, -1] %>% as.numeric()
            .x %>% dplyr::mutate(dplyr::across(tidyselect::all_of(q_1L_chr), 
                .fns = list(dv = ~disu_dbl[.x]), .names = "{fn}_{col}"))
        })
    return(aqol6d_items_tb)
}
#' Add labels to Assessment of Quality of Life Six Dimension
#' @description add_labels_to_aqol6d_tb() is an Add function that updates an object by adding data to that object. Specifically, this function implements an algorithm to add labels to assessment of quality of life six dimension tibble. Function argument aqol6d_tb specifies the object to be updated. The function returns Assessment of Quality of Life Six Dimension (a tibble).
#' @param aqol6d_tb Assessment of Quality of Life Six Dimension (a tibble)
#' @param labels_chr Labels (a character vector), Default: 'NA'
#' @return Assessment of Quality of Life Six Dimension (a tibble)
#' @rdname add_labels_to_aqol6d_tb
#' @export 
#' @importFrom Hmisc label
#' @keywords internal
add_labels_to_aqol6d_tb <- function (aqol6d_tb, labels_chr = NA_character_) 
{
    if (is.na(labels_chr)) 
        labels_chr <- c(fkClientID = "Unique client identifier", 
            round = "Data measurement round", d_age = "Age", 
            d_gender = "Gender", d_sexual_ori_s = "Sexual orientation", 
            d_studying_working = "Work and study", c_p_diag_s = " Primary diagnosis", 
            c_clinical_staging_s = "Clinical stage", c_sofas = "SOFAS", 
            s_centre = "Clinic", d_agegroup = "Age group", d_sex_birth_s = "Sex at birth", 
            d_country_bir_s = "Country of birth", d_ATSI = "Aboriginal and Torres Strait Islander", 
            d_english_home = "English spoken at home", d_english_native = "English is native language", 
            d_relation_s = "Relationship status", aqol6d_total_w = "AQoL health utility", 
            phq9_total = "PHQ9", bads_total = "BADS", gad7_total = "GAD7", 
            oasis_total = "OASIS", scared_total = "SCARED", k6_total = "K6", 
            aqol6d_total_c = "AQoL unweighted total", aqol6d_q1 = "Household tasks", 
            aqol6d_q2 = "Getting around", aqol6d_q3 = "Mobility", 
            aqol6d_q4 = "Self care", aqol6d_q5 = "Enjoy close rels", 
            aqol6d_q6 = "Family rels", aqol6d_q7 = "Community involvement", 
            aqol6d_q8 = "Despair", aqol6d_q9 = "Worry", aqol6d_q10 = "Sad", 
            aqol6d_q11 = "Agitated", aqol6d_q12 = "Energy level", 
            aqol6d_q13 = "Control", aqol6d_q14 = "Coping", aqol6d_q15 = "Frequency of pain", 
            aqol6d_q16 = "Degree of pain", aqol6d_q17 = "Pain interference", 
            aqol6d_q18 = "Vision", aqol6d_q19 = "Hearing", aqol6d_q20 = "Communication", 
            aqol6d_subtotal_c_IL = "Unweighted Independent Living", 
            aqol6d_subtotal_c_REL = "Unweighted Relationships", 
            aqol6d_subtotal_c_MH = "Unweighted Mental Health", 
            aqol6d_subtotal_c_COP = "Unweighted Coping", aqol6d_subtotal_c_P = "Unweighted Pain", 
            aqol6d_subtotal_c_SEN = "Unweighted Sense", aqol6d_subtotal_w_IL = "Independent Living", 
            aqol6d_subtotal_w_REL = "Relationships", aqol6d_subtotal_w_MH = "Mental Health", 
            aqol6d_subtotal_w_COP = "Coping", aqol6d_subtotal_w_P = "Pain", 
            aqol6d_subtotal_w_SEN = "Sense")
    Hmisc::label(aqol6d_tb) = as.list(labels_chr[match(names(aqol6d_tb), 
        names(labels_chr))])
    return(aqol6d_tb)
}
#' Add participation variable
#' @description add_participation_var() is an Add function that updates an object by adding data to that object. Specifically, this function implements an algorithm to add participation variable. Function argument data_tb specifies the object to be updated. The function returns Data (a tibble).
#' @param data_tb Data (a tibble)
#' @param id_var_nm_1L_chr Identity variable name (a character vector of length one), Default: 'fkClientID'
#' @param fup_round_nmbr_1L_int Follow-up round nmbr (an integer vector of length one), Default: 2
#' @return Data (a tibble)
#' @rdname add_participation_var
#' @export 
#' @importFrom dplyr group_by mutate n ungroup select
#' @importFrom rlang sym
add_participation_var <- function (data_tb, id_var_nm_1L_chr = "fkClientID", fup_round_nmbr_1L_int = 2L) 
{
    data_tb <- data_tb %>% dplyr::group_by(!!rlang::sym(id_var_nm_1L_chr)) %>% 
        dplyr::mutate(nbr_rounds_int = dplyr::n()) %>% dplyr::mutate(participation = ifelse(nbr_rounds_int == 
        1, "Baseline only", ifelse(nbr_rounds_int == fup_round_nmbr_1L_int, 
        "Baseline and follow-up", NA_character_))) %>% dplyr::ungroup() %>% 
        dplyr::select(-nbr_rounds_int)
    return(data_tb)
}
#' Add unwtd dimension totals
#' @description add_unwtd_dim_tots() is an Add function that updates an object by adding data to that object. Specifically, this function implements an algorithm to add unwtd dimension totals. Function argument items_tb specifies the object to be updated. The function returns Items and domains (a tibble).
#' @param items_tb Items (a tibble)
#' @param domain_items_ls Domain items (a list)
#' @param domain_pfx_1L_chr Domain prefix (a character vector of length one)
#' @return Items and domains (a tibble)
#' @rdname add_unwtd_dim_tots
#' @export 
#' @importFrom purrr reduce
#' @importFrom dplyr mutate select
#' @importFrom rlang sym
#' @keywords internal
add_unwtd_dim_tots <- function (items_tb, domain_items_ls, domain_pfx_1L_chr) 
{
    items_and_domains_tb <- purrr::reduce(1:length(domain_items_ls), 
        .init = items_tb, ~.x %>% dplyr::mutate(`:=`(!!rlang::sym(paste0(domain_pfx_1L_chr, 
            names(domain_items_ls)[.y])), rowSums(dplyr::select(., 
            domain_items_ls[[.y]])))))
    return(items_and_domains_tb)
}
#' Add weighted dimension totals
#' @description add_wtd_dim_tots() is an Add function that updates an object by adding data to that object. Specifically, this function implements an algorithm to add weighted dimension totals. Function argument unwtd_dim_tb specifies the object to be updated. The function returns Weighted and unwtd dimension (a tibble).
#' @param unwtd_dim_tb Unwtd dimension (a tibble)
#' @param domain_items_ls Domain items (a list)
#' @param domain_unwtd_pfx_1L_chr Domain unwtd prefix (a character vector of length one)
#' @param domain_wtd_pfx_1L_chr Domain weighted prefix (a character vector of length one)
#' @return Weighted and unwtd dimension (a tibble)
#' @rdname add_wtd_dim_tots
#' @export 
#' @importFrom utils data
#' @importFrom purrr map_dbl map2_dbl discard reduce
#' @importFrom dplyr filter pull select_if mutate
#' @importFrom rlang sym
#' @keywords internal
add_wtd_dim_tots <- function (unwtd_dim_tb, domain_items_ls, domain_unwtd_pfx_1L_chr, 
    domain_wtd_pfx_1L_chr) 
{
    utils::data("aqol6d_adult_disv_lup_tb", package = "youthvars", 
        envir = environment())
    utils::data("aqol6d_domain_qs_lup_tb", package = "youthvars", 
        envir = environment())
    min_vals_dbl <- purrr::map_dbl(domain_items_ls, ~length(.x)) %>% 
        unname()
    max_vals_dbl <- purrr::map2_dbl(domain_items_ls, names(domain_items_ls), 
        ~{
            paste0("Q", aqol6d_domain_qs_lup_tb %>% dplyr::filter(Domain_chr == 
                .y) %>% dplyr::pull(Question_dbl)) %>% purrr::map_dbl(~{
                tb <- aqol6d_adult_disv_lup_tb %>% dplyr::filter(Question_chr == 
                  .x) %>% dplyr::select_if(is.numeric)
                as.numeric(as.data.frame(tb)[1, ]) %>% purrr::discard(is.na) %>% 
                  length()
            }) %>% sum()
        }) %>% unname()
    wtd_and_unwtd_dim_tb <- purrr::reduce(1:length(domain_items_ls), 
        .init = unwtd_dim_tb, ~.x %>% dplyr::mutate(`:=`(!!rlang::sym(paste0(domain_wtd_pfx_1L_chr, 
            names(domain_items_ls)[.y])), (1 - (!!rlang::sym(paste0(domain_unwtd_pfx_1L_chr, 
            names(domain_items_ls)[.y])) - min_vals_dbl[.y])/(max_vals_dbl[.y] - 
            min_vals_dbl[.y])))))
    return(wtd_and_unwtd_dim_tb)
}
