#' Add adol6d scores
#' @description add_adol6d_scores() is an Add function that updates an object by adding data to that object. Specifically, this function implements an algorithm to add adol6d scores. Function argument unscored_aqol_tb specifies the object to be updated. The function returns Transformed Assessment of Quality of Life (a tibble).
#' @param unscored_aqol_tb Unscored Assessment of Quality of Life (a tibble)
#' @param prefix_1L_chr Prefix (a character vector of length one), Default: 'aqol6d_q'
#' @param id_var_nm_1L_chr Id var name (a character vector of length one), Default: 'fkClientID'
#' @param wtd_aqol_var_nm_1L_chr Wtd Assessment of Quality of Life var name (a character vector of length one), Default: 'aqol6d_total_w'
#' @return Transformed Assessment of Quality of Life (a tibble)
#' @rdname add_adol6d_scores
#' @export 
#' @importFrom dplyr select starts_with rename_all rename group_by mutate n ungroup arrange inner_join
#' @importFrom stringr str_replace
#' @importFrom tibble as_tibble
#' @importFrom rlang sym
#' @importFrom purrr map
add_adol6d_scores <- function (unscored_aqol_tb, prefix_1L_chr = "aqol6d_q", id_var_nm_1L_chr = "fkClientID", 
    wtd_aqol_var_nm_1L_chr = "aqol6d_total_w") 
{
    complete_ds_tb <- unscored_aqol_tb
    unscored_aqol_tb <- unscored_aqol_tb %>% dplyr::select(id_var_nm_1L_chr, 
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
    tfd_aqol_tb <- dplyr::inner_join(tbs_ls[[1]], tbs_ls[[2]]) %>% 
        dplyr::select(-match_var_chr)
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
    utils::data("adol_dim_scalg_eqs_lup", envir = environment())
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
#' @param aqol_items_props_tbs_ls Assessment of Quality of Life items props tibbles (a list)
#' @param prefix_chr Prefix (a character vector)
#' @param aqol_tots_var_nms_chr Assessment of Quality of Life tots var names (a character vector)
#' @param id_var_nm_1L_chr Id var name (a character vector of length one), Default: 'fkClientID'
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
add_aqol6d_items_to_aqol6d_tbs_ls <- function (aqol6d_tbs_ls, aqol_items_props_tbs_ls, prefix_chr, 
    aqol_tots_var_nms_chr, id_var_nm_1L_chr = "fkClientID", scaling_cnst_dbl = 5) 
{
    updated_aqol6d_tbs_ls <- purrr::map2(aqol6d_tbs_ls, aqol_items_props_tbs_ls, 
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
#' @param coeffs_lup_tb Coeffs lookup table (a tibble), Default: aqol6d_from_8d_coeffs_lup_tb
#' @return Assessment of Quality of Life Six Dimension items (a tibble)
#' @rdname add_aqol6dU_to_aqol6d_items_tb
#' @export 
#' @importFrom dplyr pull mutate
#' @importFrom purrr map_dbl
#' @keywords internal
add_aqol6dU_to_aqol6d_items_tb <- function (aqol6d_items_tb, coeffs_lup_tb = aqol6d_from_8d_coeffs_lup_tb) 
{
    coeff_dbl <- coeffs_lup_tb[match(c(paste0("vD", 1:6), "Constant"), 
        coeffs_lup_tb$var_name_chr), ] %>% dplyr::pull(coeff_dbl)
    aqol6d_items_tb <- aqol6d_items_tb %>% dplyr::mutate(aqol6dU = coeff_dbl[1] * 
        vD1 + coeff_dbl[2] * vD2 + coeff_dbl[3] * vD3 + coeff_dbl[4] * 
        vD4 + coeff_dbl[5] * vD5 + coeff_dbl[6] * vD6 + coeff_dbl[7]) %>% 
        dplyr::mutate(aqol6dU = aqol6dU %>% purrr::map_dbl(~ifelse(.x > 
            1, 1, .x)))
    return(aqol6d_items_tb)
}
#' Add Assessment of Quality of Life Six Dimension Health Utility to Assessment of Quality of Life Six Dimension tibbles
#' @description add_aqol6dU_to_aqol6d_tbs_ls() is an Add function that updates an object by adding data to that object. Specifically, this function implements an algorithm to add assessment of quality of life six dimension health utility to assessment of quality of life six dimension tibbles list. Function argument aqol6d_tbs_ls specifies the object to be updated. The function returns Assessment of Quality of Life Six Dimension tibbles (a list).
#' @param aqol6d_tbs_ls Assessment of Quality of Life Six Dimension tibbles (a list)
#' @param prefix_1L_chr Prefix (a character vector of length one), Default: 'aqol6d_q'
#' @param id_var_nm_1L_chr Id var name (a character vector of length one)
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
#' Add dimension disvalue to Assessment of Quality of Life Six Dimension items
#' @description add_dim_disv_to_aqol6d_items_tb() is an Add function that updates an object by adding data to that object. Specifically, this function implements an algorithm to add dimension disvalue to assessment of quality of life six dimension items tibble. Function argument aqol6d_items_tb specifies the object to be updated. The function returns Assessment of Quality of Life Six Dimension items (a tibble).
#' @param aqol6d_items_tb Assessment of Quality of Life Six Dimension items (a tibble)
#' @param domain_items_ls Domain items (a list)
#' @param domains_chr Domains (a character vector)
#' @param dim_sclg_con_lup_tb Dimension sclg con lookup table (a tibble), Default: aqol6d_dim_sclg_con_lup_tb
#' @param itm_wrst_wghts_lup_tb Itm wrst wghts lookup table (a tibble), Default: aqol6d_adult_itm_wrst_wghts_lup_tb
#' @return Assessment of Quality of Life Six Dimension items (a tibble)
#' @rdname add_dim_disv_to_aqol6d_items_tb
#' @export 
#' @importFrom purrr reduce
#' @importFrom dplyr select mutate
#' @importFrom rlang sym exec
#' @keywords internal
add_dim_disv_to_aqol6d_items_tb <- function (aqol6d_items_tb, domain_items_ls, domains_chr, dim_sclg_con_lup_tb = aqol6d_dim_sclg_con_lup_tb, 
    itm_wrst_wghts_lup_tb = aqol6d_adult_itm_wrst_wghts_lup_tb) 
{
    aqol6d_disu_fn_ls <- make_aqol6d_fns_ls(domain_items_ls)
    kD_dbl <- make_dim_sclg_cons_dbl(domains_chr = domains_chr, 
        dim_sclg_con_lup_tb = dim_sclg_con_lup_tb)
    w_dbl_ls <- make_item_wrst_wghts_ls_ls(domain_items_ls = domain_items_ls, 
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
#' Add itm disvalue to Assessment of Quality of Life Six Dimension itms
#' @description add_itm_disv_to_aqol6d_itms_tb() is an Add function that updates an object by adding data to that object. Specifically, this function implements an algorithm to add itm disvalue to assessment of quality of life six dimension itms tibble. Function argument aqol6d_items_tb specifies the object to be updated. The function returns Assessment of Quality of Life Six Dimension items (a tibble).
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
        utils::data("aqol6d_adult_disv_lup_tb", envir = environment())
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
#' Add unwtd dimension tots
#' @description add_unwtd_dim_tots() is an Add function that updates an object by adding data to that object. Specifically, this function implements an algorithm to add unwtd dimension tots. Function argument items_tb specifies the object to be updated. The function returns Items and domains (a tibble).
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
#' Add wtd dimension tots
#' @description add_wtd_dim_tots() is an Add function that updates an object by adding data to that object. Specifically, this function implements an algorithm to add wtd dimension tots. Function argument unwtd_dim_tb specifies the object to be updated. The function returns Wtd and unwtd dimension (a tibble).
#' @param unwtd_dim_tb Unwtd dimension (a tibble)
#' @param domain_items_ls Domain items (a list)
#' @param domain_unwtd_pfx_1L_chr Domain unwtd prefix (a character vector of length one)
#' @param domain_wtd_pfx_1L_chr Domain wtd prefix (a character vector of length one)
#' @return Wtd and unwtd dimension (a tibble)
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
    utils::data("aqol6d_adult_disv_lup_tb", envir = environment())
    utils::data("aqol6d_domain_qs_lup_tb", envir = environment())
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