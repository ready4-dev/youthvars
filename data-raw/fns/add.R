add_aqol6d_adol_dim_scrg_eqs <- function (unscored_aqol_tb)
{
  utils::data("adol_dim_scalg_eqs_lup",
              package = "youthvars",
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
                                                                                                                                                                           .y] %>% stats::na.omit() %>% as.vector() %>% format(digits = 10) %>%
                                                                                                               paste0(collapse = ";"), dist = "categorical"))
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
add_adol6d_scores <- function (unscored_aqol_tb,
                               prefix_1L_chr = "aqol6d_q",
                               id_var_nm_1L_chr = "fkClientID",
                               wtd_aqol_var_nm_1L_chr = "aqol6d_total_w")
{
  complete_ds_tb <- unscored_aqol_tb
  unscored_aqol_tb <- unscored_aqol_tb %>% dplyr::select(id_var_nm_1L_chr,
                                                         dplyr::starts_with(unname(prefix_1L_chr)))
  old_nms_chr <- names(unscored_aqol_tb)
  names(unscored_aqol_tb) <- c("ID", paste0("Q", 1:20))
  unscored_aqol_tb <- impute_unscrd_adol_aqol6d_ds(unscored_aqol_tb)
  disvals_tb <- unscored_aqol_tb %>% add_itm_disv_to_aqol6d_itms_tb(disvalues_lup_tb = make_adol_aqol6d_disv_lup(),
                                                                    pfx_1L_chr = "Q") %>%
    dplyr::select(ID, dplyr::starts_with("dv_")) %>%
    dplyr::rename_all(~stringr::str_replace(.x, "dv_", "dv"))
  scored_aqol_tb <- add_aqol6d_adol_dim_scrg_eqs(disvals_tb) %>%
    tibble::as_tibble() %>%
    dplyr::rename(`:=`(!!rlang::sym(id_var_nm_1L_chr), ID),
                  `:=`(!!rlang::sym(wtd_aqol_var_nm_1L_chr), uaqol))
  tbs_ls <- list(complete_ds_tb, scored_aqol_tb) %>%
    purrr::map(~.x %>% dplyr::group_by(!!rlang::sym(id_var_nm_1L_chr)) %>%
                 dplyr::mutate(match_var_chr = paste0(!!rlang::sym(id_var_nm_1L_chr),
                                                      "_",
                                                      1:dplyr::n())) %>%
                 dplyr::ungroup() %>%
                 dplyr::arrange(!!rlang::sym(id_var_nm_1L_chr)) )

  tfd_aqol_tb <- dplyr::inner_join(tbs_ls[[1]], tbs_ls[[2]]) %>%
    dplyr::select(-match_var_chr)
  return(tfd_aqol_tb)
}
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
add_aqol6dU_to_aqol6d_tbs_ls <- function (aqol6d_tbs_ls, prefix_1L_chr = "aqol6d_q", id_var_nm_1L_chr)
{
  aqol6d_tbs_ls <- aqol6d_tbs_ls %>% purrr::map(~.x %>% dplyr::mutate(aqol6dU = calculate_adol_aqol6dU(.x,
                                                                                                       prefix_1L_chr = prefix_1L_chr, id_var_nm_1L_chr = id_var_nm_1L_chr)))
  return(aqol6d_tbs_ls)
}
add_dim_disv_to_aqol6d_items_tb <- function (aqol6d_items_tb, domain_items_ls, domains_chr, dim_sclg_con_lup_tb = aqol6d_dim_sclg_con_lup_tb,
                                             itm_wrst_wghts_lup_tb = aqol6d_adult_itm_wrst_wghts_lup_tb)
{
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
add_dim_scores_to_aqol6d_items_tb <- function (aqol6d_items_tb, domain_items_ls)
{
  aqol6d_items_tb <- aqol6d_items_tb %>% dplyr::mutate(dplyr::across(paste0("dvD",
                                                                            1:length(domain_items_ls)), .fns = list(vD = ~1 - .x),
                                                                     .names = "{fn}_{col}")) %>% dplyr::rename_with(~stringr::str_replace(.,
                                                                                                                                          "vD_dvD", "vD"))
  return(aqol6d_items_tb)
}
add_itm_disv_to_aqol6d_itms_tb <- function (aqol6d_items_tb,
                                            disvalues_lup_tb = NULL,
                                            pfx_1L_chr)
{
  if(is.null(disvalues_lup_tb)){
    utils::data("aqol6d_adult_disv_lup_tb",
                package = "youthvars",
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
add_unwtd_dim_tots <- function (items_tb, domain_items_ls, domain_pfx_1L_chr)
{
  items_and_domains_tb <- purrr::reduce(1:length(domain_items_ls),
                                        .init = items_tb, ~.x %>% dplyr::mutate(`:=`(!!rlang::sym(paste0(domain_pfx_1L_chr,
                                                                                                         names(domain_items_ls)[.y])), rowSums(dplyr::select(.,
                                                                                                                                                             domain_items_ls[[.y]])))))
  return(items_and_domains_tb)
}
add_wtd_dim_tots <- function (unwtd_dim_tb, domain_items_ls, domain_unwtd_pfx_1L_chr,
                              domain_wtd_pfx_1L_chr)
{
  utils::data("aqol6d_adult_disv_lup_tb", package = "youthvars", envir = environment())
  utils::data("aqol6d_domain_qs_lup_tb", package = "youthvars", envir = environment())
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
