add_interval_var <- function(data_tb,
                             id_var_nm_1L_chr = "fkClientID",
                             msrmnt_date_var_nm_1L_chr = "d_interview_date",
                             time_unit_1L_chr = "days",
                             bl_date_var_nm_1L_chr = "bl_date_dtm",
                             interval_var_nm_1L_chr = "interval_dbl",
                             tmp_row_nbr_var_nm_1L_chr = "tmp_row_nbr_int",
                             drop_bl_date_var_1L_lgl = F){
  updated_data_tb <- data_tb %>%
    dplyr::ungroup() %>%
    dplyr::mutate(!!rlang::sym(tmp_row_nbr_var_nm_1L_chr) := 1:dplyr::n()) %>%
    dplyr::group_by(!!rlang::sym(id_var_nm_1L_chr)) %>%
    dplyr::arrange(!!rlang::sym(msrmnt_date_var_nm_1L_chr)) %>%
    dplyr::mutate(!!rlang::sym(bl_date_var_nm_1L_chr) := !!rlang::sym(msrmnt_date_var_nm_1L_chr) %>% dplyr::first()) %>%
    dplyr::mutate(interval_dbl = purrr::map2_dbl(!!rlang::sym(bl_date_var_nm_1L_chr),
                                                 !!rlang::sym(msrmnt_date_var_nm_1L_chr),
                                                 ~ lubridate::interval(.x, .y) %>%
                                                   lubridate::time_length(unit = time_unit_1L_chr))) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(!!rlang::sym(tmp_row_nbr_var_nm_1L_chr)) %>%
    dplyr::select(-!!rlang::sym(tmp_row_nbr_var_nm_1L_chr))
  if(drop_bl_date_var_1L_lgl)
    updated_data_tb <-  updated_data_tb %>%
      dplyr::select(-!!rlang::sym(bl_date_var_nm_1L_chr))
  return(updated_data_tb)
}
add_participation_var <- function(data_tb,
                                  id_var_nm_1L_chr = "fkClientID",
                                  fup_round_nbr_1L_int = 2L,
                                  participation_var_1L_chr = "participation",
                                  timepoint_vals_chr = c("Baseline","Follow-up")){
  data_tb <- data_tb %>%
    dplyr::group_by(!!rlang::sym(id_var_nm_1L_chr)) %>%
    dplyr::mutate(nbr_rounds_int = dplyr::n()) %>%
    dplyr::mutate(!!rlang::sym(participation_var_1L_chr) := ifelse(nbr_rounds_int==1,
                                         paste0(timepoint_vals_chr[1]," only"),
                                         ifelse(nbr_rounds_int==fup_round_nbr_1L_int,
                                                paste0(timepoint_vals_chr[1]," and ",tolower(timepoint_vals_chr[2])),
                                                NA_character_))) %>%

    dplyr::ungroup() %>%
    dplyr::select(-nbr_rounds_int)
  return(data_tb)
}
## DEPRECATED FNS
##
add_aqol6d_adol_dim_scrg_eqs <- function (unscored_aqol_tb,
                                          aqol6d_scrg_dss_ls = NULL)
{
  lifecycle::deprecate_soft("0.0.0.9078", "youthvars::add_aqol6d_adol_dim_scrg_eqs()", "scorz::add_aqol6d_adol_dim_scrg_eqs()")
  if(is.null(aqol6d_scrg_dss_ls)){
    aqol6d_scrg_dss_ls <- get_aqol6d_scrg_dss()
  }
  adol_dim_sclg_eqs_lup <- aqol6d_scrg_dss_ls$adol_dim_sclg_eqs_lup
  for (var in adol_dim_sclg_eqs_lup$Dim_scal) {
    expression = adol_dim_sclg_eqs_lup[adol_dim_sclg_eqs_lup$Dim_scal ==
                                          var, ]$Equ
    unscored_aqol_tb <- unscored_aqol_tb %>% dplyr::mutate(`:=`(!!var,
                                                                !!rlang::parse_expr(expression)))
    Hmisc::label(unscored_aqol_tb[, var]) = adol_dim_sclg_eqs_lup[adol_dim_sclg_eqs_lup$Dim_scal ==
                                                                     var, ]$Label
  }
  return(unscored_aqol_tb)
}
add_aqol6d_items_to_aqol6d_tbs_ls <- function (aqol6d_tbs_ls, aqol_items_prpns_tbs_ls, prefix_chr,
                                               aqol_tots_var_nms_chr,
                                               aqol6d_scrg_dss_ls = NULL,
                                               id_var_nm_1L_chr = "fkClientID", scaling_con_dbl = 5)
{
  lifecycle::deprecate_soft("0.0.0.9078", "youthvars::add_aqol6d_items_to_aqol6d_tbs_ls()", "scorz::add_aqol6d_items_to_aqol6d_tbs_ls()")
  if(is.null(aqol6d_scrg_dss_ls)){
    aqol6d_scrg_dss_ls <- get_aqol6d_scrg_dss()
  }
  updated_aqol6d_tbs_ls <- purrr::map2(aqol6d_tbs_ls, aqol_items_prpns_tbs_ls,
                                       ~{
                                         nbr_obs_1L_int <- nrow(.x) * scaling_con_dbl
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
                                                                                                                 aqol6d_scrg_dss_ls = aqol6d_scrg_dss_ls,
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
                               aqol6d_scrg_dss_ls = NULL,
                               id_var_nm_1L_chr = "fkClientID",
                               prefix_1L_chr = "aqol6d_q",
                               total_aqol_var_nm_1L_chr = "aqol6d_total_c",
                               wtd_aqol_var_nm_1L_chr = "aqol6d_total_w")
{
  lifecycle::deprecate_soft("0.0.0.9078", "youthvars::add_adol6d_scores()", "scorz::add_adol6d_scores()")
  if(is.null(aqol6d_scrg_dss_ls)){
    aqol6d_scrg_dss_ls <- get_aqol6d_scrg_dss()
  }
  complete_ds_tb <- unscored_aqol_tb
  unscored_aqol_tb <- unscored_aqol_tb %>% dplyr::select(tidyselect::all_of(id_var_nm_1L_chr),
                                                         dplyr::starts_with(unname(prefix_1L_chr)))
  old_nms_chr <- names(unscored_aqol_tb)
  names(unscored_aqol_tb) <- c("ID", paste0("Q", 1:20))
  unscored_aqol_tb <- suppressWarnings(impute_unscrd_adol_aqol6d_ds(unscored_aqol_tb))
  disvals_tb <- unscored_aqol_tb %>%
    add_itm_disv_to_aqol6d_itms_tb(disvalues_lup_tb = make_adol_aqol6d_disv_lup(aqol6d_scrg_dss_ls = aqol6d_scrg_dss_ls),
                                   pfx_1L_chr = "Q") %>%
    dplyr::select(ID, dplyr::starts_with("dv_")) %>%
    dplyr::rename_all(~stringr::str_replace(.x, "dv_", "dv"))
  scored_aqol_tb <- add_aqol6d_adol_dim_scrg_eqs(disvals_tb,
                                                 aqol6d_scrg_dss_ls = aqol6d_scrg_dss_ls) %>%
    tibble::as_tibble() %>%
    dplyr::rename(`:=`(!!rlang::sym(id_var_nm_1L_chr), ID),
                  `:=`(!!rlang::sym(wtd_aqol_var_nm_1L_chr), uaqol))
  tbs_ls <- list(complete_ds_tb, scored_aqol_tb) %>%
    purrr::map(~.x %>% dplyr::group_by(!!rlang::sym(id_var_nm_1L_chr)) %>%
                 dplyr::mutate(match_var_chr = paste0(!!rlang::sym(id_var_nm_1L_chr),
                                                      "_",
                                                      1:dplyr::n())) %>%
                 dplyr::ungroup() %>%
                 dplyr::arrange(!!rlang::sym(id_var_nm_1L_chr)))
  if("labelled" %in% class(tbs_ls[[1]][[wtd_aqol_var_nm_1L_chr]])){
    tbs_ls[[2]][[wtd_aqol_var_nm_1L_chr]] <- Hmisc::`label<-`(tbs_ls[[2]][[wtd_aqol_var_nm_1L_chr]],
                                                              value = Hmisc::label(tbs_ls[[1]][[wtd_aqol_var_nm_1L_chr]])
    )
  }else{
    if("labelled" %in% class(tbs_ls[[2]][[wtd_aqol_var_nm_1L_chr]])){
      class(tbs_ls[[2]][[wtd_aqol_var_nm_1L_chr]]) <- setdiff(class(tbs_ls[[2]][[wtd_aqol_var_nm_1L_chr]]), "labelled")
      attr(tbs_ls[[2]][[wtd_aqol_var_nm_1L_chr]], "label") <- NULL
    }
  }
  tfd_aqol_tb <- dplyr::inner_join(tbs_ls[[1]], tbs_ls[[2]]) %>%
    dplyr::select(-match_var_chr) %>%
    dplyr::mutate(!!rlang::sym(total_aqol_var_nm_1L_chr) := rowSums(dplyr::across(dplyr::starts_with(prefix_1L_chr))))
  tfd_aqol_tb <- tfd_aqol_tb %>% dplyr::filter(!is.na(!!rlang::sym(total_aqol_var_nm_1L_chr)))
  return(tfd_aqol_tb)
}
add_aqol6dU_to_aqol6d_items_tb <- function (aqol6d_items_tb,
                                            coefs_lup_tb = NULL)
{
  lifecycle::deprecate_soft("0.0.0.9078", "youthvars::add_aqol6dU_to_aqol6d_items_tb()", "scorz::add_aqol6dU_to_aqol6d_items_tb()")
  if(is.null(coefs_lup_tb)){
    aqol6d_scrg_dss_ls <- get_aqol6d_scrg_dss()
    coefs_lup_tb <- aqol6d_scrg_dss_ls$aqol6d_from_8d_coefs_lup_tb
  }
  coef_dbl <- coefs_lup_tb[match(c(paste0("vD", 1:6), "Constant"),
                                 coefs_lup_tb$var_name_chr), ] %>% dplyr::pull(coef_dbl)
  aqol6d_items_tb <- aqol6d_items_tb %>% dplyr::mutate(aqol6dU = coef_dbl[1] *
                                                         vD1 + coef_dbl[2] * vD2 + coef_dbl[3] * vD3 + coef_dbl[4] *
                                                         vD4 + coef_dbl[5] * vD5 + coef_dbl[6] * vD6 + coef_dbl[7]) %>%
    dplyr::mutate(aqol6dU = aqol6dU %>% purrr::map_dbl(~ifelse(.x >
                                                                 1, 1, .x)))
  return(aqol6d_items_tb)
}
add_aqol6dU_to_aqol6d_tbs_ls <- function (aqol6d_tbs_ls,
                                          aqol6d_scrg_dss_ls = NULL,
                                          prefix_1L_chr = "aqol6d_q",
                                          id_var_nm_1L_chr)
{
  lifecycle::deprecate_soft("0.0.0.9078", "youthvars::add_aqol6dU_to_aqol6d_tbs_ls()", "scorz::add_aqol6dU_to_aqol6d_tbs_ls()")
  if(is.null(aqol6d_scrg_dss_ls)){
    aqol6d_scrg_dss_ls <- get_aqol6d_scrg_dss()
  }
  aqol6d_tbs_ls <- aqol6d_tbs_ls %>% purrr::map(~.x %>% dplyr::mutate(aqol6dU = calculate_adol_aqol6dU(.x,
                                                                                                       aqol6d_scrg_dss_ls = aqol6d_scrg_dss_ls,
                                                                                                       prefix_1L_chr = prefix_1L_chr, id_var_nm_1L_chr = id_var_nm_1L_chr)))
  return(aqol6d_tbs_ls)
}
add_cors_and_utls_to_aqol6d_tbs_ls <- function (aqol6d_tbs_ls, aqol_scores_pars_ls, aqol_items_prpns_tbs_ls,
                                                temporal_cors_ls, prefix_chr, aqol_tots_var_nms_chr,
                                                aqol6d_scrg_dss_ls = NULL,
                                                id_var_nm_1L_chr = "fkClientID")
{
  lifecycle::deprecate_soft("0.0.0.9078", "youthvars::add_cors_and_utls_to_aqol6d_tbs_ls()", "scorz::add_cors_and_utls_to_aqol6d_tbs_ls()")
  if(is.null(aqol6d_scrg_dss_ls)){
    aqol6d_scrg_dss_ls <- get_aqol6d_scrg_dss()
  }
  aqol6d_tbs_ls <- reorder_tbs_for_target_cors(aqol6d_tbs_ls,
                                               cor_dbl = temporal_cors_ls[[1]], cor_var_chr = rep(names(temporal_cors_ls)[1],
                                                                                                  2), id_var_to_rmv_1L_chr = "id") %>% add_uids_to_tbs_ls(prefix_1L_chr = prefix_chr[["uid"]],
                                                                                                                                                          id_var_nm_1L_chr = id_var_nm_1L_chr)
  aqol6d_tbs_ls <- aqol6d_tbs_ls %>% add_aqol6d_items_to_aqol6d_tbs_ls(aqol_items_prpns_tbs_ls = aqol_items_prpns_tbs_ls,
                                                                       aqol6d_scrg_dss_ls = aqol6d_scrg_dss_ls,
                                                                       prefix_chr = prefix_chr, aqol_tots_var_nms_chr = aqol_tots_var_nms_chr,
                                                                       id_var_nm_1L_chr = id_var_nm_1L_chr)
  return(aqol6d_tbs_ls)
}
add_dim_disv_to_aqol6d_items_tb <- function (aqol6d_items_tb, domain_items_ls, domains_chr, dim_sclg_con_lup_tb = NULL,
                                             itm_wrst_wts_lup_tb = NULL)
{
  lifecycle::deprecate_soft("0.0.0.9078", "youthvars::add_dim_disv_to_aqol6d_items_tb()", "scorz::add_dim_disv_to_aqol6d_items_tb()")
  if(is.null(dim_sclg_con_lup_tb) | is.null(itm_wrst_wts_lup_tb))
    aqol6d_scrg_dss_ls <- get_aqol6d_scrg_dss()
  if(is.null(dim_sclg_con_lup_tb)){
    dim_sclg_con_lup_tb <- aqol6d_scrg_dss_ls$aqol6d_dim_sclg_con_lup_tb
  }
  if(is.null(itm_wrst_wts_lup_tb)){
    itm_wrst_wts_lup_tb <- aqol6d_scrg_dss_ls$aqol6d_adult_itm_wrst_wts_lup_tb
  }
  aqol6d_disu_fn_ls <- make_aqol6d_fns_ls(domain_items_ls)
  kD_dbl <- make_dim_sclg_cons_dbl(domains_chr = domains_chr,
                                   dim_sclg_con_lup_tb = dim_sclg_con_lup_tb)
  w_dbl_ls <- make_make_item_wrst_wts_ls_ls(domain_items_ls = domain_items_ls,
                                            itm_wrst_wts_lup_tb = itm_wrst_wts_lup_tb)
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
  lifecycle::deprecate_soft("0.0.0.9078", "youthvars::add_dim_scores_to_aqol6d_items_tb()", "scorz::add_dim_scores_to_aqol6d_items_tb()")
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
  lifecycle::deprecate_soft("0.0.0.9078", "youthvars::add_itm_disv_to_aqol6d_itms_tb()", "scorz::add_itm_disv_to_aqol6d_itms_tb()")
  if(is.null(disvalues_lup_tb)){
    aqol6d_scrg_dss_ls <- get_aqol6d_scrg_dss()
    disvalues_lup_tb <- aqol6d_scrg_dss_ls$aqol6d_adult_disv_lup_tb
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
add_labels_to_aqol6d_tb <- function (aqol6d_tb, labels_chr = NA_character_)
{
  lifecycle::deprecate_soft("0.0.0.9078", "youthvars::add_labels_to_aqol6d_tb()", "scorz::add_labels_to_aqol6d_tb()")
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
add_unwtd_dim_tots <- function (items_tb, domain_items_ls, domain_pfx_1L_chr)
{
  lifecycle::deprecate_soft("0.0.0.9078", "youthvars::add_unwtd_dim_tots()", "scorz::add_unwtd_dim_tots()")
  items_and_domains_tb <- purrr::reduce(1:length(domain_items_ls),
                                        .init = items_tb, ~.x %>% dplyr::mutate(`:=`(!!rlang::sym(paste0(domain_pfx_1L_chr,
                                                                                                         names(domain_items_ls)[.y])), rowSums(dplyr::select(.,
                                                                                                                                                             domain_items_ls[[.y]])))))
  return(items_and_domains_tb)
}
add_wtd_dim_tots <- function (unwtd_dim_tb, domain_items_ls, domain_unwtd_pfx_1L_chr,
                              domain_wtd_pfx_1L_chr, aqol6d_scrg_dss_ls = NULL)
{
  lifecycle::deprecate_soft("0.0.0.9078", "youthvars::add_wtd_dim_tots()", "scorz::add_wtd_dim_tots()")
  if(is.null(aqol6d_scrg_dss_ls))
    aqol6d_scrg_dss_ls <- get_aqol6d_scrg_dss()
  aqol6d_adult_disv_lup_tb <- aqol6d_scrg_dss_ls$aqol6d_adult_disv_lup_tb
  aqol6d_domain_qs_lup_tb <- aqol6d_scrg_dss_ls$aqol6d_domain_qs_lup_tb
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
