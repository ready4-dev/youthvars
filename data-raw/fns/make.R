make_complete_prpns_tbs_ls <- function (raw_prpns_tbs_ls, question_var_nm_1L_chr = "Question")
{# UNUSED: MIGRATE OUT
  complete_prpns_tbs_ls <- raw_prpns_tbs_ls %>% purrr::map(~{
    .x %>% dplyr::mutate(total_prop_dbl = rowSums(dplyr::select(.,
                                                                -!!rlang::sym(question_var_nm_1L_chr)), na.rm = T) -
                           100) %>% dplyr::mutate_if(is.numeric, ~purrr::map2_dbl(.,
                                                                                  total_prop_dbl, ~ifelse(.x == 100, 1 - .y, .x))) %>%
      dplyr::select(-total_prop_dbl)
  })
  return(complete_prpns_tbs_ls)
}
make_correlated_data_tb <- function (synth_data_spine_ls, synth_data_idx_1L_dbl = 1)
{ # MIGRATED FROM TTU
  correlated_data_tb <- simstudy::genCorData(synth_data_spine_ls$nbr_obs_dbl[synth_data_idx_1L_dbl],
                                             mu = synth_data_spine_ls$means_ls[[synth_data_idx_1L_dbl]],
                                             sigma = synth_data_spine_ls$sds_ls[[synth_data_idx_1L_dbl]],
                                             corMatrix = make_pdef_cor_mat_mat(synth_data_spine_ls$cor_mat_ls[[synth_data_idx_1L_dbl]]),
                                             cnames = synth_data_spine_ls$var_names_chr) %>% force_min_max_and_int_cnstrs(var_names_chr = synth_data_spine_ls$var_names_chr,
                                                                                                                          min_max_ls = synth_data_spine_ls$min_max_ls, discrete_lgl = synth_data_spine_ls$discrete_lgl)
  return(correlated_data_tb)
}
make_cors_with_utl_tbl <- function(data_tb,
                                   ds_descvs_ls,
                                   dictionary_tb = NULL,
                                   cor_type_1L_chr = "pearson"){
  cors_with_utl_tb <- purrr::map(ds_descvs_ls$round_vals_chr, ~data_tb %>% dplyr::filter(!!rlang::sym(ds_descvs_ls$round_var_nm_1L_chr) ==
                                                                                           .x) %>% dplyr::select(!!!rlang::syms(c(ds_descvs_ls$utl_wtd_var_nm_1L_chr,ds_descvs_ls$candidate_predrs_chr))) %>% as.matrix() %>% Hmisc::rcorr(type = cor_type_1L_chr)) %>%
    purrr::map2_dfc(ds_descvs_ls$round_vals_chr,
                    ~tibble::tibble(!!rlang::sym(paste0(.y,"_cor_dbl")) := .x[[1]][2:(length(ds_descvs_ls$candidate_predrs_chr)+1)],
                                    !!rlang::sym(paste0(.y,"_sig_dbl")) := .x[[3]][2:(length(ds_descvs_ls$candidate_predrs_chr)+1)])) %>%
    dplyr::mutate(variable_chr = ds_descvs_ls$candidate_predrs_chr) %>%
    dplyr::select(variable_chr, dplyr::everything())
  if(!is.null(dictionary_tb)){
    cors_with_utl_tb <- cors_with_utl_tb %>%
      dplyr::mutate(variable_chr = variable_chr %>% purrr::map_chr(~ready4::get_from_lup_obj(dictionary_tb,
                                                                                        target_var_nm_1L_chr = "var_desc_chr",
                                                                                        match_var_nm_1L_chr = "var_nm_chr",
                                                                                        match_value_xx = .x,
                                                                                        evaluate_1L_lgl = F)))

  }
  return(cors_with_utl_tb)
}
make_corstars_tbl_xx <- function (x,
                                  caption_1L_chr = NULL,
                                  mkdn_tbl_ref_1L_chr = NULL,
                                  method_chr = c("pearson", "spearman"),
                                  removeTriangle_chr = c("upper",
                                                         "lower"),
                                  result_chr = "none")
{
  x <- as.matrix(x)
  correlation_matrix <- Hmisc::rcorr(x, type = method_chr[1])
  R <- correlation_matrix$r
  p <- correlation_matrix$P
  mystars <- ifelse(p < 1e-04, "****", ifelse(p < 0.001, "*** ",
                                              ifelse(p < 0.01, "**  ", ifelse(p < 0.05, "*   ", "    "))))
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[, -1]
  Rnew <- matrix(paste(R, mystars, sep = ""), ncol = ncol(x))
  diag(Rnew) <- paste(diag(R), " ", sep = "")
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep = "")
  if (removeTriangle_chr[1] == "upper") {
    Rnew <- as.matrix(Rnew)
    Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }else{
    if (removeTriangle_chr[1] == "lower") {
      Rnew <- as.matrix(Rnew)
      Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
      Rnew <- as.data.frame(Rnew)
    }
  }
  Rnew <- cbind(Rnew[1:length(Rnew) - 1])
  stars_chr <- mystars %>%
    as.vector() %>%
    unique() %>%
    purrr::discard(is.na) %>%
    stringr::str_trim()
  stars_chr <- stars_chr[order(stars_chr, purrr::map_int(stars_chr, ~ nchar(.x)))]
  footnotes_chr <- stars_chr %>% purrr::map_chr(~{
    paste0(.x, " p<", switch(nchar(.x), "0.05","0.01","0.001","0.0001"))
  })
  footnotes_chr <- paste0("Note: ",footnotes_chr %>% paste0(collapse = ", ") %>% stringi::stri_replace_last(fixed = ",", " and"))
  if (result_chr[1] == "none")
    return(Rnew)
  else {
    if(is.null(caption_1L_chr))
      caption_1L_chr <- knitr::opts_current$get("tab.cap")
    if(is.null(mkdn_tbl_ref_1L_chr))
      mkdn_tbl_ref_1L_chr <- paste0("tab:",
                                    knitr::opts_current$get("tab.id"))

    add_to_row_ls <- list()
    add_to_row_ls$pos <- list(nrow(Rnew))
    add_to_row_ls$command <- c(paste0("\\hline\n","{\\footnotesize ",
                                      footnotes_chr,
                                      "}\n"))
    Rnew %>%
      ready4show::print_table(output_type_1L_chr = result_chr[1],
                              add_to_row_ls = add_to_row_ls,
                              caption_1L_chr = caption_1L_chr,
                              footnotes_chr = footnotes_chr,
                              inc_col_nms_1L_lgl = T,
                              inc_row_nms_1L_lgl = T,
                              mkdn_tbl_ref_1L_chr = mkdn_tbl_ref_1L_chr,
                              use_rdocx_1L_lgl = ifelse(result_chr[1] == "Word",
                                                        T,
                                                        F))
  }
}
make_descv_stats_tbl <- function(data_tb,
                                 key_var_nm_1L_chr = "round",
                                 key_var_vals_chr = NULL,
                                 variable_nms_chr,
                                 dictionary_tb = NULL,
                                 test_1L_lgl = F,
                                 sections_as_row_1L_lgl = F,
                                 nbr_of_digits_1L_int = NA_integer_){
  if(is.null(key_var_vals_chr)){
    key_var_vals_chr <- data_tb %>% dplyr::pull(key_var_nm_1L_chr) %>% unique() %>% as.character()
  }
  if(length(key_var_vals_chr)<2 & test_1L_lgl){
    descv_stats_tbl_tb <- NULL
  }else{
    descv_stats_tbl_tb <- make_tableby_ls(data_tb,
                                          key_var_nm_1L_chr = key_var_nm_1L_chr,
                                          variable_nms_chr = variable_nms_chr,
                                          test_1L_lgl = test_1L_lgl) %>%
      as.data.frame() %>%
      dplyr::select(c("variable","label",
                      tidyselect::all_of(key_var_vals_chr),
                      ifelse(test_1L_lgl,
                             "p.value",
                             character(0)))
                    %>% purrr::discard(is.na))
    if(!is.null(dictionary_tb)){
      descv_stats_tbl_tb <- descv_stats_tbl_tb %>%
        dplyr::mutate(variable = variable %>% purrr::map_chr(~ready4::get_from_lup_obj(dictionary_tb,
                                                                                          target_var_nm_1L_chr = "var_desc_chr",
                                                                                          match_var_nm_1L_chr = "var_nm_chr",
                                                                                          match_value_xx = .x,
                                                                                          evaluate_1L_lgl = F) %>% as.vector()))

    }
    vars_with_mdns_chr <- descv_stats_tbl_tb %>% dplyr::filter(label == "Median (Q1, Q3)") %>% dplyr::pull(variable)
    descv_stats_tbl_tb <- descv_stats_tbl_tb %>%
      dplyr::mutate(dplyr::across(tidyselect::all_of(key_var_vals_chr),
                                  ~ list(.x) %>% purrr::pmap_dbl(~{
                                    ifelse(..1[[1]][[1]] =="",
                                           NA_real_,
                                           ..1[[1]][[1]])
                                  }),
                                  .names = "{col}_val_1_dbl"),
                    dplyr::across(tidyselect::all_of(key_var_vals_chr),
                                  ~ list(.x,variable,label) %>%
                                    purrr::pmap(~ {
                                      if(..2 %in% vars_with_mdns_chr){
                                        if(..3 == "Median (Q1, Q3)"){
                                          return_dbl <- c(..1[[2]],..1[[3]])
                                        }else{
                                          return_dbl <- ifelse(length(..1) == 1,
                                                               NA_real_,
                                                               ..1[[2]])
                                        }
                                      }else{
                                        return_dbl <- ifelse(length(..1) == 1,
                                                             NA_real_,
                                                             ifelse(..1[[2]]=="",
                                                                    NA_real_,
                                                                    ..1[[2]]))
                                      }
                                    }
                                    ),
                                  .names = "{col}_val_2_ls")) %>%
      dplyr::select(variable,
                    label,
                    key_var_vals_chr %>% purrr::map(~c(paste0(.x, c("_val_1_dbl","_val_2_ls")))) %>%
                      purrr::flatten_chr(),
                    ifelse(test_1L_lgl,"p.value",character(0)) %>% purrr::discard(is.na)
      )
    if(sections_as_row_1L_lgl){
      descv_stats_tbl_tb <- descv_stats_tbl_tb %>%
        dplyr::select(-variable)
    }else{
      descv_stats_tbl_tb <- descv_stats_tbl_tb %>%
        dplyr::filter(label != variable)
    }
    if(!is.na(nbr_of_digits_1L_int)){
      descv_stats_tbl_tb <- c(key_var_vals_chr %>%
                                purrr::map(~c(paste0(.x, c("_val_1_dbl","_val_2_ls"
                                )))) %>%
                                purrr::flatten_chr(),
                              ifelse(test_1L_lgl,"p.value",character(0)) %>% purrr::discard(is.na)) %>%
        purrr::reduce(.init = descv_stats_tbl_tb,
                      ~ .x %>% dplyr::mutate(!!rlang::sym(.y) := !!rlang::sym(.y) %>%
                                               purrr::map_chr(~ {
                                                 ifelse(length(.x) == 1,
                                                        ifelse(is.na(.x),
                                                               "",
                                                               paste0("",
                                                                      format(round(.x, nbr_of_digits_1L_int),
                                                                             nsmall = nbr_of_digits_1L_int),
                                                                      ""
                                                               )
                                                        ),
                                                        paste0("",
                                                               .x %>%
                                                                 purrr::map_chr(~format(round(.x,
                                                                                              nbr_of_digits_1L_int),
                                                                                        nsmall = nbr_of_digits_1L_int)) %>%
                                                                 paste0(collapse = ", "),
                                                               ""
                                                        )
                                                 )
                                               })))
      descv_stats_tbl_tb <- paste0(key_var_vals_chr, "_val_2_ls") %>%
        purrr::reduce(.init = descv_stats_tbl_tb,
                      ~ .x %>%
                        dplyr::mutate(!!rlang::sym(.y) := !!rlang::sym(.y) %>%
                                        purrr::map2_chr(label,
                                                        ~ ifelse(.x=="" | .y== "Min - Max",
                                                                 .x,
                                                                 paste0("(",
                                                                        .x,
                                                                        ifelse(.y %in% c("Mean (SD)","Median (Q1, Q3)","Missing"),
                                                                               "",
                                                                               "%"),
                                                                        ")")))

                        ))
    }
  }
  return(descv_stats_tbl_tb)
}
make_final_repln_ds_dict <- function(seed_dictionary_tb = NULL,
                                    additions_tb = NULL,
                                    utl_unwtd_var_nm_1L_chr = "aqol6d_total_c"){
  if(is.null(seed_dictionary_tb)){
    utils::data("aqol_scrg_dict_r3", package = "youthvars", envir = environment())
    dictionary_tb <- ready4::renew(make_tfd_repln_ds_dict_r3(),
                                          new_ready4_dict_r3 = aqol_scrg_dict_r3)
  }else{
    dictionary_tb <- seed_dictionary_tb
  }
  if(is.null(additions_tb)){
    additions_tb <- ready4use::make_pt_ready4use_dictionary(var_nm_chr = c(#utl_unwtd_var_nm_1L_chr,
                                                                        "bl_date_dtm",
                                                                        "interval_dbl",
                                                                        "participation"),
                                                         var_ctg_chr = c(#"MAUI",
                                                                         "Temporal", "Temporal", "Temporal"),
                                                         var_desc_chr = c(#"AQOL-6D Total Score",
                                                                          "Date of baseline assessment",
                                                                          "Interval between baseline and follow-up assessments", "Rounds participated in"),
                                                         var_type_chr = c(#"numeric",
                                                                          "date","interval", "character")) %>%
      ready4use::ready4use_dictionary()
  }
  Hmisc::label(additions_tb) <- as.list(Hmisc::label(dictionary_tb) %>% unname())
  dictionary_tb <- dictionary_tb %>%
    ready4::renew(new_ready4_dict_r3 = additions_tb)
  return(dictionary_tb)
}
make_formula <- function(depnt_var_nm_1L_chr,
                         predictors_chr,
                         environment_env = parent.frame()){
  formula_fml <- stats::formula(paste0(depnt_var_nm_1L_chr,
                                       " ~ ",
                                       paste0(predictors_chr, collapse = " + ")), env = environment_env)
  return(formula_fml)
}
make_item_plt <- function(tfd_data_tb,
                          var_nm_1L_chr,
                          round_var_nm_1L_chr = "round",
                          x_label_1L_chr,
                          y_label_1L_chr = "Percentage",
                          fill_label_1L_chr = "Data collection",
                          y_scale_scl_fn = NULL,
                          use_bw_theme_1L_lgl = F,
                          legend_position_1L_chr = "none"){
  item_plt <- ggplot2::ggplot(tfd_data_tb,
                              ggplot2::aes_string(var_nm_1L_chr)) +
    ggplot2::geom_bar(ggplot2::aes(y = y,
                                   fill = !!rlang::sym(round_var_nm_1L_chr)),
                      stat = "identity",
                      na.rm = TRUE,
                      position = "dodge",
                      colour = "white",
                      alpha= 0.7)
  if(!is.null(y_scale_scl_fn)){
    item_plt <- item_plt +
      ggplot2::scale_y_continuous(labels = y_scale_scl_fn)
  }
  item_plt <- item_plt +
    ggplot2::labs(x = x_label_1L_chr,
                  y = y_label_1L_chr,
                  fill = fill_label_1L_chr)
  if(use_bw_theme_1L_lgl){
    item_plt <- item_plt +
      ggplot2::theme_bw()
  }
  item_plt <- item_plt +
    ggplot2::theme(legend.position = legend_position_1L_chr) +
    ggplot2::scale_fill_manual(values = c("#de2d26","#fc9272"))
  return(item_plt)
}
make_itm_resp_plts <- function(data_tb,
                               col_nms_chr,
                               lbl_nms_chr,
                               plot_rows_cols_pair_int,
                               heights_int,
                               round_var_nm_1L_chr = "round",
                               y_label_1L_chr = "Percentage"){
  plots_ls <- list()
  j=1
  for(i in col_nms_chr){
    tfd_data_tb <- data_tb %>%
      transform_ds_for_item_plt(var_nm_1L_chr = i,
                                round_var_nm_1L_chr = round_var_nm_1L_chr)
    labelx <- lbl_nms_chr[j]
    j = j+1
    plots_ls[[i]]<- make_item_plt(tfd_data_tb,
                                  var_nm_1L_chr = i,
                                  round_var_nm_1L_chr = round_var_nm_1L_chr,
                                  x_label_1L_chr = labelx,
                                  y_label_1L_chr = y_label_1L_chr,
                                  y_scale_scl_fn = scales::percent_format(),
                                  use_bw_theme_1L_lgl = T,
                                  legend_position_1L_chr = "none")
  }
  plot_plt <- make_item_plt(tfd_data_tb,
                            var_nm_1L_chr = i,
                            round_var_nm_1L_chr = round_var_nm_1L_chr,
                            x_label_1L_chr = labelx,
                            y_label_1L_chr = y_label_1L_chr,
                            y_scale_scl_fn = NULL,
                            use_bw_theme_1L_lgl = F,
                            legend_position_1L_chr = "bottom")
  legend_ls <- get_guide_box_lgd(plot_plt)
  composite_plt <- gridExtra::grid.arrange(ggpubr::ggarrange(plotlist=plots_ls,
                                                             nrow = plot_rows_cols_pair_int[1],
                                                             ncol = plot_rows_cols_pair_int[2]),
                                           legend_ls,
                                           nrow = length(heights_int),
                                           heights = heights_int)
  return(composite_plt)
}
make_make_item_wrst_wts_ls_ls <- function (domain_items_ls, itm_wrst_wts_lup_tb)
{ # MIGRATED FROM TTU: REORGANISE
  make_item_wrst_wts_ls_ls <- domain_items_ls %>% purrr::map(~{
    purrr::map_dbl(.x, ~{
      ready4::get_from_lup_obj(itm_wrst_wts_lup_tb,
                                  match_var_nm_1L_chr = "Question_chr", match_value_xx = .x,
                                  target_var_nm_1L_chr = "Worst_Weight_dbl", evaluate_1L_lgl = F)
    })
  })
  return(make_item_wrst_wts_ls_ls)
}

make_pdef_cor_mat_mat <- function (lower_diag_mat)
{ # MIGRATED FROM TTU: REORGANISE
  pdef_cor_mat <- lower_diag_mat %>% Matrix::forceSymmetric(uplo = "L") %>%
    as.matrix()
  if (!matrixcalc::is.positive.definite(pdef_cor_mat)) {
    pdef_cor_mat <- psych::cor.smooth(pdef_cor_mat)
  }
  return(pdef_cor_mat)
}
make_predr_pars_and_cors_tbl <- function(data_tb,
                                         ds_descvs_ls,
                                         descv_tbl_ls,
                                         dictionary_tb,
                                         nbr_of_digits_1L_int = 2L,
                                         predictors_lup = NULL){
  predr_pars_and_cors_tb <- make_cors_with_utl_tbl(data_tb,
                                                   ds_descvs_ls = ds_descvs_ls,
                                                   dictionary_tb = dictionary_tb) %>%
    dplyr::mutate(label = paste0("Correlation with ",
                                 ready4::get_from_lup_obj(dictionary_tb,
                                                             match_var_nm_1L_chr = "var_nm_chr",
                                                             match_value_xx = ds_descvs_ls$utl_wtd_var_nm_1L_chr,
                                                             target_var_nm_1L_chr = "var_desc_chr",
                                                             evaluate_1L_lgl = F)))

  predr_pars_and_cors_tb <- purrr::map_dfr(1:nrow(predr_pars_and_cors_tb),
                                           ~ predr_pars_and_cors_tb %>%
                                             dplyr::slice(.x) %>%
                                             dplyr::mutate(dplyr::across(paste0(ds_descvs_ls$round_vals_chr,"_sig_dbl"),
                                                                         ~ format(round(.x, nbr_of_digits_1L_int),
                                                                                  nsmall = nbr_of_digits_1L_int))) %>%

                                             dplyr::mutate(p.value = paste0(c( !!!rlang::syms(paste0(ds_descvs_ls$round_vals_chr,"_sig_dbl"))),
                                                                            collapse = ", "))) %>%
    dplyr::mutate(dplyr::across(paste0(ds_descvs_ls$round_vals_chr,"_sig_dbl"), ~ "")) %>%
    dplyr::mutate(dplyr::across(paste0(ds_descvs_ls$round_vals_chr,"_cor_dbl"),
                                ~ format(round(.x, nbr_of_digits_1L_int),
                                         nsmall = nbr_of_digits_1L_int))) %>%
    dplyr::rename_with(~stringr::str_replace(.x,"_cor_dbl","_val_1_chr") %>%
                         stringr::str_replace("_sig_dbl","_val_2_chr")) %>%
    dplyr::select(variable_chr,
                  label,
                  dplyr::everything())


  main_outc_tbl_tb <- descv_tbl_ls$main_outc_tbl_tb %>%
    dplyr::filter(label %in% c("Mean (SD)", "Missing")) %>%
    dplyr::rename_with(~stringr::str_replace(.x,"_val_1_dbl","_val_1_chr") %>%
                         stringr::str_replace("_val_2_ls","_val_2_chr") %>%
                         stringr::str_replace("variable", "variable_chr")) %>%
    dplyr::filter(variable_chr %in% purrr::map_chr(ds_descvs_ls$candidate_predrs_chr,
                                                   ~ready4::get_from_lup_obj(dictionary_tb,
                                                                                match_var_nm_1L_chr = "var_nm_chr",
                                                                                match_value_xx = .x,
                                                                                target_var_nm_1L_chr = "var_desc_chr",
                                                                                evaluate_1L_lgl = F)))
  predr_pars_and_cors_tb <- main_outc_tbl_tb$variable_chr %>% unique() %>%
    purrr::map_dfr(~tibble::add_case(main_outc_tbl_tb %>%
                                       dplyr::filter(variable_chr == .x),
                                     predr_pars_and_cors_tb %>%
                                       dplyr::filter(variable_chr == .x)))
  if(!is.null(predictors_lup)){
    predr_pars_and_cors_tb <- predr_pars_and_cors_tb %>%
      dplyr::mutate(variable_chr = purrr::map_chr(variable_chr,
                                                  ~ {
                                                    var_nm_1L_chr <- ready4::get_from_lup_obj(dictionary_tb,
                                                                                                 match_var_nm_1L_chr = "var_desc_chr",
                                                                                                 match_value_xx = .x,
                                                                                                 target_var_nm_1L_chr = "var_nm_chr",
                                                                                                 evaluate_1L_lgl = F)
                                                    paste0 (.x,
                                                            " (",
                                                            ready4::get_from_lup_obj(predictors_lup,
                                                                                        match_var_nm_1L_chr = "short_name_chr",
                                                                                        match_value_xx = var_nm_1L_chr,
                                                                                        target_var_nm_1L_chr = "min_val_dbl",
                                                                                        evaluate_1L_lgl = F),
                                                            "-",
                                                            ready4::get_from_lup_obj(predictors_lup,
                                                                                        match_var_nm_1L_chr = "short_name_chr",
                                                                                        match_value_xx = var_nm_1L_chr,
                                                                                        target_var_nm_1L_chr = "max_val_dbl",
                                                                                        evaluate_1L_lgl = F),
                                                            ")"
                                                    )
                                                  }
      ))
  }
  return(predr_pars_and_cors_tb)
}
make_subtotal_plt <- function(data_tb,
                              var_nm_1L_chr,
                              round_var_nm_1L_chr = "round",
                              x_label_1L_chr,
                              y_label_1L_chr = "Percentage",
                              y_scale_scl_fn = scales::percent,
                              use_bw_theme_1L_lgl = T,
                              legend_position_1L_chr = "none",
                              label_fill_1L_chr = NULL){
  subtotal_plt <- ggplot2::ggplot(data_tb,
                                  ggplot2::aes_string(var_nm_1L_chr)) +
    ggplot2::geom_histogram(bins=8,
                            color = "white",
                            ggplot2::aes(fill = !!rlang::sym(round_var_nm_1L_chr),
                                         y = 2*(..density..)/sum(..density..)),
                            position = 'dodge',
                            alpha=0.7)
  subtotal_plt <- subtotal_plt +
    ggplot2::labs(x = x_label_1L_chr,
                  y = y_label_1L_chr,
                  fill = label_fill_1L_chr)
  if(use_bw_theme_1L_lgl){
    subtotal_plt <- subtotal_plt +
      ggplot2::theme_bw()
  }
  if(!is.null(y_scale_scl_fn)){
    subtotal_plt <- subtotal_plt +
      ggplot2::scale_y_continuous(labels = y_scale_scl_fn)
  }
  subtotal_plt <- subtotal_plt +
    ggplot2::theme(legend.position = legend_position_1L_chr) +
    ggplot2::scale_fill_manual(values = c("#de2d26","#fc9272"))
  return(subtotal_plt)
}
make_sub_tot_plts <- function(data_tb,
                              col_nms_chr,
                              plot_rows_cols_pair_int,
                              round_var_nm_1L_chr = "round",
                              make_log_log_tfmn_1L_lgl = F,
                              heights_int,
                              y_label_1L_chr = "Percentage"){
  if(!is.null(col_nms_chr)){
    plots_ls<-list()
    for(i in col_nms_chr){
      if(make_log_log_tfmn_1L_lgl){
        targetvar = paste0("tran_",i)
        data_tb <- dplyr::mutate(data_tb, !!targetvar := log(-log(1-!!as.name(i))))  %>%
          dplyr::mutate(!!targetvar :=ifelse(!!as.name(i)==1,log(-log(1-0.999)),!!as.name(targetvar)))
      }
      labelx <- eval(parse(text=paste0("attributes(data_tb$",i,")$label")))
      labelx <- stringr::str_sub(labelx,
                                 start = stringi::stri_locate_last_fixed(labelx," - ")[1,1] %>%
                                   unname() + 2)
      if(make_log_log_tfmn_1L_lgl){
        labelx<- paste0("log-log transformed ", labelx)
      }
      plots_ls[[i]]<- make_subtotal_plt(data_tb,
                                        round_var_nm_1L_chr = round_var_nm_1L_chr,
                                        var_nm_1L_chr = i,
                                        x_label_1L_chr = labelx,
                                        y_label_1L_chr = y_label_1L_chr)
    }
    plot_for_lgd_plt <- make_subtotal_plt(data_tb,
                                          round_var_nm_1L_chr = round_var_nm_1L_chr,
                                          var_nm_1L_chr = i,
                                          x_label_1L_chr = labelx,
                                          legend_position_1L_chr = "bottom",
                                          label_fill_1L_chr = "Data collection",
                                          y_label_1L_chr = y_label_1L_chr)
    legend_ls <- get_guide_box_lgd(plot_for_lgd_plt)
    composite_plt <- gridExtra::grid.arrange(ggpubr::ggarrange(plotlist=plots_ls,
                                                               nrow = plot_rows_cols_pair_int[1],
                                                               ncol = plot_rows_cols_pair_int[2]),
                                             legend_ls,
                                             nrow = length(heights_int),
                                             heights = heights_int)

  }else{
    composite_plt <- NULL
  }
  return(composite_plt)
}
make_synth_series_tbs_ls <- function (synth_data_spine_ls, series_names_chr)
{ # MIGRATED FROM TTU: REORGANISE
  synth_series_tbs_ls <- 1:length(series_names_chr) %>% purrr::map(~make_correlated_data_tb(synth_data_spine_ls = synth_data_spine_ls,
                                                                                            synth_data_idx_1L_dbl = .x) %>% replace_with_missing_vals(synth_data_spine_ls = synth_data_spine_ls,
                                                                                                                                                      idx_int = .x)) %>% stats::setNames(series_names_chr)
  return(synth_series_tbs_ls)
}
make_tableby_cntrls <- function(test_1L_lgl = F){
  tableby_cntrls_ls <- arsenal::tableby.control(
    test = test_1L_lgl,
    total = F,
    digits=1,
    digits.pct=1,
    digits.p=3,
    numeric.test = "anova",
    cat.test = "chisq",
    numeric.stats = c("meansd",
                      "medianq1q3",
                      "range",
                      "Nmiss2"),
    cat.stats = c("countpct", "Nmiss2"),
    ordered.stats=c("countpct", "Nmiss2"),
    stats.labels = list(meansd = "Mean (SD)",
                        medianq1q3 = "Median (Q1, Q3)",
                        range = "Min - Max",
                        Nmiss2 = "Missing"))
  return(tableby_cntrls_ls)
}
make_tableby_ls <- function(data_tb,
                            key_var_nm_1L_chr,
                            variable_nms_chr,
                            test_1L_lgl = F){
  forumla_fml <- make_formula(key_var_nm_1L_chr,
                              predictors_chr = variable_nms_chr)
  tableby_ls <- arsenal::tableby(forumla_fml,
                                 data = data_tb,
                                 control = make_tableby_cntrls(test_1L_lgl))
  return(tableby_ls)
}
make_tfd_repln_ds_dict_r3 <- function(repln_ds_dict_r3 = NULL){
  if(is.null(repln_ds_dict_r3)){
    data("repln_ds_dict_r3", package =  "youthvars", envir = environment())
  }
  tfd_repln_ds_dict_r3 <- repln_ds_dict_r3 %>%
    dplyr::mutate(dplyr::across(.fns = as.character)) %>%
    dplyr::mutate(var_nm_chr = dplyr::case_when(var_nm_chr == "phq9_total" ~ "PHQ9",
                                                var_nm_chr == "bads_total" ~ "BADS",
                                                var_nm_chr == "gad7_total" ~ "GAD7",
                                                var_nm_chr == "oasis_total" ~ "OASIS",
                                                var_nm_chr == "scared_total" ~ "SCARED",
                                                var_nm_chr == "k6_total" ~ "K6",
                                                var_nm_chr == "c_sofas" ~ "SOFAS",
                                                T ~ var_nm_chr))
  Hmisc::label(tfd_repln_ds_dict_r3) = as.list(c("Variable","Category", "Description", "Class"))
  return(tfd_repln_ds_dict_r3)
}
make_var_by_round_plt <- function(data_tb,
                                  var_nm_1L_chr,
                                  round_var_nm_1L_chr = "round",
                                  x_label_1L_chr,
                                  y_label_1L_chr = "Percentage",
                                  y_scale_scl_fn = scales::percent,
                                  label_fill_1L_chr = "Data collection"){
  var_by_round_plt <- ggplot2::ggplot(data_tb,
                                      ggplot2::aes(x=!!rlang::sym(var_nm_1L_chr),
                                                   fill=!!rlang::sym(round_var_nm_1L_chr))) +
    ggplot2::theme_bw() +
    ggplot2::geom_histogram(ggplot2::aes(y = stat(width*density)),
                            bins = 10,
                            position="dodge",
                            colour="white",
                            alpha=0.7)

  if(!is.null(y_scale_scl_fn)){
    var_by_round_plt <- var_by_round_plt +
      ggplot2::scale_y_continuous(labels = y_scale_scl_fn)
  }
  var_by_round_plt <- var_by_round_plt +
    #ggplot2::scale_y_continuous(labels = scales::percent_format()) +
    ggplot2::labs(y = y_label_1L_chr, x= x_label_1L_chr, fill = label_fill_1L_chr)  +
    ggplot2::scale_fill_manual(values=c("#de2d26","#fc9272"))  +
    ggplot2::theme(legend.position="bottom")
  return(var_by_round_plt)
}
# make_vec_with_sum_of_int_val <- function (target_int, start_int, end_int, length_int)
# { # MIGRATED FROM TTU: REORGANISE
#   vec_int <- Surrogate::RandVec(a = start_int, b = end_int,
#                                 s = target_int, n = length_int, m = 1) %>% purrr::pluck("RandVecOutput") %>%
#     as.vector() %>% round() %>% as.integer() %>% force_vec_to_sum_to_int(target_1L_int = target_int)
#   return(vec_int)
# }
## DEPRECATED FNS
make_adol_aqol6d_disv_lup <- function (aqol6d_scrg_dss_ls = NULL)
{
  lifecycle::deprecate_soft("0.0.0.9078",
                            "youthvars::make_adol_aqol6d_disv_lup()",
                            "scorz::make_adol_aqol6d_disv_lup()")
  if(is.null(aqol6d_scrg_dss_ls))
    aqol6d_scrg_dss_ls <- get_aqol6d_scrng_dss()
  aqol6d_adult_disv_lup_tb <- aqol6d_scrg_dss_ls$aqol6d_adult_disv_lup_tb
  adol_aqol6d_disv_lup <- aqol6d_adult_disv_lup_tb %>%
    dplyr::mutate(Answer_4_dbl = dplyr::case_when(Question_chr == "Q18" ~ 0.622,
                                                  TRUE ~ Answer_4_dbl),
                  Answer_5_dbl = dplyr::case_when(Question_chr == "Q3" ~ 0.827,
                                                  TRUE ~ Answer_5_dbl),
                  Answer_6_dbl = dplyr::case_when(Question_chr == "Q1" ~ 0.073,
                                                  TRUE ~ Answer_5_dbl))
  return(adol_aqol6d_disv_lup)
}
make_aqol6d_adol_pop_tbs_ls <- function (aqol_items_prpns_tbs_ls, aqol_scores_pars_ls, series_names_chr,
                                         synth_data_spine_ls, temporal_cors_ls,
                                         aqol6d_scrg_dss_ls = NULL,
                                         id_var_nm_1L_chr = "fkClientID",
                                         prefix_chr = c(uid = "Participant_", aqol_item = "aqol6d_q",
                                                        domain_unwtd_pfx_1L_chr = "aqol6d_subtotal_c_", domain_wtd_pfx_1L_chr = "aqol6d_subtotal_w_"))
{
  lifecycle::deprecate_soft("0.0.0.9078",
                            "youthvars::make_aqol6d_adol_pop_tbs_ls()",
                            "scorz::make_aqol6d_adol_pop_tbs_ls()")
  if(is.null(aqol6d_scrg_dss_ls)){
    aqol6d_scrg_dss_ls <- get_aqol6d_scrng_dss()
  }
  domain_qs_lup_tb <- aqol6d_scrg_dss_ls$aqol6d_domain_qs_lup_tb
  item_pfx_1L_chr <- prefix_chr[["aqol_item"]]
  uid_pfx_1L_chr <- prefix_chr[["uid"]]
  aqol6d_adol_pop_tbs_ls <- make_synth_series_tbs_ls(synth_data_spine_ls,
                                                     series_names_chr = series_names_chr) %>% add_cors_and_utls_to_aqol6d_tbs_ls(aqol_scores_pars_ls = aqol_scores_pars_ls,
                                                                                                                                 aqol_items_prpns_tbs_ls = aqol_items_prpns_tbs_ls, temporal_cors_ls = temporal_cors_ls,
                                                                                                                                 prefix_chr = prefix_chr, aqol_tots_var_nms_chr = synth_data_spine_ls$aqol_tots_var_nms_chr,
                                                                                                                                 aqol6d_scrg_dss_ls = aqol6d_scrg_dss_ls,
                                                                                                                                 id_var_nm_1L_chr = id_var_nm_1L_chr) %>% purrr::map(~{
                                                                                                                                   domain_items_ls <- make_domain_items_ls(domain_qs_lup_tb = domain_qs_lup_tb,
                                                                                                                                                                           item_pfx_1L_chr = item_pfx_1L_chr)
                                                                                                                                   domain_items_ls %>% add_unwtd_dim_tots(items_tb = .x,
                                                                                                                                                                          domain_pfx_1L_chr = prefix_chr[["domain_unwtd_pfx_1L_chr"]]) %>%
                                                                                                                                     add_wtd_dim_tots(domain_items_ls = domain_items_ls,
                                                                                                                                                      domain_unwtd_pfx_1L_chr = prefix_chr[["domain_unwtd_pfx_1L_chr"]],
                                                                                                                                                      domain_wtd_pfx_1L_chr = prefix_chr[["domain_wtd_pfx_1L_chr"]],
                                                                                                                                                      aqol6d_scrg_dss_ls = aqol6d_scrg_dss_ls) %>%
                                                                                                                                     add_labels_to_aqol6d_tb()
                                                                                                                                 }) %>% purrr::map(~.x %>% dplyr::select(!!rlang::sym(id_var_nm_1L_chr),
                                                                                                                                                                         dplyr::starts_with(item_pfx_1L_chr), dplyr::starts_with(prefix_chr[["domain_unwtd_pfx_1L_chr"]]),
                                                                                                                                                                         dplyr::starts_with(prefix_chr[["domain_wtd_pfx_1L_chr"]]),
                                                                                                                                                                         dplyr::everything()))
  return(aqol6d_adol_pop_tbs_ls)
}
make_aqol6d_fns_ls <- function (domain_items_ls)
{
  lifecycle::deprecate_soft("0.0.0.9078",
                            "youthvars::make_aqol6d_fns_ls()",
                            "scorz::make_aqol6d_fns_ls()")
  aqol6d_disu_fn_ls <- paste0("calculate_aqol6d_dim_", 1:length(domain_items_ls),
                              "_disv") %>% purrr::map(~rlang::sym(.x))
  return(aqol6d_disu_fn_ls)
}
make_aqol6d_items_tb <- function (aqol_tb, old_pfx_1L_chr, new_pfx_1L_chr)
{
  lifecycle::deprecate_soft("0.0.0.9078",
                            "youthvars::make_aqol6d_items_tb()",
                            "scorz::make_aqol6d_items_tb()")
  aqol6d_items_tb <- aqol_tb %>% dplyr::select(dplyr::starts_with(old_pfx_1L_chr)) %>%
    dplyr::rename_all(~{
      stringr::str_replace(., old_pfx_1L_chr, new_pfx_1L_chr)
    })
  return(aqol6d_items_tb)
}
make_dim_sclg_cons_dbl <- function (domains_chr, dim_sclg_con_lup_tb)
{
  lifecycle::deprecate_soft("0.0.0.9078",
                            "youthvars::make_dim_sclg_cons_dbl()",
                            "scorz::make_dim_sclg_cons_dbl()")
  dim_sclg_cons_dbl <- purrr::map_dbl(domains_chr, ~ready4::get_from_lup_obj(dim_sclg_con_lup_tb,
                                                                             match_var_nm_1L_chr = "Dimension_chr", match_value_xx = .x,
                                                                             target_var_nm_1L_chr = "Constant_dbl", evaluate_1L_lgl = F))
  return(dim_sclg_cons_dbl)
}
make_domain_items_ls <- function (domain_qs_lup_tb, item_pfx_1L_chr)
{
  lifecycle::deprecate_soft("0.0.0.9078",
                            "youthvars::get_aqol6d_scrg_dss()",
                            "scorz::get_aqol6d_scrg_dss()")
  domains_chr <- domain_qs_lup_tb$Domain_chr %>% unique()
  q_nbrs_ls <- purrr::map(domains_chr, ~domain_qs_lup_tb %>%
                            dplyr::filter(Domain_chr == .x) %>% dplyr::pull(Question_dbl))
  domain_items_ls <- purrr::map(q_nbrs_ls, ~paste0(item_pfx_1L_chr,
                                                   .x)) %>% stats::setNames(domains_chr)
  return(domain_items_ls)
}
