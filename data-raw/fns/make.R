make_adol_aqol6d_disv_lup <- function ()
{
  utils::data("aqol6d_adult_disv_lup_tb", package = "youthvars", envir = environment())
  adol_aqol6d_disv_lup <- aqol6d_adult_disv_lup_tb %>%
    dplyr::mutate(Answer_4_dbl = dplyr::case_when(Question_chr == "Q18" ~ 0.622,
                                                  TRUE ~ Answer_4_dbl),
                  Answer_5_dbl = dplyr::case_when(Question_chr == "Q3" ~ 0.827,
                                                  TRUE ~ Answer_5_dbl),
                  Answer_6_dbl = dplyr::case_when(Question_chr == "Q1" ~ 0.073,
                                                  TRUE ~ Answer_5_dbl))
  return(adol_aqol6d_disv_lup)
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
      dplyr::mutate(variable_chr = variable_chr %>% purrr::map_chr(~ready4fun::get_from_lup_obj(dictionary_tb,
                                                                                        target_var_nm_1L_chr = "var_desc_chr",
                                                                                        match_var_nm_1L_chr = "var_nm_chr",
                                                                                        match_value_xx = .x,
                                                                                        evaluate_lgl = F)))

  }
  return(cors_with_utl_tb)
}
make_descv_stats_tbl <- function(data_tb,
                                 key_var_nm_1L_chr = "round",
                                 key_var_vals_chr,
                                 variable_nms_chr,
                                 dictionary_tb = NULL,
                                 test_1L_lgl = F,
                                 sections_as_row_1L_lgl = F,
                                 nbr_of_digits_1L_int = NA_integer_){
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
      dplyr::mutate(variable = variable %>% purrr::map_chr(~ready4fun::get_from_lup_obj(dictionary_tb,
                                                                           target_var_nm_1L_chr = "var_desc_chr",
                                                                           match_var_nm_1L_chr = "var_nm_chr",
                                                                           match_value_xx = .x,
                                                                           evaluate_lgl = F)))

  }
  vars_with_mdns_chr <- descv_stats_tbl_tb %>% dplyr::filter(label == "Median (Q1, Q3)") %>% dplyr::pull(variable)
  descv_stats_tbl_tb <- descv_stats_tbl_tb %>%
    dplyr::mutate(dplyr::across(key_var_vals_chr,
                                ~ .x %>% purrr::map2_dbl(variable,
                                                         ~ ifelse(.y %in% vars_with_mdns_chr,
                                                                  ifelse(.x[[1]]=="",
                                                                         NA_real_,
                                                                         .x[[1]]),
                                                                  ifelse(.x[[1]]=="",
                                                                         NA_real_,
                                                                         .x[[1]]))),
                                .names = "{col}_val_1_dbl"),
                  dplyr::across(key_var_vals_chr,
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
    descv_stats_tbl_tb <- descv_stats_tbl_tb %>%
      dplyr::mutate(dplyr::across(c(key_var_vals_chr %>%
                                    purrr::map(~c(paste0(.x, c("_val_1_dbl","_val_2_ls")))) %>%
                                    purrr::flatten_chr(),
                                    ifelse(test_1L_lgl,"p.value",character(0)) %>% purrr::discard(is.na)),
                                  ~ .x %>% purrr::map_chr(~ {
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
                                    })
                                  )) %>%
      dplyr::mutate(dplyr::across(paste0(key_var_vals_chr, "_val_2_ls"),
                                  ~ {
                                    .x %>% purrr::map2_chr(label,
                                                           ~ ifelse(.x=="" | .y== "Min - Max",
                                                                    .x,
                                                                    paste0("(",
                                                                           .x,
                                                                           ifelse(.y %in% c("Mean (SD)","Median (Q1, Q3)","Missing"),
                                                                                  "",
                                                                                  "%"),
                                                                           ")")))

                                    }
                                  ))
  }
  return(descv_stats_tbl_tb)
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
                                round_var_nm_1L_chr = "round"){
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
                                  y_scale_scl_fn = scales::percent_format(),
                                  use_bw_theme_1L_lgl = T,
                                  legend_position_1L_chr = "none")
  }
  plot_plt <- make_item_plt(tfd_data_tb,
                            var_nm_1L_chr = i,
                            round_var_nm_1L_chr = round_var_nm_1L_chr,
                            x_label_1L_chr = labelx,
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
make_predr_pars_and_cors_tbl <- function(data_tb,
                                         ds_descvs_ls,
                                         dictionary_tb,
                                         nbr_of_digits_1L_int = 2L,
                                         predictors_lup = NULL){
  predr_pars_and_cors_tb <- make_cors_with_utl_tbl(data_tb,
                                                   ds_descvs_ls = ds_descvs_ls,
                                                   dictionary_tb = dictionary_tb) %>%
    dplyr::mutate(label = paste0("Correlation with ",
                                 ready4fun::get_from_lup_obj(dictionary_tb,
                                                             match_var_nm_1L_chr = "var_nm_chr",
                                                             match_value_xx = ds_descvs_ls$utl_wtd_var_nm_1L_chr,
                                                             target_var_nm_1L_chr = "var_desc_chr",
                                                             evaluate_lgl = F)))

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
                                                   ~ready4fun::get_from_lup_obj(dictionary_tb,
                                                                                match_var_nm_1L_chr = "var_nm_chr",
                                                                                match_value_xx = .x,
                                                                                target_var_nm_1L_chr = "var_desc_chr",
                                                                                evaluate_lgl = F)))
  predr_pars_and_cors_tb <- main_outc_tbl_tb$variable_chr %>% unique() %>%
    purrr::map_dfr(~tibble::add_case(main_outc_tbl_tb %>%
                                       dplyr::filter(variable_chr == .x),
                                     predr_pars_and_cors_tb %>%
                                       dplyr::filter(variable_chr == .x)))
  if(!is.null(predictors_lup)){
    predr_pars_and_cors_tb <- predr_pars_and_cors_tb %>%
      dplyr::mutate(variable_chr = purrr::map_chr(variable_chr,
                                                  ~ {
                                                    var_nm_1L_chr <- ready4fun::get_from_lup_obj(dictionary_tb,
                                                                                                 match_var_nm_1L_chr = "var_desc_chr",
                                                                                                 match_value_xx = .x,
                                                                                                 target_var_nm_1L_chr = "var_nm_chr",
                                                                                                 evaluate_lgl = F)
                                                    paste0 (.x,
                                                            " (",
                                                            ready4fun::get_from_lup_obj(predictors_lup,
                                                                                        match_var_nm_1L_chr = "short_name_chr",
                                                                                        match_value_xx = var_nm_1L_chr,
                                                                                        target_var_nm_1L_chr = "min_val_dbl",
                                                                                        evaluate_lgl = F),
                                                            "-",
                                                            ready4fun::get_from_lup_obj(predictors_lup,
                                                                                        match_var_nm_1L_chr = "short_name_chr",
                                                                                        match_value_xx = var_nm_1L_chr,
                                                                                        target_var_nm_1L_chr = "max_val_dbl",
                                                                                        evaluate_lgl = F),
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
                              heights_int ){
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
                                      var_nm_1L_chr = i,
                                      x_label_1L_chr = labelx)
  }
  plot_for_lgd_plt <- make_subtotal_plt(data_tb,
                                        var_nm_1L_chr = i,
                                        x_label_1L_chr = labelx,
                                        legend_position_1L_chr = "bottom",
                                        label_fill_1L_chr = "Data collection" )
  legend_ls <- get_guide_box_lgd(plot_for_lgd_plt)
  composite_plt <- gridExtra::grid.arrange(ggpubr::ggarrange(plotlist=plots_ls,
                                                             nrow = plot_rows_cols_pair_int[1],
                                                             ncol = plot_rows_cols_pair_int[2]),
                                           legend_ls,
                                           nrow = length(heights_int),
                                           heights = heights_int)
  return(composite_plt)
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
