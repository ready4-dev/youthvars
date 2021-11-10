print_descv_stats_tbl <- function (df,
                                   data_tb,
                                   output_type_1L_chr,
                                   round_var_nm_1L_chr,
                                   variable_nms_chr,
                                   bl_fup_vals_chr = c("Baseline","Follow-up"),
                                   capitalise_1L_lgl = T,
                                   caption_1L_chr = NULL,
                                   header_col_nms_chr = NULL,
                                   mkdn_tbl_ref_1L_chr = NULL,
                                   test_1L_lgl = F)
{
  if(capitalise_1L_lgl)
    df <- df %>%
      dplyr::mutate(variable = variable %>% purrr::map_chr(~Hmisc::capitalize(.x)))
  if(is.null(header_col_nms_chr))
    header_col_nms_chr <- bl_fup_vals_chr
  df <- df %>%
    dplyr::filter(!label %in% variable_nms_chr)
  valid_var_nms_chr <- df$label %>% unique() %>%
    stringi::stri_replace_all_fixed("_","")
  rename_lup <- tibble::tibble(old_nms_chr = df$label %>% unique(),
                               new_nms_chr = make.unique(valid_var_nms_chr, sep="V"))
  df$label <- df$label %>% purrr::map_chr(~ifelse(.x %in% rename_lup$old_nms_chr,
                                                  ready4::get_from_lup_obj(rename_lup,
                                                                              match_value_xx = .x,
                                                                              match_var_nm_1L_chr = "old_nms_chr",
                                                                              target_var_nm_1L_chr = "new_nms_chr",
                                                                              evaluate_1L_lgl = F),
                                                  .x))

  #df$variable <- gsub("\\s*\\([^\\)]+\\)", "",df$variable)
  n_inc_1L_dbl <- data_tb %>%
    dplyr::filter(!!rlang::sym(round_var_nm_1L_chr) == bl_fup_vals_chr[1]) %>%
    nrow()
  n_fup_1L_dbl <- data_tb %>%
    dplyr::filter(!!rlang::sym(round_var_nm_1L_chr) == bl_fup_vals_chr[2]) %>%
    nrow()
  if(is.null(caption_1L_chr))
    caption_1L_chr <- knitr::opts_current$get("tab.cap")
  if(is.null(mkdn_tbl_ref_1L_chr))
    mkdn_tbl_ref_1L_chr <- paste0("tab:",
                                  knitr::opts_current$get("tab.id"))
  if(output_type_1L_chr %in% c("HTML",
                               "Word"))
    df <- df %>% transform_tb_for_merged_col_1(output_type_1L_chr = output_type_1L_chr)
  if (output_type_1L_chr %in% c("PDF","HTML")) {
    df <- df %>% dplyr::mutate_all(~stringr::str_replace(.x,
                                                         "%", "\\\\%") %>% stringr::str_replace(",",
                                                                                                "\\\\,"))
  }
  if (output_type_1L_chr %in% c("PDF","HTML")) {
    names_chr <- c("",
                   "",
                   "(N =",
                   paste0(n_inc_1L_dbl,
                          ifelse(output_type_1L_chr == "HTML","\\)",")")),
                   "(N =",
                   paste0(n_fup_1L_dbl,
                          ifelse(output_type_1L_chr == "HTML","\\)",")")))
    header_chr <- c(" ",
                    " ",
                    2,
                    2) %>%
      stats::setNames(c(c("",""),header_col_nms_chr))
    if(test_1L_lgl){
      names_chr <- c(names_chr,
                     ifelse(output_type_1L_chr == "HTML","*p*","\\textit{p}"))
      header_chr <- c(header_chr, " ")
    }
    names(df) <- names_chr
    df %>% kableExtra::kbl(booktabs = T,
                           caption = caption_1L_chr,
                           escape = F) %>%
      kableExtra::kable_styling() %>%
      kableExtra::column_spec(3:6,
                              width = "3em") %>%
      kableExtra::column_spec(1, bold = T, width = "14em") %>%
      kableExtra::add_header_above(header_chr) %>%
      kableExtra::collapse_rows(columns = 1)
  }
  else {
    # df <- df %>% transform_tb_for_merged_col_1(output_type_1L_chr = output_type_1L_chr)
    add_to_row_ls <- NULL
    df %>% ready4show::print_table(output_type_1L_chr = output_type_1L_chr,
                                   caption_1L_chr = caption_1L_chr,
                                   mkdn_tbl_ref_1L_chr = mkdn_tbl_ref_1L_chr,
                                   use_rdocx_1L_lgl = ifelse(output_type_1L_chr ==
                                                               "Word",
                                                             T,
                                                             F),
                                   add_to_row_ls = add_to_row_ls,
                                   sanitize_fn = force)
  }
}
