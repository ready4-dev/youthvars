## ----message=F----------------------------------------------------------------
library(youthvars)

## ----eval = !knitr::is_html_output(), echo = !knitr::is_html_output()---------
#  library(xtable)
#  options(xtable.floating = FALSE)
#  options(xtable.timestamp = "")

## ----message=FALSE------------------------------------------------------------
data("repln_ds_dict_r3", package = "youthvars")
dictionary_tb <- repln_ds_dict_r3

## ----ddic, eval = knitr::is_html_output(), results='asis'---------------------
dictionary_tb %>%
  ready4show::print_table(output_type_1L_chr = ifelse(knitr::is_html_output(),"HTML","PDF"), 
                          caption_1L_chr = "Data dictionary",  
                          use_lbls_as_col_nms_1L_lgl = T, 
                          hline_after_ls = c(-1),
                          add_to_row_ls = list(pos = list(-1,nrow(dictionary_tb)),
                                               command = c(Hmisc::label(dictionary_tb) %>%
                                                             Hmisc::latexTranslate() %>%
                                                             paste0(collapse = " & ") %>%
                                                             paste0("\\\\\n"),
                                                           paste("\\\\\n"))),
                          mkdn_tbl_ref_1L_chr = "tab:ddic")

## -----------------------------------------------------------------------------
data("replication_popl_tb", package = "youthvars")
raw_data_tb <- replication_popl_tb 

## -----------------------------------------------------------------------------
raw_data_tb <- raw_data_tb %>%
  ready4use::add_labels_from_dictionary(dictionary_tb)

## ----repds, echo = knitr::is_html_output(), results='asis'--------------------
raw_data_tb %>%
  head() %>%
  dplyr::mutate(d_interview_date = as.character(d_interview_date)) %>%
  ready4show::print_table(output_type_1L_chr = ifelse(knitr::is_html_output(),"HTML","PDF"),
                          caption_1L_chr = "Replication dataset", 
                          use_lbls_as_col_nms_1L_lgl = T, 
                          hline_after_ls = c(-1),
                          add_to_row_ls = list(pos = list(-1,nrow(raw_data_tb %>%
  head())),
                                               command = c(Hmisc::label(raw_data_tb) %>%
                                                             Hmisc::latexTranslate() %>%
                                                             paste0(collapse = " & ") %>%
                                                             paste0("\\\\\n"),
                                                           paste("\\\\\n"))),
                          mkdn_tbl_ref_1L_chr = "tab:repds") 

