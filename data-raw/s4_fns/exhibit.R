exhibit_YouthvarsProfile <- function(x,
                                     captions_chr = NULL,
                                     header_1L_chr = "",
                                     header_col_nms_chr = " ",
                                     mkdn_tbl_refs_chr = NULL,
                                     profile_idx_int = NA_integer_,
                                     output_type_1L_chr = "HTML",
                                     what_1L_chr = "descriptives",
                                     ...){
  print_ls <- NULL
  if(what_1L_chr == "descriptives"){
    if(is.na(profile_idx_int))
      profile_idx_int <- 1:length(x@descriptives_ls) %>% as.integer()
    print_ls <- profile_idx_int %>%
      purrr::map(~{
        profile_idx_1L_int <- .x
        if(identical(x@descriptives_ls[[profile_idx_1L_int]]@descriptives_df, data.frame())){
          message("It was not possible to print a table as the descriptives_df element of the descriptives_ls slot of the supplied YouthvarsDescriptives instance is empty. Use characterise(x) or renew(x) methods to generate a value for this element.")
        }else{
          x@descriptives_ls[[profile_idx_1L_int]]@descriptives_df %>%
            print_descv_stats_tbl(bl_fup_vals_chr = header_1L_chr,
                                  caption_1L_chr = captions_chr[profile_idx_1L_int],
                                  data_tb = x@a_Ready4useDyad@ds_tb,
                                  header_col_nms_chr = header_col_nms_chr,
                                  mkdn_tbl_ref_1L_chr = mkdn_tbl_refs_chr[profile_idx_1L_int],
                                  output_type_1L_chr = output_type_1L_chr,
                                  round_var_nm_1L_chr = character(0),
                                  test_1L_lgl =  x@descriptives_ls[[profile_idx_1L_int]]@test_1L_lgl,
                                  variable_nms_chr =  x@descriptives_ls[[profile_idx_1L_int]]@profiled_vars_chr,
                                  ...)
        }
      })
    if(length(profile_idx_int)==1)
      print_ls <- print_ls[[1]]
  }
  print_ls
}
exhibit_YouthvarsSeries <- function(x,
                                    captions_chr = NULL,
                                    mkdn_tbl_refs_chr = NULL,
                                    profile_idx_int = NA_integer_,
                                    output_type_1L_chr = "HTML",
                                    timepoints_int = c(1L,2L),
                                    type_1L_chr = "characterize", # Deprecate
                                    what_1L_chr = "descriptives",
                                    ...){
  print_ls <- NULL
  if(what_1L_chr == "descriptives"){
    if(is.na(profile_idx_int))
      profile_idx_int <- 1:length(x@descriptives_ls) %>% as.integer()
    print_ls <-profile_idx_int %>%
      purrr::map(~{
        profile_idx_1L_int <- .x
        if(identical(x@descriptives_ls[[profile_idx_1L_int]]@descriptives_df, data.frame())){
          message("It was not possible to print a table as the descriptives_df element of the descriptives_ls slot of the supplied YouthvarsDescriptives instance is empty. Use characterise(x) or renew(x) methods to generate a value for this element.")
        }else{
          if(x@descriptives_ls[[profile_idx_1L_int]]@key_var_nm_1L_chr == "participation"){ ### MUST GENERALISE
            header_col_nms_chr <- x@a_Ready4useDyad@ds_tb$participation %>% unique()
          }else{
            header_col_nms_chr <- NULL
          }
          x@descriptives_ls[[profile_idx_1L_int]]@descriptives_df %>%
            print_descv_stats_tbl(bl_fup_vals_chr = x@timepoint_vals_chr[timepoints_int],
                                  caption_1L_chr = captions_chr[profile_idx_1L_int],
                                  data_tb = x@a_Ready4useDyad@ds_tb,
                                  header_col_nms_chr = header_col_nms_chr,
                                  mkdn_tbl_ref_1L_chr = mkdn_tbl_refs_chr[profile_idx_1L_int],
                                  output_type_1L_chr = output_type_1L_chr,
                                  round_var_nm_1L_chr = x@timepoint_var_nm_1L_chr,
                                  test_1L_lgl =  x@descriptives_ls[[profile_idx_1L_int]]@test_1L_lgl,
                                  variable_nms_chr =  x@descriptives_ls[[profile_idx_1L_int]]@profiled_vars_chr,
                                  ...)
        }
      })
    if(length(profile_idx_int)==1)
      print_ls <- print_ls[[1]]
  }
  print_ls
}
