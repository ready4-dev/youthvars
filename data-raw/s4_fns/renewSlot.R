renewSlot_YouthvarsSeries <- function(x,
                                      slot_nm_1L_chr = "descriptives_ls",
                                      nbr_of_digits_1L_int = 3L,
                                      compare_by_time_chr = NA_character_,
                                      compare_by_time_with_test_chr = NA_character_,
                                      compare_ptcpn_chr = NA_character_,
                                      compare_ptcpn_with_test_chr = NA_character_,
                                      profiled_vars_ls = deprecated(),
                                      timepoints_int = c(1L,2L),
                                      ...#type_1L_chr = "characterize"
){
  if(slot_nm_1L_chr == "descriptives_ls"){
    if(identical(x@descriptives_ls,
                 list(list()))){
      descriptives_ls <- NULL
    }else{
      descriptives_ls <- x@descriptives_ls
    }
    if(lifecycle::is_present(profiled_vars_ls)) {
      lifecycle::deprecate_warn("0.0.0.9421",
                                "youthvars::renewSlot(profiled_vars_ls)",
                                details = "Please use `renewSlot(compare_by_time_chr,compare_by_time_with_test_chr,compare_ptcpn_chr,compare_ptcpn_with_test_chr)` instead.")
    }else{
      profiled_vars_ls <- list(compare_by_time_chr,
                               compare_by_time_with_test_chr,
                               compare_ptcpn_chr,
                               compare_ptcpn_with_test_chr)
      if(identical((profiled_vars_ls %>% purrr::discard(~is.na(.x[1]))),list())){
        profiled_vars_ls <- NULL
      }else{
        profiled_vars_ls <- profiled_vars_ls %>%
          stats::setNames(c("temporal",
                            "temporal_tested",
                            "participation",
                            "participation_tested")) %>%
          purrr::discard(~is.na(.x[1]))
      }
    }
    if(!is.null(profiled_vars_ls)){
      incl_idcs_int <- names(profiled_vars_ls) %>% startsWith("temporal")
      temporal_chr <- names(profiled_vars_ls)[incl_idcs_int]
      if(!identical(temporal_chr, character(0))){
        descriptives_ls <- temporal_chr[incl_idcs_int] %>%
          purrr::map2(profiled_vars_ls[incl_idcs_int],
                      ~ YouthvarsDescriptives(key_var_nm_1L_chr = x@timepoint_var_nm_1L_chr,
                                              key_var_vals_chr = x@timepoint_vals_chr[timepoints_int],
                                              nbr_of_digits_1L_int = nbr_of_digits_1L_int,
                                              profiled_vars_chr = .y,
                                              sections_as_row_1L_lgl = F,
                                              test_1L_lgl = endsWith(.x,"_tested"))) %>%
          stats::setNames(temporal_chr) %>%
          append(descriptives_ls)
      }
      incl_idcs_int <- names(profiled_vars_ls) %>% startsWith("participation")
      participation_chr <- names(profiled_vars_ls)[incl_idcs_int]
      if(!identical(participation_chr, character(0))){
        descriptives_ls <- participation_chr[incl_idcs_int] %>%
          purrr::map2(profiled_vars_ls[incl_idcs_int],
                      ~ YouthvarsDescriptives(ds_tfmn_ls = list(args_ls = NULL,
                                                                fn = function(x){dplyr::filter(x,round == "Baseline")}),
                                              key_var_nm_1L_chr = "participation",
                                              nbr_of_digits_1L_int = nbr_of_digits_1L_int,
                                              profiled_vars_chr = .y,
                                              sections_as_row_1L_lgl = F,
                                              test_1L_lgl = T)) %>%
          stats::setNames(participation_chr) %>%
          append(descriptives_ls)
      }
      x@descriptives_ls <- descriptives_ls
    }
  }else{
    methods::callNextMethod()
  }
  return(x)
}
