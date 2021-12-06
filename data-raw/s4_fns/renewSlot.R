renewSlot_YouthvarsSeries <- function(x,
                                      slot_nm_1L_chr = "descriptives_ls",
                                      nbr_of_digits_1L_int = 3L,
                                      profiled_vars_ls = NULL,
                                      timepoints_int = c(1L,2L),
                                      ...#type_1L_chr = "characterize"
){
  if(slot_nm_1L_chr == "descriptives_ls"){
    #if(type_1L_chr == "characterize"){
    if(identical(x@descriptives_ls,
                 list(list()))){
      descriptives_ls <- NULL
    }else{
      descriptives_ls <- x@descriptives_ls
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