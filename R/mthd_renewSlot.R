#' 
#' Renew (set) the values of data in a module slot
#' @name renewSlot-YouthvarsSeries
#' @description renewSlot method applied to YouthvarsSeries
#' @param x An object of class YouthvarsSeries
#' @param slot_nm_1L_chr Slot name (a character vector of length one), Default: 'descriptives_ls'
#' @param nbr_of_digits_1L_int Number of digits (an integer vector of length one), Default: 3
#' @param compare_by_time_chr Compare by time (a character vector), Default: 'NA'
#' @param compare_by_time_with_test_chr Compare by time with test (a character vector), Default: 'NA'
#' @param compare_ptcpn_chr Compare ptcpn (a character vector), Default: 'NA'
#' @param compare_ptcpn_with_test_chr Compare ptcpn with test (a character vector), Default: 'NA'
#' @param profiled_vars_ls Profiled variables (a list), Default: deprecated()
#' @param timepoints_int Timepoints (an integer vector), Default: c(1L, 2L)
#' @param ... Additional arguments
#' @return x (An object of class YouthvarsSeries)
#' @rdname renewSlot-methods
#' @aliases renewSlot,YouthvarsSeries-method
#' @export 
#' @importFrom lifecycle deprecate_warn is_present
#' @importFrom purrr discard map2
#' @importFrom stats setNames
#' @importFrom dplyr filter
#' @importFrom methods callNextMethod
#' @importFrom ready4 renewSlot
methods::setMethod("renewSlot", "YouthvarsSeries", function (x, slot_nm_1L_chr = "descriptives_ls", nbr_of_digits_1L_int = 3L, 
    compare_by_time_chr = NA_character_, compare_by_time_with_test_chr = NA_character_, 
    compare_ptcpn_chr = NA_character_, compare_ptcpn_with_test_chr = NA_character_, 
    profiled_vars_ls = deprecated(), timepoints_int = c(1L, 2L), 
    ...) 
{
    lifecycle::deprecate_warn("0.0.0.9112", "youthvars::renewSlot()", 
        details = "Please use `renew(x, what_1L_chr = 'descriptives_ls', compare_by_time_chr,compare_by_time_with_test_chr,compare_ptcpn_chr,compare_ptcpn_with_test_chr)` instead.")
    if (slot_nm_1L_chr == "descriptives_ls") {
        if (identical(x@descriptives_ls, list(list()))) {
            descriptives_ls <- NULL
        }
        else {
            descriptives_ls <- x@descriptives_ls
        }
        if (lifecycle::is_present(profiled_vars_ls)) {
            lifecycle::deprecate_warn("0.0.0.9112", "youthvars::renewSlot(profiled_vars_ls)", 
                details = "Please use `renew(x, what_1L_chr = 'descriptives_ls', compare_by_time_chr,compare_by_time_with_test_chr,compare_ptcpn_chr,compare_ptcpn_with_test_chr)` instead.")
        }
        else {
            profiled_vars_ls <- list(compare_by_time_chr, compare_by_time_with_test_chr, 
                compare_ptcpn_chr, compare_ptcpn_with_test_chr)
            if (identical((profiled_vars_ls %>% purrr::discard(~is.na(.x[1]))), 
                list())) {
                profiled_vars_ls <- NULL
            }
            else {
                profiled_vars_ls <- profiled_vars_ls %>% stats::setNames(c("temporal", 
                  "temporal_tested", "participation", "participation_tested")) %>% 
                  purrr::discard(~is.na(.x[1]))
            }
        }
        if (!is.null(profiled_vars_ls)) {
            incl_idcs_int <- names(profiled_vars_ls) %>% startsWith("temporal")
            temporal_chr <- names(profiled_vars_ls)[incl_idcs_int]
            if (!identical(temporal_chr, character(0))) {
                descriptives_ls <- temporal_chr[incl_idcs_int] %>% 
                  purrr::map2(profiled_vars_ls[incl_idcs_int], 
                    ~YouthvarsDescriptives(key_var_nm_1L_chr = x@timepoint_var_nm_1L_chr, 
                      key_var_vals_chr = x@timepoint_vals_chr[timepoints_int], 
                      nbr_of_digits_1L_int = nbr_of_digits_1L_int, 
                      profiled_vars_chr = .y, sections_as_row_1L_lgl = F, 
                      test_1L_lgl = endsWith(.x, "_tested"))) %>% 
                  stats::setNames(temporal_chr) %>% append(descriptives_ls)
            }
            incl_idcs_int <- names(profiled_vars_ls) %>% startsWith("participation")
            participation_chr <- names(profiled_vars_ls)[incl_idcs_int]
            if (!identical(participation_chr, character(0))) {
                descriptives_ls <- participation_chr[incl_idcs_int] %>% 
                  purrr::map2(profiled_vars_ls[incl_idcs_int], 
                    ~YouthvarsDescriptives(ds_tfmn_ls = list(args_ls = NULL, 
                      fn = function(x) {
                        dplyr::filter(x, round == "Baseline")
                      }), key_var_nm_1L_chr = "participation", 
                      nbr_of_digits_1L_int = nbr_of_digits_1L_int, 
                      profiled_vars_chr = .y, sections_as_row_1L_lgl = F, 
                      test_1L_lgl = T)) %>% stats::setNames(participation_chr) %>% 
                  append(descriptives_ls)
            }
            x@descriptives_ls <- descriptives_ls
        }
    }
    else {
        methods::callNextMethod()
    }
    return(x)
})
