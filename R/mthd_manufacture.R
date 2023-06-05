#' 
#' Manufacture a new object
#' @name manufacture-YouthvarsProfile
#' @description manufacture method applied to YouthvarsProfile
#' @param x An object of class YouthvarsProfile
#' @param element_nm_1L_chr Element name (a character vector of length one), Default: 'overall'
#' @param nbr_of_digits_1L_int Number of digits (an integer vector of length one), Default: 3
#' @param profile_chr Profile (a character vector), Default: character(0)
#' @param what_1L_chr What (a character vector of length one), Default: 'descriptives_ls'
#' @param ... Additional arguments
#' @return Object (an output object of multiple potential types)
#' @rdname manufacture-methods
#' @aliases manufacture,YouthvarsProfile-method
#' @export 
#' @importFrom stats setNames
#' @importFrom ready4 manufacture
methods::setMethod("manufacture", "YouthvarsProfile", function (x, element_nm_1L_chr = "overall", nbr_of_digits_1L_int = 3L, 
    profile_chr = character(0), what_1L_chr = "descriptives_ls", 
    ...) 
{
    object_xx <- NULL
    if (what_1L_chr == "descriptives_ls") {
        if (identical(x@descriptives_ls, list(list()))) {
            descriptives_ls <- NULL
        }
        else {
            descriptives_ls <- x@descriptives_ls
        }
        object_xx <- list(YouthvarsDescriptives(key_var_nm_1L_chr = character(0), 
            key_var_vals_chr = "Overall", nbr_of_digits_1L_int = nbr_of_digits_1L_int, 
            profiled_vars_chr = profile_chr, sections_as_row_1L_lgl = F, 
            test_1L_lgl = F)) %>% stats::setNames(element_nm_1L_chr) %>% 
            append(descriptives_ls)
    }
    return(object_xx)
})
#' 
#' Manufacture a new object
#' @name manufacture-YouthvarsSeries
#' @description manufacture method applied to YouthvarsSeries
#' @param x An object of class YouthvarsSeries
#' @param compare_by_time_chr Compare by time (a character vector), Default: 'NA'
#' @param compare_by_time_with_test_chr Compare by time with test (a character vector), Default: 'NA'
#' @param compare_ptcpn_chr Compare ptcpn (a character vector), Default: 'NA'
#' @param compare_ptcpn_with_test_chr Compare ptcpn with test (a character vector), Default: 'NA'
#' @param nbr_of_digits_1L_int Number of digits (an integer vector of length one), Default: 3
#' @param timepoints_int Timepoints (an integer vector), Default: c(1L, 2L)
#' @param what_1L_chr What (a character vector of length one), Default: 'descriptives_ls'
#' @param ... Additional arguments
#' @return Object (an output object of multiple potential types)
#' @rdname manufacture-methods
#' @aliases manufacture,YouthvarsSeries-method
#' @export 
#' @importFrom purrr discard map2
#' @importFrom stats setNames
#' @importFrom dplyr filter
#' @importFrom methods callNextMethod
#' @importFrom ready4 manufacture
methods::setMethod("manufacture", "YouthvarsSeries", function (x, compare_by_time_chr = NA_character_, compare_by_time_with_test_chr = NA_character_, 
    compare_ptcpn_chr = NA_character_, compare_ptcpn_with_test_chr = NA_character_, 
    nbr_of_digits_1L_int = 3L, timepoints_int = c(1L, 2L), what_1L_chr = "descriptives_ls", 
    ...) 
{
    object_xx <- NULL
    if (what_1L_chr == "descriptives_ls") {
        if (identical(x@descriptives_ls, list(list()))) {
            descriptives_ls <- NULL
        }
        else {
            descriptives_ls <- x@descriptives_ls
        }
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
            object_xx <- descriptives_ls
        }
        else {
            object_xx <- methods::callNextMethod()
        }
    }
    else {
        object_xx <- methods::callNextMethod()
    }
    return(object_xx)
})
