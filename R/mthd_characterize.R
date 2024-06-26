#' 
#' Characterize model module data by generating (tabular) descriptive statistics
#' @name characterize-YouthvarsDescriptives
#' @description characterize method applied to YouthvarsDescriptives
#' @param x An object of class YouthvarsDescriptives
#' @param y_Ready4useDyad PARAM_DESCRIPTION
#' @param ... Additional arguments
#' @return Descriptives (a data.frame)
#' @rdname characterize-methods
#' @aliases characterize,YouthvarsDescriptives-method
#' @export 
#' @importFrom rlang exec
#' @importFrom ready4 characterize
methods::setMethod("characterize", "YouthvarsDescriptives", function (x, y_Ready4useDyad, ...) 
{
    if (!is.null(x@ds_tfmn_ls$args_ls)) {
        data_tb <- rlang::exec(x@ds_tfmn_ls$fn, y_Ready4useDyad@ds_tb, 
            !!!x@ds_tfmn_ls$args_ls)
    }
    else {
        data_tb <- y_Ready4useDyad@ds_tb
    }
    if (is.na(x@key_var_vals_chr[1])) {
        key_var_vals_chr <- NULL
    }
    else {
        key_var_vals_chr <- x@key_var_vals_chr
    }
    descriptives_df <- make_descv_stats_tbl(data_tb = data_tb, 
        key_var_nm_1L_chr = x@key_var_nm_1L_chr, key_var_vals_chr = key_var_vals_chr, 
        dictionary_tb = y_Ready4useDyad@dictionary_r3, sections_as_row_1L_lgl = x@sections_as_row_1L_lgl, 
        test_1L_lgl = x@test_1L_lgl, variable_nms_chr = x@profiled_vars_chr, 
        nbr_of_digits_1L_int = x@nbr_of_digits_1L_int)
    return(descriptives_df)
})
