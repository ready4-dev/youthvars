#' 
#' Ratify that a dataset meets validity criteria
#' @name ratify-YouthvarsSeries
#' @description ratify method applied to YouthvarsSeries
#' @param x An object of class YouthvarsSeries
#' @param timepoints_int Timepoints (an integer vector), Default: c(1L, 2L)
#' @param type_1L_chr Type (a character vector of length one), Default: 'two_timepoints'
#' @return x (An object of class YouthvarsSeries)
#' @rdname ratify-methods
#' @aliases ratify,YouthvarsSeries-method
#' @export 
#' @importFrom tibble tibble add_case
#' @importFrom purrr flatten_chr
#' @importFrom ready4use remove_labels_from_ds add_labels_from_dictionary
#' @importFrom dplyr arrange
#' @importFrom ready4 ratify
methods::setMethod("ratify", "YouthvarsSeries", function (x, timepoints_int = c(1L, 2L), type_1L_chr = "two_timepoints") 
{
    if (type_1L_chr == "two_timepoints") 
        assert_ds_is_valid(x@a_Ready4useDyad@ds_tb, id_var_nm_1L_chr = x@id_var_nm_1L_chr, 
            round_var_nm_1L_chr = x@timepoint_var_nm_1L_chr, 
            round_bl_val_1L_chr = x@timepoint_vals_chr[1])
    if (!is.na(x@participation_var_1L_chr)) {
        if (!x@participation_var_1L_chr %in% x@a_Ready4useDyad@ds_tb) {
            x@a_Ready4useDyad@ds_tb <- add_participation_var(x@a_Ready4useDyad@ds_tb, 
                id_var_nm_1L_chr = x@id_var_nm_1L_chr, fup_round_nbr_1L_int = timepoints_int[2], 
                participation_var_1L_chr = x@participation_var_1L_chr, 
                timepoint_vals_chr = x@timepoint_vals_chr[timepoints_int])
            labels_ls <- labels(x@a_Ready4useDyad@dictionary_r3)
            labels_tb <- tibble::tibble(var_nm_chr = names(labels_ls), 
                var_desc_chr = labels_ls %>% purrr::flatten_chr())
            x@a_Ready4useDyad@dictionary_r3 <- tibble::add_case(x@a_Ready4useDyad@dictionary_r3 %>% 
                ready4use::remove_labels_from_ds(), var_nm_chr = x@participation_var_1L_chr, 
                var_ctg_chr = "temporal", var_desc_chr = "selected timepoints for which data was provided", 
                var_type_chr = "character") %>% dplyr::arrange(var_ctg_chr, 
                var_nm_chr) %>% ready4use::add_labels_from_dictionary(dictionary_tb = labels_tb)
        }
    }
    return(x)
})
