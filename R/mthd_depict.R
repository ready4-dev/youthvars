#' 
#' Depict (plot) an object
#' @name depict-YouthvarsSeries
#' @description depict method applied to YouthvarsSeries
#' @param x An object of class YouthvarsSeries
#' @param var_nms_chr Variable names (a character vector)
#' @param type_1L_chr Type (a character vector of length one), Default: 'by_time'
#' @param labels_chr Labels (a character vector), Default: 'NA'
#' @param label_fill_1L_chr Label fill (a character vector of length one), Default: 'Data collection'
#' @param y_label_1L_chr Y label (a character vector of length one), Default: 'Percentage'
#' @param y_scale_scl_fn Y scale scale (a function), Default: scales::percent
#' @return NULL
#' @rdname depict-methods
#' @aliases depict,YouthvarsSeries-method
#' @export 
#' @importFrom scales percent
#' @importFrom purrr map_chr map2 pluck
#' @importFrom ready4 get_from_lup_obj depict
methods::setMethod("depict", "YouthvarsSeries", function (x, var_nms_chr, type_1L_chr = "by_time", labels_chr = NA_character_, 
    label_fill_1L_chr = "Data collection", y_label_1L_chr = "Percentage", 
    y_scale_scl_fn = scales::percent) 
{
    if (is.na(labels_chr[1])) 
        labels_chr <- var_nms_chr %>% purrr::map_chr(~ready4::get_from_lup_obj(x@a_Ready4useDyad@dictionary_r3, 
            match_var_nm_1L_chr = "var_nm_chr", match_value_xx = .x, 
            target_var_nm_1L_chr = "var_desc_chr"))
    if (type_1L_chr == "by_time") {
        plot_ls <- purrr::map2(var_nms_chr, labels_chr, ~make_var_by_round_plt(x@a_Ready4useDyad@ds_tb, 
            label_fill_1L_chr = label_fill_1L_chr, round_var_nm_1L_chr = x@timepoint_var_nm_1L_chr, 
            var_nm_1L_chr = .x, x_label_1L_chr = .y, y_label_1L_chr = y_label_1L_chr, 
            y_scale_scl_fn = y_scale_scl_fn))
        if (length(var_nms_chr) == 1) 
            plot_ls <- plot_ls %>% purrr::pluck(1)
    }
    else {
        plot_ls <- NULL
    }
    plot_ls
})
