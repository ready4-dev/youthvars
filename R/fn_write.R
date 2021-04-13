#' Write qstn resp plots
#' @description write_qstn_resp_plts() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write qstn resp plots. The function returns Composite (a plot).
#' @param data_tb Data (a tibble)
#' @param col_nms_chr Column names (a character vector)
#' @param lbl_nms_chr Label names (a character vector)
#' @param plot_rows_cols_pair_int Plot rows columns pair (an integer vector)
#' @param heights_int Heights (an integer vector)
#' @param round_var_nm_1L_chr Round variable name (a character vector of length one), Default: 'round'
#' @return Composite (a plot)
#' @rdname write_qstn_resp_plts
#' @export 
#' @importFrom scales percent_format
#' @importFrom gridExtra grid.arrange
#' @importFrom ggpubr ggarrange
#' @keywords internal
write_qstn_resp_plts <- function (data_tb, col_nms_chr, lbl_nms_chr, plot_rows_cols_pair_int, 
    heights_int, round_var_nm_1L_chr = "round") 
{
    plots_ls <- list()
    j = 1
    for (i in col_nms_chr) {
        tfd_data_tb <- data_tb %>% transform_ds_for_item_plt(var_nm_1L_chr = i, 
            round_var_nm_1L_chr = round_var_nm_1L_chr)
        labelx <- lbl_nms_chr[j]
        j = j + 1
        plots_ls[[i]] <- make_item_plt(tfd_data_tb, var_nm_1L_chr = i, 
            round_var_nm_1L_chr = round_var_nm_1L_chr, label_1L_chr = labelx, 
            y_scale_scl = scales::percent_format(), use_bw_theme_1L_lgl = T, 
            legend_position_1L_chr = "none")
    }
    plot_plt <- make_item_plt(tfd_data_tb, var_nm_1L_chr = i, 
        round_var_nm_1L_chr = round_var_nm_1L_chr, label_1L_chr = labelx, 
        y_scale_scl = NULL, use_bw_theme_1L_lgl = F, legend_position_1L_chr = "bottom")
    legend_ls <- extract_guide_box_lgd(plot_plt)
    composite_plt <- gridExtra::grid.arrange(ggpubr::ggarrange(plotlist = plots_ls, 
        nrow = plot_rows_cols_pair_int[1], ncol = plot_rows_cols_pair_int[2]), 
        legend_ls, nrow = length(heights_int), heights = heights_int)
    return(composite_plt)
}
#' Write sub ttl plots
#' @description write_sub_ttl_plts() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write sub ttl plots. The function returns Composite (a plot).
#' @param data_tb Data (a tibble)
#' @param col_nms_chr Column names (a character vector)
#' @param plot_rows_cols_pair_int Plot rows columns pair (an integer vector)
#' @param round_var_nm_1L_chr Round variable name (a character vector of length one), Default: 'round'
#' @param make_log_log_tfmn_1L_lgl Make log log transformation (a logical vector of length one), Default: F
#' @param heights_int Heights (an integer vector)
#' @return Composite (a plot)
#' @rdname write_sub_ttl_plts
#' @export 
#' @importFrom dplyr mutate
#' @importFrom stringr str_sub
#' @importFrom stringi stri_locate_last_fixed
#' @importFrom gridExtra grid.arrange
#' @importFrom ggpubr ggarrange
#' @keywords internal
write_sub_ttl_plts <- function (data_tb, col_nms_chr, plot_rows_cols_pair_int, round_var_nm_1L_chr = "round", 
    make_log_log_tfmn_1L_lgl = F, heights_int) 
{
    plots_ls <- list()
    for (i in col_nms_chr) {
        if (make_log_log_tfmn_1L_lgl) {
            targetvar = paste0("tran_", i)
            data_tb <- dplyr::mutate(data_tb, `:=`(!!targetvar, 
                log(-log(1 - !!as.name(i))))) %>% dplyr::mutate(`:=`(!!targetvar, 
                ifelse(!!as.name(i) == 1, log(-log(1 - 0.999)), 
                  !!as.name(targetvar))))
        }
        labelx <- eval(parse(text = paste0("attributes(data_tb$", 
            i, ")$label")))
        labelx <- stringr::str_sub(labelx, start = stringi::stri_locate_last_fixed(labelx, 
            " - ")[1, 1] %>% unname() + 2)
        if (make_log_log_tfmn_1L_lgl) {
            labelx <- paste0("log-log transformed ", labelx)
        }
        plots_ls[[i]] <- make_subtotal_plt(data_tb, var_nm_1L_chr = i, 
            label_1L_chr = labelx)
    }
    plot_for_lgd_plt <- make_subtotal_plt(data_tb, var_nm_1L_chr = i, 
        label_1L_chr = labelx, legend_position_1L_chr = "bottom", 
        label_fill_1L_chr = "Data collection")
    legend_ls <- extract_guide_box_lgd(plot_for_lgd_plt)
    composite_plt <- gridExtra::grid.arrange(ggpubr::ggarrange(plotlist = plots_ls, 
        nrow = plot_rows_cols_pair_int[1], ncol = plot_rows_cols_pair_int[2]), 
        legend_ls, nrow = length(heights_int), heights = heights_int)
    return(composite_plt)
}
