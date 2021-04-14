#' Make adolescent Assessment of Quality of Life Six Dimension disvalue
#' @description make_adol_aqol6d_disv_lup() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make adolescent assessment of quality of life six dimension disvalue lookup table. The function returns Adolescent Assessment of Quality of Life Six Dimension disvalue (a lookup table).

#' @return Adolescent Assessment of Quality of Life Six Dimension disvalue (a lookup table)
#' @rdname make_adol_aqol6d_disv_lup
#' @export 
#' @importFrom utils data
#' @importFrom dplyr mutate case_when
#' @keywords internal
make_adol_aqol6d_disv_lup <- function () 
{
    utils::data("aqol6d_adult_disv_lup_tb", package = "youthvars", 
        envir = environment())
    adol_aqol6d_disv_lup <- aqol6d_adult_disv_lup_tb %>% dplyr::mutate(Answer_4_dbl = dplyr::case_when(Question_chr == 
        "Q18" ~ 0.622, TRUE ~ Answer_4_dbl), Answer_5_dbl = dplyr::case_when(Question_chr == 
        "Q3" ~ 0.827, TRUE ~ Answer_5_dbl), Answer_6_dbl = dplyr::case_when(Question_chr == 
        "Q1" ~ 0.073, TRUE ~ Answer_5_dbl))
    return(adol_aqol6d_disv_lup)
}
#' Make descriptive statistics table
#' @description make_descv_stats_tbl() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make descriptive statistics table. The function returns Descriptive statistics table (a tibble).
#' @param data_tb Data (a tibble)
#' @param key_var_nm_1L_chr Key variable name (a character vector of length one), Default: 'round'
#' @param make_descv_stats_tbl PARAM_DESCRIPTION
#' @param key_var_vals_chr Key variable values (a character vector)
#' @param variable_nms_chr Variable names (a character vector)
#' @param test_1L_lgl Test (a logical vector of length one), Default: F
#' @return Descriptive statistics table (a tibble)
#' @rdname make_descv_stats_tbl
#' @export 
#' @importFrom dplyr select
#' @importFrom tidyselect all_of
#' @importFrom purrr discard
make_descv_stats_tbl <- function (data_tb, key_var_nm_1L_chr = "round", make_descv_stats_tbl, 
    key_var_vals_chr, variable_nms_chr, test_1L_lgl = F) 
{
    descv_stats_tbl_tb <- make_tableby_ls(data_tb, key_var_nm_1L_chr = key_var_nm_1L_chr, 
        variable_nms_chr = variable_nms_chr, test_1L_lgl = test_1L_lgl) %>% 
        as.data.frame() %>% dplyr::select(c("label", tidyselect::all_of(key_var_vals_chr), 
        ifelse(test_1L_lgl, "p.value", character(0))) %>% purrr::discard(is.na))
    return(descv_stats_tbl_tb)
}
#' Make item
#' @description make_item_plt() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make item plot. The function returns Item (a plot).
#' @param tfd_data_tb Transformed data (a tibble)
#' @param var_nm_1L_chr Variable name (a character vector of length one)
#' @param round_var_nm_1L_chr Round variable name (a character vector of length one), Default: 'round'
#' @param x_label_1L_chr X label (a character vector of length one)
#' @param y_label_1L_chr Y label (a character vector of length one), Default: 'Percentage'
#' @param fill_label_1L_chr Fill label (a character vector of length one), Default: 'Data collection'
#' @param y_scale_scl_fn Y scale scale (a function), Default: NULL
#' @param use_bw_theme_1L_lgl Use black and white theme (a logical vector of length one), Default: F
#' @param legend_position_1L_chr Legend position (a character vector of length one), Default: 'none'
#' @return Item (a plot)
#' @rdname make_item_plt
#' @export 
#' @importFrom ggplot2 ggplot aes_string geom_bar aes scale_y_continuous labs theme_bw theme scale_fill_manual
#' @importFrom rlang sym
#' @keywords internal
make_item_plt <- function (tfd_data_tb, var_nm_1L_chr, round_var_nm_1L_chr = "round", 
    x_label_1L_chr, y_label_1L_chr = "Percentage", fill_label_1L_chr = "Data collection", 
    y_scale_scl_fn = NULL, use_bw_theme_1L_lgl = F, legend_position_1L_chr = "none") 
{
    item_plt <- ggplot2::ggplot(tfd_data_tb, ggplot2::aes_string(var_nm_1L_chr)) + 
        ggplot2::geom_bar(ggplot2::aes(y = y, fill = !!rlang::sym(round_var_nm_1L_chr)), 
            stat = "identity", na.rm = TRUE, position = "dodge", 
            colour = "white", alpha = 0.7)
    if (!is.null(y_scale_scl_fn)) {
        item_plt <- item_plt + ggplot2::scale_y_continuous(labels = y_scale_scl_fn)
    }
    item_plt <- item_plt + ggplot2::labs(x = x_label_1L_chr, 
        y = y_label_1L_chr, fill = fill_label_1L_chr)
    if (use_bw_theme_1L_lgl) {
        item_plt <- item_plt + ggplot2::theme_bw()
    }
    item_plt <- item_plt + ggplot2::theme(legend.position = legend_position_1L_chr) + 
        ggplot2::scale_fill_manual(values = c("#de2d26", "#fc9272"))
    return(item_plt)
}
#' Make item resp plots
#' @description make_itm_resp_plts() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make item resp plots. The function returns Composite (a plot).
#' @param data_tb Data (a tibble)
#' @param col_nms_chr Column names (a character vector)
#' @param lbl_nms_chr Label names (a character vector)
#' @param plot_rows_cols_pair_int Plot rows columns pair (an integer vector)
#' @param heights_int Heights (an integer vector)
#' @param round_var_nm_1L_chr Round variable name (a character vector of length one), Default: 'round'
#' @return Composite (a plot)
#' @rdname make_itm_resp_plts
#' @export 
#' @importFrom scales percent_format
#' @importFrom gridExtra grid.arrange
#' @importFrom ggpubr ggarrange
make_itm_resp_plts <- function (data_tb, col_nms_chr, lbl_nms_chr, plot_rows_cols_pair_int, 
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
            round_var_nm_1L_chr = round_var_nm_1L_chr, x_label_1L_chr = labelx, 
            y_scale_scl_fn = scales::percent_format(), use_bw_theme_1L_lgl = T, 
            legend_position_1L_chr = "none")
    }
    plot_plt <- make_item_plt(tfd_data_tb, var_nm_1L_chr = i, 
        round_var_nm_1L_chr = round_var_nm_1L_chr, x_label_1L_chr = labelx, 
        y_scale_scl_fn = NULL, use_bw_theme_1L_lgl = F, legend_position_1L_chr = "bottom")
    legend_ls <- extract_guide_box_lgd(plot_plt)
    composite_plt <- gridExtra::grid.arrange(ggpubr::ggarrange(plotlist = plots_ls, 
        nrow = plot_rows_cols_pair_int[1], ncol = plot_rows_cols_pair_int[2]), 
        legend_ls, nrow = length(heights_int), heights = heights_int)
    return(composite_plt)
}
#' Make sub total plots
#' @description make_sub_tot_plts() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make sub total plots. The function returns Composite (a plot).
#' @param data_tb Data (a tibble)
#' @param col_nms_chr Column names (a character vector)
#' @param plot_rows_cols_pair_int Plot rows columns pair (an integer vector)
#' @param round_var_nm_1L_chr Round variable name (a character vector of length one), Default: 'round'
#' @param make_log_log_tfmn_1L_lgl Make log log transformation (a logical vector of length one), Default: F
#' @param heights_int Heights (an integer vector)
#' @return Composite (a plot)
#' @rdname make_sub_tot_plts
#' @export 
#' @importFrom dplyr mutate
#' @importFrom stringr str_sub
#' @importFrom stringi stri_locate_last_fixed
#' @importFrom gridExtra grid.arrange
#' @importFrom ggpubr ggarrange
make_sub_tot_plts <- function (data_tb, col_nms_chr, plot_rows_cols_pair_int, round_var_nm_1L_chr = "round", 
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
            x_label_1L_chr = labelx)
    }
    plot_for_lgd_plt <- make_subtotal_plt(data_tb, var_nm_1L_chr = i, 
        x_label_1L_chr = labelx, legend_position_1L_chr = "bottom", 
        label_fill_1L_chr = "Data collection")
    legend_ls <- extract_guide_box_lgd(plot_for_lgd_plt)
    composite_plt <- gridExtra::grid.arrange(ggpubr::ggarrange(plotlist = plots_ls, 
        nrow = plot_rows_cols_pair_int[1], ncol = plot_rows_cols_pair_int[2]), 
        legend_ls, nrow = length(heights_int), heights = heights_int)
    return(composite_plt)
}
#' Make subtotal
#' @description make_subtotal_plt() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make subtotal plot. The function returns Subtotal (a plot).
#' @param data_tb Data (a tibble)
#' @param var_nm_1L_chr Variable name (a character vector of length one)
#' @param round_var_nm_1L_chr Round variable name (a character vector of length one), Default: 'round'
#' @param x_label_1L_chr X label (a character vector of length one)
#' @param y_label_1L_chr Y label (a character vector of length one), Default: 'Percentage'
#' @param y_scale_scl_fn Y scale scale (a function), Default: scales::percent
#' @param use_bw_theme_1L_lgl Use black and white theme (a logical vector of length one), Default: T
#' @param legend_position_1L_chr Legend position (a character vector of length one), Default: 'none'
#' @param label_fill_1L_chr Label fill (a character vector of length one), Default: NULL
#' @return Subtotal (a plot)
#' @rdname make_subtotal_plt
#' @export 
#' @importFrom scales percent
#' @importFrom ggplot2 ggplot aes_string geom_histogram aes labs theme_bw scale_y_continuous theme scale_fill_manual
#' @importFrom rlang sym
#' @keywords internal
make_subtotal_plt <- function (data_tb, var_nm_1L_chr, round_var_nm_1L_chr = "round", 
    x_label_1L_chr, y_label_1L_chr = "Percentage", y_scale_scl_fn = scales::percent, 
    use_bw_theme_1L_lgl = T, legend_position_1L_chr = "none", 
    label_fill_1L_chr = NULL) 
{
    subtotal_plt <- ggplot2::ggplot(data_tb, ggplot2::aes_string(var_nm_1L_chr)) + 
        ggplot2::geom_histogram(bins = 8, color = "white", ggplot2::aes(fill = !!rlang::sym(round_var_nm_1L_chr), 
            y = 2 * (..density..)/sum(..density..)), position = "dodge", 
            alpha = 0.7)
    subtotal_plt <- subtotal_plt + ggplot2::labs(x = x_label_1L_chr, 
        y = y_label_1L_chr, fill = label_fill_1L_chr)
    if (use_bw_theme_1L_lgl) {
        subtotal_plt <- subtotal_plt + ggplot2::theme_bw()
    }
    if (!is.null(y_scale_scl_fn)) {
        subtotal_plt <- subtotal_plt + ggplot2::scale_y_continuous(labels = y_scale_scl_fn)
    }
    subtotal_plt <- subtotal_plt + ggplot2::theme(legend.position = legend_position_1L_chr) + 
        ggplot2::scale_fill_manual(values = c("#de2d26", "#fc9272"))
    return(subtotal_plt)
}
#' Make tableby cntrls
#' @description make_tableby_cntrls() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make tableby cntrls. The function returns Tableby cntrls (a list).
#' @param test_1L_lgl Test (a logical vector of length one), Default: F
#' @return Tableby cntrls (a list)
#' @rdname make_tableby_cntrls
#' @export 
#' @importFrom arsenal tableby.control
#' @keywords internal
make_tableby_cntrls <- function (test_1L_lgl = F) 
{
    tableby_cntrls_ls <- arsenal::tableby.control(test = test_1L_lgl, 
        total = F, digits = 1, digits.pct = 1, digits.p = 3, 
        numeric.test = "anova", cat.test = "chisq", numeric.stats = c("meansd", 
            "medianq1q3", "range", "Nmiss2"), cat.stats = c("countpct", 
            "Nmiss2"), ordered.stats = c("countpct", "Nmiss2"), 
        stats.labels = list(meansd = "Mean (SD)", medianq1q3 = "Median (Q1, Q3)", 
            range = "Min - Max", Nmiss2 = "Missing"))
    return(tableby_cntrls_ls)
}
#' Make tableby
#' @description make_tableby_ls() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make tableby list. The function returns Tableby (a list).
#' @param data_tb Data (a tibble)
#' @param key_var_nm_1L_chr Key variable name (a character vector of length one)
#' @param variable_nms_chr Variable names (a character vector)
#' @param test_1L_lgl Test (a logical vector of length one), Default: F
#' @return Tableby (a list)
#' @rdname make_tableby_ls
#' @export 
#' @importFrom arsenal tableby
#' @keywords internal
make_tableby_ls <- function (data_tb, key_var_nm_1L_chr, variable_nms_chr, test_1L_lgl = F) 
{
    forumla_fml <- make_formula(key_var_nm_1L_chr, predictors_chr = variable_nms_chr)
    tableby_ls <- arsenal::tableby(forumla_fml, data = data_tb, 
        control = make_tableby_cntrls(test_1L_lgl))
    return(tableby_ls)
}
#' Make transformed replication dataset dictionary
#' @description make_tfd_repln_ds_dict_r3() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make transformed replication dataset dictionary ready4 s3. The function returns Transformed replication dataset dictionary (a ready4 S3).
#' @param repln_ds_dict_r3 Replication dataset dictionary (a ready4 S3), Default: NULL
#' @return Transformed replication dataset dictionary (a ready4 S3)
#' @rdname make_tfd_repln_ds_dict_r3
#' @export 
#' @importFrom dplyr mutate across case_when
#' @importFrom Hmisc label
make_tfd_repln_ds_dict_r3 <- function (repln_ds_dict_r3 = NULL) 
{
    if (is.null(repln_ds_dict_r3)) {
        data("repln_ds_dict_r3", package = "youthvars", envir = environment())
    }
    tfd_repln_ds_dict_r3 <- repln_ds_dict_r3 %>% dplyr::mutate(dplyr::across(.fns = as.character)) %>% 
        dplyr::mutate(var_nm_chr = dplyr::case_when(var_nm_chr == 
            "phq9_total" ~ "PHQ9", var_nm_chr == "bads_total" ~ 
            "BADS", var_nm_chr == "gad7_total" ~ "GAD7", var_nm_chr == 
            "oasis_total" ~ "OASIS", var_nm_chr == "scared_total" ~ 
            "SCARED", var_nm_chr == "k6_total" ~ "K6", var_nm_chr == 
            "c_sofas" ~ "SOFAS", T ~ var_nm_chr))
    Hmisc::label(tfd_repln_ds_dict_r3) = as.list(c("Variable", 
        "Category", "Description", "Class"))
    return(tfd_repln_ds_dict_r3)
}
#' Make variable by round
#' @description make_var_by_round_plt() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make variable by round plot. The function returns Variable by round (a plot).
#' @param data_tb Data (a tibble)
#' @param var_nm_1L_chr Variable name (a character vector of length one)
#' @param round_var_nm_1L_chr Round variable name (a character vector of length one), Default: 'round'
#' @param x_label_1L_chr X label (a character vector of length one)
#' @param y_label_1L_chr Y label (a character vector of length one), Default: 'Percentage'
#' @param y_scale_scl_fn Y scale scale (a function), Default: scales::percent
#' @param label_fill_1L_chr Label fill (a character vector of length one), Default: 'Data collection'
#' @return Variable by round (a plot)
#' @rdname make_var_by_round_plt
#' @export 
#' @importFrom scales percent
#' @importFrom ggplot2 ggplot aes theme_bw geom_histogram scale_y_continuous labs scale_fill_manual theme
#' @importFrom rlang sym
#' @keywords internal
make_var_by_round_plt <- function (data_tb, var_nm_1L_chr, round_var_nm_1L_chr = "round", 
    x_label_1L_chr, y_label_1L_chr = "Percentage", y_scale_scl_fn = scales::percent, 
    label_fill_1L_chr = "Data collection") 
{
    var_by_round_plt <- ggplot2::ggplot(data_tb, ggplot2::aes(x = !!rlang::sym(var_nm_1L_chr), 
        fill = !!rlang::sym(round_var_nm_1L_chr))) + ggplot2::theme_bw() + 
        ggplot2::geom_histogram(ggplot2::aes(y = stat(width * 
            density)), bins = 10, position = "dodge", colour = "white", 
            alpha = 0.7)
    if (!is.null(y_scale_scl_fn)) {
        var_by_round_plt <- var_by_round_plt + ggplot2::scale_y_continuous(labels = y_scale_scl_fn)
    }
    var_by_round_plt <- var_by_round_plt + ggplot2::labs(y = y_label_1L_chr, 
        x = x_label_1L_chr, fill = label_fill_1L_chr) + ggplot2::scale_fill_manual(values = c("#de2d26", 
        "#fc9272")) + ggplot2::theme(legend.position = "bottom")
    return(var_by_round_plt)
}
