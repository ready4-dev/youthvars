#' 
#' Depict (plot) features of model module data
#' @name depict-YouthvarsSeries
#' @description depict method applied to YouthvarsSeries
#' @param x An object of class YouthvarsSeries
#' @param x_vars_chr X variables (a character vector), Default: character(0)
#' @param y_vars_chr Y variables (a character vector), Default: character(0)
#' @param z_vars_chr Z variables (a character vector), Default: character(0)
#' @param arrange_1L_lgl Arrange (a logical vector of length one), Default: FALSE
#' @param arrange_args_ls Arrange arguments (a list), Default: list()
#' @param as_percent_1L_lgl As percent (a logical vector of length one), Default: FALSE
#' @param colours_chr Colours (a character vector), Default: c("#de2d26", "#fc9272")
#' @param drop_legend_1L_lgl Drop legend (a logical vector of length one), Default: FALSE
#' @param drop_missing_1L_lgl Drop missing (a logical vector of length one), Default: FALSE
#' @param drop_ticks_1L_lgl Drop ticks (a logical vector of length one), Default: FALSE
#' @param fill_single_1L_lgl Fill single (a logical vector of length one), Default: FALSE
#' @param flip_1L_lgl Flip (a logical vector of length one), Default: F
#' @param line_1L_chr Line (a character vector of length one), Default: 'black'
#' @param position_xx Position (an output object of multiple potential types), Default: NULL
#' @param recode_lup_r3 Recode (a ready4 submodule extension of lookup table), Default: ready4show::ready4show_correspondences()
#' @param significance_1L_lgl Significance (a logical vector of length one), Default: F
#' @param significance_args_ls Significance arguments (a list), Default: list()
#' @param style_1L_chr Style (a character vector of length one), Default: ready4use::get_styles()
#' @param titles_chr Titles (a character vector), Default: character(0)
#' @param type_1L_chr Type (a character vector of length one), Default: c("ggsci", "manual", "viridis", "by_time")
#' @param x_labels_chr X labels (a character vector), Default: character(0)
#' @param y_labels_chr Y labels (a character vector), Default: character(0)
#' @param z_labels_chr Z labels (a character vector), Default: character(0)
#' @param what_1L_chr What (a character vector of length one), Default: ready4use::get_journal_plot_fn("names")
#' @param label_fill_1L_chr Label fill (a character vector of length one), Default: deprecated()
#' @param labels_chr Labels (a character vector), Default: deprecated()
#' @param var_nms_chr Variable names (a character vector), Default: deprecated()
#' @param y_label_1L_chr Y label (a character vector of length one), Default: deprecated()
#' @param y_scale_scl_fn Y scale scale (a function), Default: deprecated()
#' @param ... Additional arguments
#' @return Plot (an output object of multiple potential types)
#' @rdname depict-methods
#' @aliases depict,YouthvarsSeries-method
#' @export 
#' @importFrom ready4show ready4show_correspondences
#' @importFrom ready4use get_styles get_journal_plot_fn
#' @importFrom lifecycle is_present deprecate_warn
#' @importFrom scales percent
#' @importFrom rlang exec
#' @importFrom ready4 depict
methods::setMethod("depict", "YouthvarsSeries", function (x, x_vars_chr = character(0), y_vars_chr = character(0), 
    z_vars_chr = character(0), arrange_1L_lgl = FALSE, arrange_args_ls = list(), 
    as_percent_1L_lgl = FALSE, colours_chr = c("#de2d26", "#fc9272"), 
    drop_legend_1L_lgl = FALSE, drop_missing_1L_lgl = FALSE, 
    drop_ticks_1L_lgl = FALSE, fill_single_1L_lgl = FALSE, flip_1L_lgl = F, 
    line_1L_chr = "black", position_xx = NULL, recode_lup_r3 = ready4show::ready4show_correspondences(), 
    significance_1L_lgl = F, significance_args_ls = list(), style_1L_chr = ready4use::get_styles(), 
    titles_chr = character(0), type_1L_chr = c("ggsci", "manual", 
        "viridis", "by_time"), x_labels_chr = character(0), y_labels_chr = character(0), 
    z_labels_chr = character(0), what_1L_chr = ready4use::get_journal_plot_fn("names"), 
    label_fill_1L_chr = deprecated(), labels_chr = deprecated(), 
    var_nms_chr = deprecated(), y_label_1L_chr = deprecated(), 
    y_scale_scl_fn = deprecated(), ...) 
{
    style_1L_chr <- match.arg(style_1L_chr)
    type_1L_chr <- match.arg(type_1L_chr)
    type_1L_chr <- ifelse(type_1L_chr == "by_time", "ggsci", 
        type_1L_chr)
    what_1L_chr <- match.arg(what_1L_chr)
    args_ls <- list(...)
    if (lifecycle::is_present(labels_chr)) {
        lifecycle::deprecate_warn("0.0.0.9127", "youthvars::depict(labels_chr)", 
            details = "Please use `youthvars::depict(x_labels_chr)` instead.")
        if (identical(x_labels_chr, character(0))) {
            x_labels_chr <- labels_chr
        }
    }
    if (lifecycle::is_present(var_nms_chr)) {
        lifecycle::deprecate_warn("0.0.0.9127", "youthvars::depict(var_nms_chr)", 
            details = "Please use `youthvars::depict(x_vars_chr)` instead.")
        if (identical(x_vars_chr, character(0))) {
            x_vars_chr <- var_nms_chr
            if (!is.null(y_scale_scl_fn)) {
                as_percent_1L_lgl <- TRUE
            }
            what_1L_chr <- "histogram"
            z_vars_chr <- x@timepoint_var_nm_1L_chr
            if (!"bins" %in% names(args_ls)) {
                args_ls$bins <- 10
            }
            if (!"position" %in% names(args_ls) & is.null(position_xx)) {
                args_ls$position <- "dodge"
            }
        }
    }
    if (lifecycle::is_present(y_scale_scl_fn)) {
        lifecycle::deprecate_warn("0.0.0.9127", "youthvars::depict(y_scale_scl_fn)", 
            details = "Please use `youthvars::depict(as_percent_1L_lgl)` instead.")
        if (identical(y_scale_scl_fn, scales::percent)) {
            as_percent_1L_lgl <- TRUE
        }
    }
    if (lifecycle::is_present(y_label_1L_chr)) {
        lifecycle::deprecate_warn("0.0.0.9127", "youthvars::depict(y_label_1L_chr)", 
            details = "Please use `youthvars::depict(y_labels_chr)` instead.")
        if (identical(y_labels_chr, character(0))) {
            y_labels_chr <- rep(y_label_1L_chr, length(x_vars_chr))
        }
    }
    if (lifecycle::is_present(label_fill_1L_chr)) {
        lifecycle::deprecate_warn("0.0.0.9127", "youthvars::depict(label_fill_1L_chr)", 
            details = "Please use `youthvars::depict(z_labels_chr)` instead.")
        if (identical(z_labels_chr, character(0))) {
            z_labels_chr <- rep(label_fill_1L_chr, length(x_vars_chr))
        }
    }
    args_ls <- append(list(slot_nm_1L_chr = "a_Ready4useDyad", 
        x_vars_chr = x_vars_chr, y_vars_chr = y_vars_chr, z_vars_chr = z_vars_chr, 
        arrange_1L_lgl = arrange_1L_lgl, arrange_args_ls = arrange_args_ls, 
        as_percent_1L_lgl = as_percent_1L_lgl, colours_chr = colours_chr, 
        drop_legend_1L_lgl = drop_legend_1L_lgl, drop_missing_1L_lgl = drop_missing_1L_lgl, 
        drop_ticks_1L_lgl = drop_ticks_1L_lgl, fill_single_1L_lgl = fill_single_1L_lgl, 
        flip_1L_lgl = flip_1L_lgl, line_1L_chr = line_1L_chr, 
        position_xx = position_xx, recode_lup_r3 = recode_lup_r3, 
        significance_1L_lgl = significance_1L_lgl, significance_args_ls = significance_args_ls, 
        style_1L_chr = style_1L_chr, titles_chr = titles_chr, 
        type_1L_chr = type_1L_chr, x_labels_chr = x_labels_chr, 
        y_labels_chr = y_labels_chr, z_labels_chr = z_labels_chr, 
        what_1L_chr = what_1L_chr), args_ls)
    plot_xx <- rlang::exec(depictSlot, x, !!!args_ls)
    return(plot_xx)
})
#' 
#' Depict (plot) features of model module data
#' @name depict-YouthvarsProfile
#' @description depict method applied to YouthvarsProfile
#' @param x An object of class YouthvarsProfile
#' @param x_vars_chr X variables (a character vector), Default: character(0)
#' @param y_vars_chr Y variables (a character vector), Default: character(0)
#' @param z_vars_chr Z variables (a character vector), Default: character(0)
#' @param arrange_1L_lgl Arrange (a logical vector of length one), Default: FALSE
#' @param arrange_args_ls Arrange arguments (a list), Default: list()
#' @param as_percent_1L_lgl As percent (a logical vector of length one), Default: FALSE
#' @param colours_chr Colours (a character vector), Default: c("#de2d26", "#fc9272")
#' @param drop_legend_1L_lgl Drop legend (a logical vector of length one), Default: FALSE
#' @param drop_missing_1L_lgl Drop missing (a logical vector of length one), Default: FALSE
#' @param drop_ticks_1L_lgl Drop ticks (a logical vector of length one), Default: FALSE
#' @param fill_single_1L_lgl Fill single (a logical vector of length one), Default: FALSE
#' @param flip_1L_lgl Flip (a logical vector of length one), Default: F
#' @param line_1L_chr Line (a character vector of length one), Default: 'black'
#' @param position_xx Position (an output object of multiple potential types), Default: NULL
#' @param recode_lup_r3 Recode (a ready4 submodule extension of lookup table), Default: ready4show::ready4show_correspondences()
#' @param significance_1L_lgl Significance (a logical vector of length one), Default: F
#' @param significance_args_ls Significance arguments (a list), Default: list()
#' @param style_1L_chr Style (a character vector of length one), Default: ready4use::get_styles()
#' @param titles_chr Titles (a character vector), Default: character(0)
#' @param type_1L_chr Type (a character vector of length one), Default: c("ggsci", "manual", "viridis")
#' @param x_labels_chr X labels (a character vector), Default: character(0)
#' @param y_labels_chr Y labels (a character vector), Default: character(0)
#' @param z_labels_chr Z labels (a character vector), Default: character(0)
#' @param what_1L_chr What (a character vector of length one), Default: ready4use::get_journal_plot_fn("names")
#' @param labels_chr Labels (a character vector), Default: deprecated()
#' @param var_nms_chr Variable names (a character vector), Default: deprecated()
#' @param y_label_1L_chr Y label (a character vector of length one), Default: deprecated()
#' @param y_scale_scl_fn Y scale scale (a function), Default: deprecated()
#' @param ... Additional arguments
#' @return Plot (an output object of multiple potential types)
#' @rdname depict-methods
#' @aliases depict,YouthvarsProfile-method
#' @export 
#' @importFrom ready4show ready4show_correspondences
#' @importFrom ready4use get_styles get_journal_plot_fn
#' @importFrom lifecycle is_present deprecate_warn
#' @importFrom scales percent
#' @importFrom rlang exec
#' @importFrom ready4 depict
methods::setMethod("depict", "YouthvarsProfile", function (x, x_vars_chr = character(0), y_vars_chr = character(0), 
    z_vars_chr = character(0), arrange_1L_lgl = FALSE, arrange_args_ls = list(), 
    as_percent_1L_lgl = FALSE, colours_chr = c("#de2d26", "#fc9272"), 
    drop_legend_1L_lgl = FALSE, drop_missing_1L_lgl = FALSE, 
    drop_ticks_1L_lgl = FALSE, fill_single_1L_lgl = FALSE, flip_1L_lgl = F, 
    line_1L_chr = "black", position_xx = NULL, recode_lup_r3 = ready4show::ready4show_correspondences(), 
    significance_1L_lgl = F, significance_args_ls = list(), style_1L_chr = ready4use::get_styles(), 
    titles_chr = character(0), type_1L_chr = c("ggsci", "manual", 
        "viridis"), x_labels_chr = character(0), y_labels_chr = character(0), 
    z_labels_chr = character(0), what_1L_chr = ready4use::get_journal_plot_fn("names"), 
    labels_chr = deprecated(), var_nms_chr = deprecated(), y_label_1L_chr = deprecated(), 
    y_scale_scl_fn = deprecated(), ...) 
{
    style_1L_chr <- match.arg(style_1L_chr)
    type_1L_chr <- match.arg(type_1L_chr)
    what_1L_chr <- match.arg(what_1L_chr)
    args_ls <- list(...)
    if (lifecycle::is_present(labels_chr)) {
        lifecycle::deprecate_warn("0.0.0.9127", "youthvars::depict(labels_chr)", 
            details = "Please use `youthvars::depict(x_labels_chr)` instead.")
        if (identical(x_labels_chr, character(0))) {
            x_labels_chr <- labels_chr
        }
    }
    if (lifecycle::is_present(var_nms_chr)) {
        lifecycle::deprecate_warn("0.0.0.9127", "youthvars::depict(var_nms_chr)", 
            details = "Please use `youthvars::depict(x_vars_chr)` instead.")
        if (identical(x_vars_chr, character(0))) {
            x_vars_chr <- var_nms_chr
            if (!is.null(y_scale_scl_fn)) {
                as_percent_1L_lgl <- TRUE
            }
            what_1L_chr <- "histogram"
            if (!"bins" %in% names(args_ls)) {
                args_ls$bins <- 10
            }
        }
    }
    if (lifecycle::is_present(y_scale_scl_fn)) {
        lifecycle::deprecate_warn("0.0.0.9127", "youthvars::depict(y_scale_scl_fn)", 
            details = "Please use `youthvars::depict(as_percent_1L_lgl)` instead.")
        if (identical(y_scale_scl_fn, scales::percent)) {
            as_percent_1L_lgl <- TRUE
        }
    }
    if (lifecycle::is_present(y_label_1L_chr)) {
        lifecycle::deprecate_warn("0.0.0.9127", "youthvars::depict(y_label_1L_chr)", 
            details = "Please use `youthvars::depict(y_labels_chr)` instead.")
        if (identical(y_labels_chr, character(0))) {
            y_labels_chr <- rep(y_label_1L_chr, length(x_vars_chr))
        }
    }
    args_ls <- append(list(slot_nm_1L_chr = "a_Ready4useDyad", 
        x_vars_chr = x_vars_chr, y_vars_chr = y_vars_chr, z_vars_chr = z_vars_chr, 
        arrange_1L_lgl = arrange_1L_lgl, arrange_args_ls = arrange_args_ls, 
        as_percent_1L_lgl = as_percent_1L_lgl, colours_chr = colours_chr, 
        drop_legend_1L_lgl = drop_legend_1L_lgl, drop_missing_1L_lgl = drop_missing_1L_lgl, 
        drop_ticks_1L_lgl = drop_ticks_1L_lgl, fill_single_1L_lgl = fill_single_1L_lgl, 
        flip_1L_lgl = flip_1L_lgl, line_1L_chr = line_1L_chr, 
        position_xx = position_xx, recode_lup_r3 = recode_lup_r3, 
        significance_1L_lgl = significance_1L_lgl, significance_args_ls = significance_args_ls, 
        style_1L_chr = style_1L_chr, titles_chr = titles_chr, 
        type_1L_chr = type_1L_chr, x_labels_chr = x_labels_chr, 
        y_labels_chr = y_labels_chr, z_labels_chr = z_labels_chr, 
        what_1L_chr = what_1L_chr), args_ls)
    plot_xx <- rlang::exec(depictSlot, x, !!!args_ls)
    return(plot_xx)
})
