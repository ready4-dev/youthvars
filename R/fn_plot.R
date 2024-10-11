#' Plot descriptives
#' @description plot_descriptives() is a Plot function that plots data. Specifically, this function implements an algorithm to plot descriptives. The function returns Plot (a plot).
#' @param X_YouthvarsProfile PARAM_DESCRIPTION
#' @param what_1L_chr What (a character vector of length one)
#' @param legend_1L_lgl Legend (a logical vector of length one), Default: FALSE
#' @param missing_1L_chr Missing (a character vector of length one), Default: 'grey50'
#' @param palette_1L_chr Palette (a character vector of length one), Default: 'lancet'
#' @param type_1L_chr Type (a character vector of length one), Default: c("ggsci", "viridis")
#' @param x_ready4show_correspondences PARAM_DESCRIPTION, Default: ready4show::ready4show_correspondences()
#' @param y_label_1L_chr Y label (a character vector of length one), Default: 'Count'
#' @return Plot (a plot)
#' @rdname plot_descriptives
#' @export 
#' @importFrom ready4show ready4show_correspondences manufacture.ready4show_correspondences
#' @importFrom dplyr filter mutate select
#' @importFrom rlang sym
#' @importFrom ggplot2 ggplot aes geom_bar ylab theme_minimal theme element_blank guides guide_legend
plot_descriptives <- function (X_YouthvarsProfile, what_1L_chr, legend_1L_lgl = FALSE, 
    missing_1L_chr = "grey50", palette_1L_chr = "lancet", type_1L_chr = c("ggsci", 
        "viridis"), x_ready4show_correspondences = ready4show::ready4show_correspondences(), 
    y_label_1L_chr = "Count") 
{
    type_1L_chr <- match.arg(type_1L_chr)
    ds_tb <- X_YouthvarsProfile@a_Ready4useDyad@ds_tb
    if (is.na(missing_1L_chr)) {
        ds_tb <- ds_tb %>% dplyr::filter(!is.na(!!rlang::sym(what_1L_chr)))
    }
    if (!identical(x_ready4show_correspondences, ready4show::ready4show_correspondences())) {
        ds_tb <- ds_tb %>% dplyr::mutate(`:=`(!!rlang::sym(what_1L_chr), 
            x_ready4show_correspondences %>% ready4show::manufacture.ready4show_correspondences(ds_tb %>% 
                dplyr::select(!!rlang::sym(what_1L_chr)), flatten_1L_lgl = TRUE)))
    }
    plot_plt <- ggplot2::ggplot(data = ds_tb, ggplot2::aes(x = !!rlang::sym(what_1L_chr), 
        fill = factor(!!rlang::sym(what_1L_chr)))) + ggplot2::geom_bar() + 
        ggplot2::ylab(y_label_1L_chr) + ggplot2::theme_minimal()
    if (!legend_1L_lgl) {
        plot_plt <- plot_plt + ggplot2::theme(legend.position = "none")
    }
    else {
        plot_plt <- plot_plt + ggplot2::theme(axis.text.x = ggplot2::element_blank(), 
            axis.ticks.x = ggplot2::element_blank()) + ggplot2::guides(fill = ggplot2::guide_legend(title = what_1L_chr))
    }
    plot_plt <- plot_plt %>% add_discrete_palette(missing_1L_chr = missing_1L_chr, 
        type_1L_chr = type_1L_chr, what_1L_chr = palette_1L_chr)
    return(plot_plt)
}
