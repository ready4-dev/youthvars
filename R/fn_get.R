#' Get guide box legend
#' @description get_guide_box_lgd() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get guide box legend. Function argument plot_plt specifies the where to look for the required object. The function returns Legend (a list).
#' @param plot_plt Plot (a plot)
#' @return Legend (a list)
#' @rdname get_guide_box_lgd
#' @export 
#' @importFrom ggplot2 ggplot_gtable ggplot_build
#' @keywords internal
get_guide_box_lgd <- function (plot_plt) 
{
    tmp <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(plot_plt))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend_ls <- tmp$grobs[[leg]]
    return(legend_ls)
}
