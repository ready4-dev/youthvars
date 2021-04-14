get_guide_box_lgd <- function (plot_plt)
{
    tmp <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(plot_plt))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend_ls <- tmp$grobs[[leg]]
    return(legend_ls)
}
