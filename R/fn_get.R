#' Get Assessment of Quality of Life Six Dimension scoring datasets
#' @description get_aqol6d_scrg_dss() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get assessment of quality of life six dimension scoring datasets. The function returns Assessment of Quality of Life Six Dimension scoring datasets (a list).

#' @return Assessment of Quality of Life Six Dimension scoring datasets (a list)
#' @rdname get_aqol6d_scrg_dss
#' @export 
#' @importFrom lifecycle deprecate_soft
#' @importFrom ready4 ingest procureSlot procure
#' @importFrom ready4use Ready4useRepos
#' @keywords internal
get_aqol6d_scrg_dss <- function () 
{
    lifecycle::deprecate_soft("0.0.0.9078", "youthvars::get_aqol6d_scrg_dss()", 
        "scorz::get_aqol6d_scrg_dss()")
    aqol6d_scrg_dss_ls <- ready4::ingest(ready4use::Ready4useRepos(gh_repo_1L_chr = "ready4-dev/scorz", 
        gh_tag_1L_chr = "Documentation_0.0"), fls_to_ingest_chr = c("aqol6d_scrg_dss_ls")) %>% 
        ready4::procureSlot("b_Ready4useIngest") %>% ready4::procure(fl_nm_1L_chr = "aqol6d_scrg_dss_ls")
    return(aqol6d_scrg_dss_ls)
}
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
