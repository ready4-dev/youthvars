#' Get Assessment of Quality of Life Six Dimension scoring datasets
#' @description get_aqol6d_scrg_dss() is a Get function that extracts data from an object. Specifically, this function implements an algorithm to get assessment of quality of life six dimension scoring datasets. The function returns Assessment of Quality of Life Six Dimension scoring datasets (a list).

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
#' @description get_guide_box_lgd() is a Get function that extracts data from an object. Specifically, this function implements an algorithm to get guide box legend. The function returns Legend (a list).
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
#' Get journal palette function
#' @description get_journal_palette_fn() is a Get function that extracts data from an object. Specifically, this function implements an algorithm to get journal palette function. The function returns Journal palette (a function).
#' @param type_1L_chr Type (a character vector of length one), Default: c("colour", "fill")
#' @param what_1L_chr What (a character vector of length one), Default: 'lancet'
#' @return Journal palette (a function)
#' @rdname get_journal_palette_fn
#' @export 
#' @importFrom ggsci scale_colour_aaas scale_colour_bmj scale_colour_bs5 scale_colour_cosmic scale_colour_d3 scale_colour_flatui scale_colour_frontiers scale_colour_futurama scale_colour_gsea scale_colour_igv scale_colour_jama scale_colour_jco scale_colour_lancet scale_colour_locuszoom scale_colour_material scale_colour_nejm scale_colour_npg scale_colour_observable scale_colour_rickandmorty scale_colour_simpsons scale_colour_startrek scale_colour_tron scale_colour_tw3 scale_colour_uchicago scale_colour_ucscgb scale_fill_aaas scale_fill_bmj scale_fill_bs5 scale_fill_cosmic scale_fill_d3 scale_fill_flatui scale_fill_frontiers scale_fill_futurama scale_fill_gsea scale_fill_igv scale_fill_jama scale_fill_jco scale_fill_lancet scale_fill_locuszoom scale_fill_material scale_fill_nejm scale_fill_npg scale_fill_observable scale_fill_rickandmorty scale_fill_simpsons scale_fill_startrek scale_fill_tron scale_fill_tw3 scale_fill_uchicago scale_fill_ucscgb
#' @importFrom purrr pluck
#' @keywords internal
get_journal_palette_fn <- function (type_1L_chr = c("colour", "fill"), what_1L_chr = "lancet") 
{
    type_1L_chr <- match.arg(type_1L_chr)
    options_ls <- list(scale_colour_aaas = ggsci::scale_colour_aaas, 
        scale_colour_bmj = ggsci::scale_colour_bmj, scale_colour_bs5 = ggsci::scale_colour_bs5, 
        scale_colour_cosmic = ggsci::scale_colour_cosmic, scale_colour_d3 = ggsci::scale_colour_d3, 
        scale_colour_flatui = ggsci::scale_colour_flatui, scale_colour_frontiers = ggsci::scale_colour_frontiers, 
        scale_colour_futurama = ggsci::scale_colour_futurama, 
        scale_colour_gsea = ggsci::scale_colour_gsea, scale_colour_igv = ggsci::scale_colour_igv, 
        scale_colour_jama = ggsci::scale_colour_jama, scale_colour_jco = ggsci::scale_colour_jco, 
        scale_colour_lancet = ggsci::scale_colour_lancet, scale_colour_locuszoom = ggsci::scale_colour_locuszoom, 
        scale_colour_material = ggsci::scale_colour_material, 
        scale_colour_nejm = ggsci::scale_colour_nejm, scale_colour_npg = ggsci::scale_colour_npg, 
        scale_colour_observable = ggsci::scale_colour_observable, 
        scale_colour_rickandmorty = ggsci::scale_colour_rickandmorty, 
        scale_colour_simpsons = ggsci::scale_colour_simpsons, 
        scale_colour_startrek = ggsci::scale_colour_startrek, 
        scale_colour_tron = ggsci::scale_colour_tron, scale_colour_tw3 = ggsci::scale_colour_tw3, 
        scale_colour_uchicago = ggsci::scale_colour_uchicago, 
        scale_colour_ucscgb = ggsci::scale_colour_ucscgb, scale_fill_aaas = ggsci::scale_fill_aaas, 
        scale_fill_bmj = ggsci::scale_fill_bmj, scale_fill_bs5 = ggsci::scale_fill_bs5, 
        scale_fill_cosmic = ggsci::scale_fill_cosmic, scale_fill_d3 = ggsci::scale_fill_d3, 
        scale_fill_flatui = ggsci::scale_fill_flatui, scale_fill_frontiers = ggsci::scale_fill_frontiers, 
        scale_fill_futurama = ggsci::scale_fill_futurama, scale_fill_gsea = ggsci::scale_fill_gsea, 
        scale_fill_igv = ggsci::scale_fill_igv, scale_fill_jama = ggsci::scale_fill_jama, 
        scale_fill_jco = ggsci::scale_fill_jco, scale_fill_lancet = ggsci::scale_fill_lancet, 
        scale_fill_locuszoom = ggsci::scale_fill_locuszoom, scale_fill_material = ggsci::scale_fill_material, 
        scale_fill_nejm = ggsci::scale_fill_nejm, scale_fill_npg = ggsci::scale_fill_npg, 
        scale_fill_observable = ggsci::scale_fill_observable, 
        scale_fill_rickandmorty = ggsci::scale_fill_rickandmorty, 
        scale_fill_simpsons = ggsci::scale_fill_simpsons, scale_fill_startrek = ggsci::scale_fill_startrek, 
        scale_fill_tron = ggsci::scale_fill_tron, scale_fill_tw3 = ggsci::scale_fill_tw3, 
        scale_fill_uchicago = ggsci::scale_fill_uchicago, scale_fill_ucscgb = ggsci::scale_fill_ucscgb)
    journal_palette_fn <- options_ls %>% purrr::pluck(paste0("scale_", 
        type_1L_chr, "_", what_1L_chr))
    return(journal_palette_fn)
}
