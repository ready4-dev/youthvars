get_guide_box_lgd <- function (plot_plt)
{
    tmp <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(plot_plt))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend_ls <- tmp$grobs[[leg]]
    return(legend_ls)
}
## DEPRECATED FNS
get_aqol6d_scrg_dss <- function(){
  lifecycle::deprecate_soft("0.0.0.9078",
                            "youthvars::get_aqol6d_scrg_dss()",
                            "scorz::get_aqol6d_scrg_dss()")
  aqol6d_scrg_dss_ls <- ready4::ingest(ready4use::Ready4useRepos(gh_repo_1L_chr = "ready4-dev/scorz",
                                                                 gh_tag_1L_chr = "Documentation_0.0"),
                                       fls_to_ingest_chr = c("aqol6d_scrg_dss_ls")) %>%
    ready4::procureSlot("b_Ready4useIngest") %>%
    ready4::procure(fl_nm_1L_chr = "aqol6d_scrg_dss_ls")
  return(aqol6d_scrg_dss_ls)
}
