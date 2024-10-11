plot_descriptives <- function(X_YouthvarsProfile,
                              what_1L_chr,
                              legend_1L_lgl = FALSE,
                              missing_1L_chr = "grey50",
                              palette_1L_chr = "lancet",
                              type_1L_chr = c("ggsci", "viridis"),
                              x_ready4show_correspondences = ready4show::ready4show_correspondences(),
                              y_label_1L_chr = "Count"){
  type_1L_chr <- match.arg(type_1L_chr)
  ds_tb <- X_YouthvarsProfile@a_Ready4useDyad@ds_tb
  if(is.na(missing_1L_chr)){
    ds_tb <- ds_tb %>%
      dplyr::filter(!is.na(!!rlang::sym(what_1L_chr)))
  }
  if(!identical(x_ready4show_correspondences, ready4show::ready4show_correspondences())){
    ds_tb <- ds_tb %>% dplyr::mutate(!!rlang::sym(what_1L_chr) := x_ready4show_correspondences %>% ready4show::manufacture.ready4show_correspondences(ds_tb %>% dplyr::select(!!rlang::sym(what_1L_chr)), flatten_1L_lgl = TRUE))
  }
  plot_plt <- ggplot2::ggplot(data = ds_tb,
                              ggplot2::aes(x = !!rlang::sym(what_1L_chr), fill = factor(!!rlang::sym(what_1L_chr)))) +
    ggplot2::geom_bar() +
    ggplot2::ylab(y_label_1L_chr) +
    ggplot2::theme_minimal()
  if(!legend_1L_lgl){
    plot_plt <-  plot_plt +
      ggplot2::theme(legend.position="none")
  }else{
    plot_plt <-  plot_plt +
      ggplot2::theme(#axis.title.x=ggplot2::element_blank(),
        axis.text.x=ggplot2::element_blank(),
        axis.ticks.x=ggplot2::element_blank()) +
      ggplot2::guides(fill = ggplot2::guide_legend(title = what_1L_chr))
  }
  plot_plt <- plot_plt %>% add_discrete_palette(missing_1L_chr = missing_1L_chr,
                                                type_1L_chr = type_1L_chr,
                                                what_1L_chr = palette_1L_chr)
  return(plot_plt)

}
