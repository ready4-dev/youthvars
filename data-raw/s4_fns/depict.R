depict_YouthvarsProfile <- function(x,
                                    x_vars_chr = character(0),
                                    y_vars_chr = character(0),
                                    z_vars_chr = character(0),
                                    arrange_1L_lgl = FALSE,
                                    arrange_args_ls = list(),
                                    as_percent_1L_lgl = FALSE,
                                    colours_chr = c("#de2d26","#fc9272"),
                                    drop_legend_1L_lgl = FALSE,
                                    drop_missing_1L_lgl = FALSE,
                                    drop_ticks_1L_lgl = FALSE,
                                    fill_single_1L_lgl = FALSE,
                                    line_1L_chr = "black",
                                    position_xx = NULL,
                                    recode_lup_r3 = ready4show::ready4show_correspondences(),
                                    style_1L_chr = ready4use::get_styles(),
                                    titles_chr = character(0),
                                    type_1L_chr = c("ggsci", "manual", "viridis"),
                                    x_labels_chr = character(0),
                                    y_labels_chr = character(0),
                                    z_labels_chr = character(0),
                                    what_1L_chr =  ready4use::get_journal_plot_fn("names"),
                                    labels_chr = deprecated(), #NA_character_
                                    var_nms_chr = deprecated(),
                                    y_label_1L_chr = deprecated(), # character(0)
                                    y_scale_scl_fn = deprecated(), #scales::percent, #
                                    ...){
  style_1L_chr <- match.arg(style_1L_chr)
  type_1L_chr <- match.arg(type_1L_chr)
  what_1L_chr <- match.arg(what_1L_chr)
  args_ls <- list(...)
  if(lifecycle::is_present(labels_chr)) {
    lifecycle::deprecate_warn("0.0.0.9127",
                              "youthvars::depict(labels_chr)",
                              details = "Please use `youthvars::depict(x_labels_chr)` instead.")
    if(identical(x_labels_chr, character(0))){#is.na(labels_chr[1]) &
      x_labels_chr <- labels_chr
    }
  }
  if(lifecycle::is_present(var_nms_chr)) {
    lifecycle::deprecate_warn("0.0.0.9127",
                              "youthvars::depict(var_nms_chr)",
                              details = "Please use `youthvars::depict(x_vars_chr)` instead.")
    if(identical(x_vars_chr, character(0))){
      x_vars_chr <- var_nms_chr
      if(!is.null(y_scale_scl_fn)){
        as_percent_1L_lgl <- TRUE
      }
      what_1L_chr <- "histogram"
      if(!"bins" %in% names(args_ls)){
        args_ls$bins <- 10
      }
    }
  }
  if(lifecycle::is_present(y_scale_scl_fn)) {
    lifecycle::deprecate_warn("0.0.0.9127",
                              "youthvars::depict(y_scale_scl_fn)",
                              details = "Please use `youthvars::depict(as_percent_1L_lgl)` instead.")
    if(identical(y_scale_scl_fn, scales::percent)){
      as_percent_1L_lgl <- TRUE
    }
  }
  if(lifecycle::is_present(y_label_1L_chr)) {
    lifecycle::deprecate_warn("0.0.0.9127",
                              "youthvars::depict(y_label_1L_chr)",
                              details = "Please use `youthvars::depict(y_labels_chr)` instead.")
    if(identical(y_labels_chr, character(0))){
      y_labels_chr <- rep(y_label_1L_chr, length(x_vars_chr))
    }
  }
  args_ls <- append(list(slot_nm_1L_chr = "a_Ready4useDyad",
                         x_vars_chr = x_vars_chr,
                         y_vars_chr = y_vars_chr,
                         z_vars_chr = z_vars_chr,
                         arrange_1L_lgl = arrange_1L_lgl,
                         arrange_args_ls = arrange_args_ls,
                         as_percent_1L_lgl = as_percent_1L_lgl,
                         colours_chr = colours_chr,
                         drop_legend_1L_lgl = drop_legend_1L_lgl,
                         drop_missing_1L_lgl = drop_missing_1L_lgl,
                         drop_ticks_1L_lgl = drop_ticks_1L_lgl,
                         fill_single_1L_lgl = fill_single_1L_lgl,
                         line_1L_chr = line_1L_chr,
                         position_xx = position_xx,
                         recode_lup_r3 = recode_lup_r3,
                         style_1L_chr = style_1L_chr,
                         titles_chr = titles_chr,
                         type_1L_chr = type_1L_chr,
                         x_labels_chr = x_labels_chr ,
                         y_labels_chr = y_labels_chr,
                         z_labels_chr = z_labels_chr,
                         what_1L_chr =  what_1L_chr),
                    args_ls)
  plot_xx <- rlang::exec(depictSlot, x, !!!args_ls)

  return(plot_xx)
}
depict_YouthvarsSeries <- function(x,
                                   x_vars_chr = character(0),
                                   y_vars_chr = character(0),
                                   z_vars_chr = character(0),
                                   arrange_1L_lgl = FALSE,
                                   arrange_args_ls = list(),
                                   as_percent_1L_lgl = FALSE,
                                   colours_chr = c("#de2d26","#fc9272"),
                                   drop_legend_1L_lgl = FALSE,
                                   drop_missing_1L_lgl = FALSE,
                                   drop_ticks_1L_lgl = FALSE,
                                   fill_single_1L_lgl = FALSE,
                                   line_1L_chr = "black",
                                   position_xx = NULL,
                                   recode_lup_r3 = ready4show::ready4show_correspondences(),
                                   style_1L_chr = ready4use::get_styles(),
                                   titles_chr = character(0),
                                   type_1L_chr = c("ggsci", "manual", "viridis", "by_time"),
                                   x_labels_chr = character(0),
                                   y_labels_chr = character(0),
                                   z_labels_chr = character(0),
                                   what_1L_chr =  ready4use::get_journal_plot_fn("names"),
                                   label_fill_1L_chr = deprecated(), #"Data collection",
                                   labels_chr = deprecated(), #NA_character_
                                   var_nms_chr = deprecated(),
                                   y_label_1L_chr = deprecated(), # character(0)
                                   y_scale_scl_fn = deprecated(), #scales::percent, #
                                   # type_1L_chr = "by_time",
                                   # labels_chr = NA_character_,
                                   ...){

  ## Filter for specific time-point at_time_1L_xx ==
  ## Default y or by to time variable by_time_1L_lgl = T/F

  style_1L_chr <- match.arg(style_1L_chr)
  type_1L_chr <- match.arg(type_1L_chr)
  type_1L_chr <- ifelse(type_1L_chr=="by_time","ggsci",type_1L_chr)
  what_1L_chr <- match.arg(what_1L_chr)
  args_ls <- list(...)
  if(lifecycle::is_present(labels_chr)) {
    lifecycle::deprecate_warn("0.0.0.9127",
                              "youthvars::depict(labels_chr)",
                              details = "Please use `youthvars::depict(x_labels_chr)` instead.")
    if(identical(x_labels_chr, character(0))){#is.na(labels_chr[1]) &
      x_labels_chr <- labels_chr
    }
  }
  if(lifecycle::is_present(var_nms_chr)) {
    lifecycle::deprecate_warn("0.0.0.9127",
                              "youthvars::depict(var_nms_chr)",
                              details = "Please use `youthvars::depict(x_vars_chr)` instead.")
    if(identical(x_vars_chr, character(0))){
      x_vars_chr <- var_nms_chr
      if(!is.null(y_scale_scl_fn)){
        as_percent_1L_lgl <- TRUE
      }
      what_1L_chr <- "histogram"
      z_vars_chr <- x@timepoint_var_nm_1L_chr
      if(!"bins" %in% names(args_ls)){
        args_ls$bins <- 10
      }
      if(!"position" %in% names(args_ls) & is.null(position_xx)){
        args_ls$position <- "dodge"
      }
    }
  }
  if(lifecycle::is_present(y_scale_scl_fn)) {
    lifecycle::deprecate_warn("0.0.0.9127",
                              "youthvars::depict(y_scale_scl_fn)",
                              details = "Please use `youthvars::depict(as_percent_1L_lgl)` instead.")
    if(identical(y_scale_scl_fn, scales::percent)){
      as_percent_1L_lgl <- TRUE
    }
  }
  if(lifecycle::is_present(y_label_1L_chr)) {
    lifecycle::deprecate_warn("0.0.0.9127",
                              "youthvars::depict(y_label_1L_chr)",
                              details = "Please use `youthvars::depict(y_labels_chr)` instead.")
    if(identical(y_labels_chr, character(0))){
      y_labels_chr <- rep(y_label_1L_chr, length(x_vars_chr))
    }
  }
  if(lifecycle::is_present(label_fill_1L_chr)) {
    lifecycle::deprecate_warn("0.0.0.9127",
                              "youthvars::depict(label_fill_1L_chr)",
                              details = "Please use `youthvars::depict(z_labels_chr)` instead.")
    if(identical(z_labels_chr, character(0))){
      z_labels_chr <- rep(label_fill_1L_chr, length(x_vars_chr))
    }
  }
  args_ls <- append(list(slot_nm_1L_chr = "a_Ready4useDyad",
                         x_vars_chr = x_vars_chr,
                         y_vars_chr = y_vars_chr,
                         z_vars_chr = z_vars_chr,
                         arrange_1L_lgl = arrange_1L_lgl,
                         arrange_args_ls = arrange_args_ls,
                         as_percent_1L_lgl = as_percent_1L_lgl,
                         colours_chr = colours_chr,
                         drop_legend_1L_lgl = drop_legend_1L_lgl,
                         drop_missing_1L_lgl = drop_missing_1L_lgl,
                         drop_ticks_1L_lgl = drop_ticks_1L_lgl,
                         fill_single_1L_lgl = fill_single_1L_lgl,
                         line_1L_chr = line_1L_chr,
                         position_xx = position_xx,
                         recode_lup_r3 = recode_lup_r3,
                         style_1L_chr = style_1L_chr,
                         titles_chr = titles_chr,
                         type_1L_chr = type_1L_chr,
                         x_labels_chr = x_labels_chr ,
                         y_labels_chr = y_labels_chr,
                         z_labels_chr = z_labels_chr,
                         what_1L_chr =  what_1L_chr),
                    args_ls)
  plot_xx <- rlang::exec(depictSlot, x, !!!args_ls)
  return(plot_xx)
}
