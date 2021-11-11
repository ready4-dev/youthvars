library(ready4)
library(youthvars)
exhibit <- function (x, ...)
{
  UseMethod("exhibit", x)
}
methods::setGeneric("exhibit")
renewSlot <- function (x, ...)
{
  UseMethod("renewSlot", x)
}
methods::setGeneric("renewSlot")
Ready4Launch <- setClass("Ready4Launch") # ready4
Ready4Private <- setClass("Ready4Private", contains = "Ready4Launch") # ready4
Ready4Public <- setClass("Ready4Public", contains = "Ready4Launch") # ready4
renewSlot_Ready4Launch <- function(x,
                                   new_val_xx,
                                   slot_nm_1L_chr){
  eval(parse(text = paste0("x@",slot_nm_1L_chr," <- new_val_xx")))
  return(x)
}
methods::setMethod("renewSlot",
                   methods::className("Ready4Launch"#, package = "ready4"
                   ),
                   renewSlot_Ready4Launch)
##
Ready4useDyad <- methods::setClass("Ready4useDyad", # ready4use
                                contains = "Ready4Launch",
                                slots = c(ds_tb = "tbl_df",
                                          dictionary_r3 = "ready4use_dictionary"),
                                prototype =  list(ds_tb = tibble::tibble(),
                                                  dictionary_r3 = ready4use::ready4use_dictionary()))
##
exhibit_Ready4useDyad <- function(x,
                                  caption_1L_chr = NA_character_,
                                  display_1L_chr = "all",
                                  mkdn_tbl_ref_1L_chr = "",
                                  output_type_1L_chr = "HTML",
                                  type_1L_chr = "ds",
                                  use_lbls_as_col_nms_1L_lgl = T,
                                  use_rdocx_1L_lgl = F,
                                  ...){
  if(type_1L_chr == "ds"){
    df <- x@ds_tb
    caption_1L_chr <- ifelse(is.na(caption_1L_chr),
                             "Dataset",
                             caption_1L_chr)
  }
  if(type_1L_chr == "dict"){
    df <- x@dictionary_r3
    caption_1L_chr <- ifelse(is.na(caption_1L_chr),
                             "Data Dictionary",
                             caption_1L_chr)
  }
  if(display_1L_chr == "head")
    df <- df %>%
      head()
  if(display_1L_chr == "tail")
    df <- df %>%
      tail()
  df %>%
    ready4show::print_table(output_type_1L_chr = ifelse(knitr::is_html_output(),"HTML","PDF"),
                            use_rdocx_1L_lgl = use_rdocx_1L_lgl,
                            caption_1L_chr = caption_1L_chr,
                            use_lbls_as_col_nms_1L_lgl = use_lbls_as_col_nms_1L_lgl,
                            mkdn_tbl_ref_1L_chr = mkdn_tbl_ref_1L_chr,
                            ...)
}
methods::setMethod("exhibit",
                   methods::className("Ready4useDyad"#, package = "ready4"
                   ),
                   exhibit_Ready4useDyad)
##
YouthvarsDescriptives <- methods::setClass("YouthvarsDescriptives", #youthvars
                                           contains = "Ready4Launch",
                                           slots = c(descriptives_df = "data.frame",
                                                     key_var_nm_1L_chr = "character",
                                                     ds_tfmn_ls = "list",
                                                     key_var_vals_chr = "character",
                                                     nbr_of_digits_1L_int = "integer",
                                                     profiled_vars_chr = "character",
                                                     sections_as_row_1L_lgl = "logical",
                                                     test_1L_lgl = "logical"),
                                           prototype = list(descriptives_df = data.frame(),
                                                            key_var_nm_1L_chr = "round",
                                                            ds_tfmn_ls = list(),
                                                            key_var_vals_chr = NA_character_,
                                                            nbr_of_digits_1L_int = 3L,
                                                            profiled_vars_chr = NA_character_,
                                                            sections_as_row_1L_lgl = F,
                                                            test_1L_lgl = F))
##
characterize_YouthvarsDescriptives <- function(x,
                                               y_Ready4useDyad){
  if(x@key_var_nm_1L_chr %in% c("participation")){
    data_tb <- rlang::exec(x@ds_tfmn_ls$fn,
                           y_Ready4useDyad@ds_tb,
                           !!!x@ds_tfmn_ls$args_ls)
  }else{
    data_tb <- y_Ready4useDyad@ds_tb
  }
  if(is.na(x@key_var_vals_chr[1])){
    key_var_vals_chr <- NULL
  }else{
    key_var_vals_chr <- x@key_var_vals_chr
  }
  descriptives_df <- make_descv_stats_tbl(data_tb =  data_tb,
                                          key_var_nm_1L_chr = x@key_var_nm_1L_chr,
                                          key_var_vals_chr = key_var_vals_chr,
                                          dictionary_tb = y_Ready4useDyad@dictionary_r3,
                                          sections_as_row_1L_lgl = x@sections_as_row_1L_lgl,
                                          test_1L_lgl = x@test_1L_lgl,
                                          variable_nms_chr = x@profiled_vars_chr,
                                          nbr_of_digits_1L_int = x@nbr_of_digits_1L_int)
  return(descriptives_df)
}
renew_YouthvarsDescriptives <- function(x,
                                        type_1L_chr = "characterize",
                                        ...){
  if(type_1L_chr == "characterize")
    x@descriptives_df <- characterize(x,
                                      ...)
  return(x)
}
methods::setMethod("characterize",
                   methods::className("YouthvarsDescriptives"#, package = "ready4use"
                   ),
                   characterize_YouthvarsDescriptives)
methods::setMethod("renew",
                   methods::className("YouthvarsDescriptives"#, package = "ready4use"
                   ),
                   renew_YouthvarsDescriptives)
##
YouthvarsProfile <- methods::setClass("YouthvarsProfile", #youthvars
                                      contains = "Ready4Launch",
                                      slots = c(a_Ready4useDyad = "Ready4useDyad",
                                                descriptives_ls = "list",
                                                id_var_nm_1L_chr = "character"),
                                      prototype = list(a_Ready4useDyad = Ready4useDyad(),
                                                       descriptives_ls = list(),
                                                       id_var_nm_1L_chr = "fkClientID"))
##
renew_YouthvarsProfile <- function(x,
                                   type_1L_chr = "characterize"){
  if(type_1L_chr == "characterize")
    x@descriptives_ls <- purrr::map(x@descriptives_ls,
                                    ~ready4::renew(.x,
                                                   y_Ready4useDyad = x@a_Ready4useDyad))
  return(x)
}
methods::setMethod("renew",
                   methods::className("YouthvarsProfile"#, package = "ready4use"
                   ),
                   renew_YouthvarsProfile)
##
YouthvarsSeries <- methods::setClass("YouthvarsSeries", #youthvars
                                     contains = "YouthvarsProfile",
                                     slots = c(#a_YouthvarsProfile = "YouthvarsProfile",
                                               timepoint_vals_chr = "character",
                                               timepoint_var_nm_1L_chr = "character"),
                                     prototype =  list(#a_YouthvarsProfile = YouthvarsProfile(),
                                                       timepoint_vals_chr = c("Baseline","Follow-up"),
                                                       timepoint_var_nm_1L_chr = "round"))
##
exhibit_YouthvarsSeries <- function(x,
                                    profile_idx_int = NA_integer_,
                                    output_type_1L_chr = "HTML",
                                    timepoints_int = c(1L,2L),
                                    type_1L_chr = "characterize",
                                    ...){
  if(is.na(profile_idx_int))
    profile_idx_int <- 1:length(x@descriptives_ls) %>% as.integer()
  profile_idx_int %>%
    purrr::map(~{
      profile_idx_1L_int <- .x
      if(identical(x@descriptives_ls[[profile_idx_1L_int]]@descriptives_df, data.frame())){
        message("It was not possible to print a table as the descriptives_df element of the descriptives_ls slot of the supplied YouthvarsDescriptives instance is empty. Use characterise(x) or renew(x) methods to generate a value for this element.")
      }else{
        if(x@descriptives_ls[[profile_idx_1L_int]]@key_var_nm_1L_chr == "participation"){
          header_col_nms_chr <- x@a_Ready4useDyad@ds_tb$participation %>% unique()#
        }else{
          header_col_nms_chr <- NULL
        }
        x@descriptives_ls[[profile_idx_1L_int]]@descriptives_df %>%
          print_descv_stats_tbl(bl_fup_vals_chr = x@timepoint_vals_chr[timepoints_int],
                                data_tb = x@a_Ready4useDyad@ds_tb,# CHECK
                                header_col_nms_chr = header_col_nms_chr,
                                output_type_1L_chr = output_type_1L_chr,
                                round_var_nm_1L_chr = x@timepoint_var_nm_1L_chr,
                                test_1L_lgl =  x@descriptives_ls[[profile_idx_1L_int]]@test_1L_lgl,
                                variable_nms_chr =  x@descriptives_ls[[profile_idx_1L_int]]@profiled_vars_chr,
                                ...)
      }
    })
}
ratify_YouthvarsSeries <- function(x,
                                   type_1L_chr = "two_timepoints"){
  if(type_1L_chr=="two_timepoints")
    assert_ds_is_valid(x@a_Ready4useDyad@ds_tb,
                       id_var_nm_1L_chr = x@id_var_nm_1L_chr,
                       round_var_nm_1L_chr = x@timepoint_var_nm_1L_chr,
                       round_bl_val_1L_chr = x@timepoint_vals_chr[1])
  return(x)
}
methods::setMethod("exhibit",
                   methods::className("YouthvarsSeries"#, package = "ready4use"
                   ),
                   exhibit_YouthvarsSeries)
methods::setMethod("ratify",
                   methods::className("YouthvarsSeries"#, package = "ready4use"
                   ),
                   ratify_YouthvarsSeries)
##
YouthvarsModelSpec <- methods::setClass("YouthvarsModelSpec", #youthvars
                                        contains = "Ready4Launch",
                                        slots = c(a_YouthvarsProfile = "YouthvarsProfile",
                                                  candidate_predrs_chr = "character",
                                                  depnt_var_nm_1L_chr = "character",
                                                  depnt_var_max_val_1L_dbl = "numeric"),
                                        prototype =  list(a_YouthvarsProfile = YouthvarsProfile(),
                                                          candidate_predrs_chr = NA_character_,
                                                          depnt_var_nm_1L_chr = NA_character_,
                                                          depnt_var_max_val_1L_dbl = Inf))
exhibit_YouthvarsModelSpec <- function(x,
                                       output_type_1L_chr = "HTML",
                                       type_1L_chr = "correlation",
                                       timepoints_int = NA_integer_){
  if(type_1L_chr == "correlation"){
    if(is.na(timepoints_int)){
      if("timepoint_vals_chr" %in% slotNames(x@a_YouthvarsProfile)){
        timepoints_int <- 1:length(x@a_YouthvarsProfile@timepoint_vals_chr) %>% as.integer()
      }else{
        timepoints_int <- 1
      }
    }
      timepoints_int %>%
        purrr::map(~
          transform_ds_for_tstng(x@a_YouthvarsProfile@a_Ready4useDyad@ds_tb,
                                 depnt_var_nm_1L_chr = x@depnt_var_nm_1L_chr,
                                 depnt_var_max_val_1L_dbl = x@depnt_var_max_val_1L_dbl,
                                 candidate_predrs_chr = x@candidate_predrs_chr,
                                 round_var_nm_1L_chr = ifelse("timepoint_var_nm_1L_chr" %in% slotNames(x@a_YouthvarsProfile),
                                                              x@a_YouthvarsProfile@timepoint_var_nm_1L_chr,
                                                              NA_character_),
                                 round_val_1L_chr = ifelse("timepoint_vals_chr" %in% slotNames(x@a_YouthvarsProfile),
                                                           x@a_YouthvarsProfile@timepoint_vals_chr[.x],
                                                           NA_character_))  %>%
          make_corstars_tbl_xx(result_chr = output_type_1L_chr)
        )
  }
}
methods::setMethod("exhibit",
                   methods::className("YouthvarsModelSpec"#, package = "ready4use"
                   ),
                   exhibit_YouthvarsModelSpec)
# Plot
plot_YouthvarsSeries <- function(x,
                                 type_1L_chr = "by_time",
                                 var_nms_chr,
                                 labels_chr = NA_character_){
  if(is.na(labels_chr[1]))
    labels_chr <- var_nms_chr %>%
      purrr::map_chr(~ready4::get_from_lup_obj(x@a_Ready4useDyad@dictionary_r3,
                                               match_var_nm_1L_chr = "var_nm_chr",
                                               match_value_xx = .x,
                                               target_var_nm_1L_chr = "var_desc_chr"))
  if(type_1L_chr == "by_time"){
    purrr::map2(var_nms_chr,
                labels_chr,
                ~  make_var_by_round_plt(x@a_Ready4useDyad@ds_tb,
                                         var_nm_1L_chr = .x,
                                         round_var_nm_1L_chr = x@timepoint_var_nm_1L_chr,
                                         x_label_1L_chr = .y))

  }
}
methods::setMethod("plot",
                   methods::className("YouthvarsSeries"#, package = "ready4use"
                   ),
                   plot_YouthvarsSeries)
# Make scoring class, dyad plus:
ScorzItems <- methods::setClass("ScorzItems", #scorz
                                 contains = "Ready4Launch",
                                 slots = c(a_YouthvarsProfile = "YouthvarsProfile",
                                           domain_unwtd_var_nms_chr = "character",
                                           domain_wtd_var_nms_chr = "character",
                                           instrument_dict_r3 = "ready4use_dictionary",
                                           instrument_nm_1L_chr = "character",
                                           itm_labels_chr = "character",
                                           itm_prefix_1L_chr =  "character",
                                           total_wtd_var_nm_1L_chr = "character",
                                           total_unwtd_var_nm_1L_chr = "character"),
                                 prototype = list(a_YouthvarsProfile = YouthvarsProfile(),
                                                  domain_unwtd_var_nms_chr = NA_character_,
                                                  domain_wtd_var_nms_chr = NA_character_,
                                                  instrument_dict_r3 = ready4use::ready4use_dictionary(),
                                                  instrument_nm_1L_chr = NA_character_,
                                                  itm_labels_chr = NA_character_,
                                                  itm_prefix_1L_chr =  NA_character_,
                                                  total_wtd_var_nm_1L_chr = NA_character_,
                                                  total_unwtd_var_nm_1L_chr = NA_character_))
ScorzAqol6 <- methods::setClass("ScorzAqol6",
                                 contains = "ScorzItems",
                                 slots = c(a_YouthvarsProfile = "YouthvarsProfile",
                                           domain_unwtd_var_nms_chr = "character",
                                           domain_wtd_var_nms_chr = "character",
                                           instrument_dict_r3 = "ready4use_dictionary",
                                           instrument_nm_1L_chr = "character",
                                           itm_labels_chr = "character",
                                           itm_prefix_1L_chr =  "character",
                                           total_wtd_var_nm_1L_chr = "character",
                                           total_unwtd_var_nm_1L_chr = "character"),
                                 prototype = list(a_YouthvarsProfile = YouthvarsProfile(),
                                                  domain_unwtd_var_nms_chr = NA_character_,
                                                  domain_wtd_var_nms_chr = paste0("vD",1:6),
                                                  instrument_dict_r3 = youthvars::aqol_scrg_dict_r3,
                                                  instrument_nm_1L_chr = "Assessment of Quality of Life (6 Dimension)",
                                                  itm_labels_chr = c("Household tasks", "Getting around",
                                                                 "Morbility","Self care","Enjoy close rel\'s",
                                                                 "Family rel\'s", "Community involv\'t",
                                                                 "Despair","Worry", "Sad", "Agitated",
                                                                 "Energy level", "Control", "Coping",
                                                                 "Frequency of pain", "Degree of pain",
                                                                 "Pain interference","Vision", "Hearing",
                                                                 "Communication"),
                                                  itm_prefix_1L_chr =  "aqol6d_q",
                                                  total_wtd_var_nm_1L_chr = "aqol6d_total_w",
                                                  total_unwtd_var_nm_1L_chr = NA_character_))
ScorzAqol6Adol <- methods::setClass("ScorzAqol6Adol",
                                    contains = "ScorzAqol6",
                                    prototype = list(instrument_nm_1L_chr = "Assessment of Quality of Life (6 Dimension, Adolescent Version)"))
##
plot_ScorzItems <- function(x,
                            heights_int = NA_integer_,
                            plot_rows_cols_pair_int = NA_integer_,
                            type_1L_chr = "item_by_time",
                            y_label_1L_chr = "",
                            var_idcs_int = NA_integer_,
                            ...){
  if(endsWith(type_1L_chr,"by_time") & "timepoint_var_nm_1L_chr" %in% slotNames(x@a_YouthvarsProfile)){
    if(type_1L_chr == "comp_item_by_time"){
      if(is.na(heights_int[1]))
        heights_int <- c(20L, 1L)
      if(is.na(plot_rows_cols_pair_int[1]))
        plot_rows_cols_pair_int <- c(5L,4L)
      plt <- make_itm_resp_plts(x@a_YouthvarsProfile@a_Ready4useDyad@ds_tb,
                                col_nms_chr = names(dplyr::select(x@a_YouthvarsProfile@a_Ready4useDyad@ds_tb,
                                                                  starts_with(x@itm_prefix_1L_chr))),
                                lbl_nms_chr = x@itm_labels_chr,
                                plot_rows_cols_pair_int = plot_rows_cols_pair_int,
                                heights_int = heights_int,
                                round_var_nm_1L_chr = x@a_YouthvarsProfile@timepoint_var_nm_1L_chr,# CONDITIONAL
                                y_label_1L_chr = y_label_1L_chr,
                                ...)

    }
    if(type_1L_chr == "comp_domain_by_time"){
      if(is.na(heights_int[1]))
        heights_int <- c(10L, 1L)
      if(is.na(plot_rows_cols_pair_int[1]))
        plot_rows_cols_pair_int <- c(3L,2L)
      plt <- make_sub_tot_plts(x@a_YouthvarsProfile@a_Ready4useDyad@ds_tb,
                               col_nms_chr = x@domain_wtd_var_nms_chr,
                               plot_rows_cols_pair_int = plot_rows_cols_pair_int,
                               round_var_nm_1L_chr = x@a_YouthvarsProfile@timepoint_var_nm_1L_chr,
                               heights_int = heights_int,
                               y_label_1L_chr = y_label_1L_chr,
                               ...)
    }
    if(type_1L_chr %in% c("domain_by_time","item_by_time","total_by_time")){
      if(type_1L_chr == "item_by_time"){
        var_nms_chr <- names(dplyr::select(x@a_YouthvarsProfile@a_Ready4useDyad@ds_tb,
                                           starts_with(x@itm_prefix_1L_chr)))
      }
      if(type_1L_chr == "domain_by_time"){
        var_nms_chr <- x@domain_wtd_var_nms_chr
      }
      if(type_1L_chr == "total_by_time"){
        var_nms_chr <- c(x@total_wtd_var_nm_1L_chr,x@total_unwtd_var_nm_1L_chr) %>%
          purrr::discard(is.na)
      }
      if(is.na(var_idcs_int[1]))
        var_idcs_int <- 1:length(var_nms_chr)
      var_nms_chr[var_idcs_int] %>%
        purrr::map(~ plot(x@a_YouthvarsProfile,
                          type_1L_chr = "by_time",
                          var_nms_chr = .x))
    }

  }
}
methods::setMethod("plot",
                   methods::className("ScorzItems"#, package = "ready4use"
                   ),
                   plot_ScorzItems)
##
renew_ScorzAqol6Adol <- function(x,
                                 type_1L_chr = "utility"){
  scored_data_tb <- add_adol6d_scores(x@a_YouthvarsProfile@a_Ready4useDyad@ds_tb,
                                      prefix_1L_chr =  x@itm_prefix_1L_chr,
                                      id_var_nm_1L_chr = x@a_YouthvarsProfile@id_var_nm_1L_chr,
                                      wtd_aqol_var_nm_1L_chr = x@total_wtd_var_nm_1L_chr)
  dictionary_r3 <- ready4::renew(x@a_YouthvarsProfile@a_Ready4useDyad@dictionary_r3,
                                 new_ready4_dict_r3 = x@instrument_dict_r3)
  scored_data_tb <- scored_data_tb %>%
    ready4use::add_labels_from_dictionary(dictionary_r3)
  x@a_YouthvarsProfile@a_Ready4useDyad@ds_tb <- scored_data_tb
  x@a_YouthvarsProfile@a_Ready4useDyad@dictionary_r3 <- dictionary_r3
  return(x)
}
methods::setMethod("renew",
                   methods::className("ScorzAqol6Adol"#, package = "ready4use"
                   ),
                   renew_ScorzAqol6Adol)
##
data("replication_popl_tb")
data("repln_ds_dict_r3", package = "youthvars")
data("aqol_scrg_dict_r3", package = "youthvars")
x_Ready4useDyad <- Ready4useDyad(ds_tb = replication_popl_tb %>%
                             # add_adol6d_scores(prefix_1L_chr = "aqol6d_q",
                             #                   id_var_nm_1L_chr = "fkClientID",
                             #                   wtd_aqol_var_nm_1L_chr = "aqol6d_total_w") %>%
                               add_participation_var(),
                           dictionary_r3 = repln_ds_dict_r3
                           # ready4::renew(repln_ds_dict_r3,
                           #                               new_ready4_dict_r3 = aqol_scrg_dict_r3)
                           )
exhibit(x, display_1L_chr = "head")
a_YouthvarsDescriptives <- YouthvarsDescriptives(key_var_nm_1L_chr = "round",
                                                 key_var_vals_chr = c("Baseline","Follow-up"),
                                                 nbr_of_digits_1L_int = 3L,
                                                 profiled_vars_chr = c("d_age","d_sexual_ori_s",
                                                                       "d_ATSI","d_studying_working","d_relation_s"))
# test_df <- characterize(a_YouthvarsDescriptives,
#              y_Ready4useDyad = x_Ready4useDyad)
b_YouthvarsDescriptives <- renew(a_YouthvarsDescriptives,
                                 y_Ready4useDyad = x_Ready4useDyad)
#
x_YouthvarsProfile <- YouthvarsProfile(a_Ready4useDyad = x_Ready4useDyad,
                                       descriptives_ls = list(a = a_YouthvarsDescriptives,
                                                              b = YouthvarsDescriptives(key_var_nm_1L_chr = "round",
                                                                                        key_var_vals_chr = c("Baseline","Follow-up"),
                                                                                        nbr_of_digits_1L_int = 3L,
                                                                                        profiled_vars_chr = c("k6_total", "phq9_total", "bads_total", "gad7_total"),
                                                                                        test_1L_lgl = T) ,
                                                              c = YouthvarsDescriptives(ds_tfmn_ls = list(args_ls = NULL,
                                                                                                         fn = function(x){dplyr::filter(x,round == "Baseline")}),
                                                                                       key_var_nm_1L_chr = "participation",
                                                                                       nbr_of_digits_1L_int = 3L,
                                                                                       profiled_vars_chr = c("k6_total", "phq9_total", "bads_total", "gad7_total"),
                                                                                       test_1L_lgl = T)
                                                              ))
x_YouthvarsProfile <- renew(x_YouthvarsProfile,
                            type_1L_chr = "characterize")
x_YouthvarsSeries <- YouthvarsSeries(x_YouthvarsProfile,
                                     timepoint_vals_chr = c("Baseline","Follow-up"),
                                     timepoint_var_nm_1L_chr = "round")
x_YouthvarsSeries <- ratify(x_YouthvarsSeries,
                            type_1L_chr = "two_timepoints")
x_YouthvarsSeries %>%
              exhibit(type_1L_chr = "characterize",
                      output_type_1L_chr = "HTML")
plot(x_YouthvarsSeries,
     type_1L_chr = "by_time",
     var_nms_chr = "c_sofas")
#
x_ScorzAqol6Adol <- ScorzAqol6Adol(a_YouthvarsProfile = x_YouthvarsSeries)
y_ScorzAqol6Adol <- renew(x_ScorzAqol6Adol)
plot(x_ScorzAqol6Adol, type_1L_chr = "item_by_time")
plot(y_ScorzAqol6Adol, type_1L_chr = "domain_by_time", var_idcs_int = c(2L))
plot(y_ScorzAqol6Adol, type_1L_chr = "domain_by_time")
plot(y_ScorzAqol6Adol, type_1L_chr = "domain_by_time", var_idcs_int = c(1L))
plot(y_ScorzAqol6Adol, type_1L_chr = "total_by_time")
plot(y_ScorzAqol6Adol, type_1L_chr = "comp_item_by_time")
plot(y_ScorzAqol6Adol, type_1L_chr = "comp_domain_by_time")
#

#
x_YouthvarsModelSpec <- YouthvarsModelSpec(a_YouthvarsProfile = x_YouthvarsProfile,
                                           candidate_predrs_chr = c("k6_total", "phq9_total", "bads_total", "gad7_total"),
                                           depnt_var_nm_1L_chr = "aqol6d_total_w",
                                           depnt_var_max_val_1L_dbl = Inf)
y_YouthvarsModelSpec <- YouthvarsModelSpec(a_YouthvarsProfile = x_YouthvarsSeries,
                                           candidate_predrs_chr = c("k6_total", "phq9_total", "bads_total", "gad7_total"),
                                           depnt_var_nm_1L_chr = "aqol6d_total_w",
                                           depnt_var_max_val_1L_dbl = Inf)
exhibit(y_YouthvarsModelSpec,
        type_1L_chr = "correlation")


# Add reckon method
  # Add exhibit method

# add_dates_from_dstr <- function(ds_tb,
#                                 bl_start_date_dtm,
#                                 bl_end_date_dtm,
#                                 duration_args_ls,
#                                 duration_fn = stats::rnorm,
#                                 date_var_nm_1L_chr = "date_psx",
#                                 id_var_nm_1L_chr = "fkClientID",
#                                 round_var_nm_1L_chr = "round",
#                                 round_bl_val_1L_chr = "Baseline",
#                                 origin_1L_chr = '1970-01-01'){
#   args_ls <- append(list(n=nrow(ds_tb)), duration_args_ls)
#   days_of_fup_int <- rlang::exec(.fn = duration_fn, !!!args_ls) %>% round(0) %>% as.integer()
#   updated_ds_tb <- ds_tb %>%
#     dplyr::mutate(duration_prd = dplyr::case_when(!!rlang::sym(round_var_nm_1L_chr) != round_bl_val_1L_chr ~lubridate::days(days_of_fup_int),
#                                                   T ~ lubridate::days(0))) %>%
#     dplyr::mutate(!!rlang::sym(date_var_nm_1L_chr) := dplyr::case_when(!!rlang::sym(round_var_nm_1L_chr) == round_bl_val_1L_chr ~ as.Date(sample(as.numeric(bl_start_date_dtm):as.numeric(bl_end_date_dtm),
#                                                                                                                                                  dplyr::n(),
#                                                                                                                                                  replace = T),
#                                                                                                                                           origin = origin_1L_chr ))) %>%
#     dplyr::group_by(!!rlang::sym(id_var_nm_1L_chr)) %>%
#     dplyr::mutate(!!rlang::sym(date_var_nm_1L_chr) := dplyr::case_when(!!rlang::sym(round_var_nm_1L_chr) == round_bl_val_1L_chr ~ !!rlang::sym(date_var_nm_1L_chr),
#                                                                        T ~ dplyr::lag(!!rlang::sym(date_var_nm_1L_chr)) + duration_prd)) %>%
#     dplyr::ungroup()  %>%
#     dplyr::select(!!rlang::sym(id_var_nm_1L_chr),
#                   !!rlang::sym(round_var_nm_1L_chr),
#                   !!rlang::sym(date_var_nm_1L_chr),
#                   duration_prd,
#                   dplyr::everything())
#   return(updated_ds_tb)
# }
