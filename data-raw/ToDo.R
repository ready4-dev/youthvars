library(youthvars)
exhibit <- function (x, ...)
{
  UseMethod("exhibit", x)
}
methods::setGeneric("exhibit")
Ready4Launch <- setClass("Ready4Launch")
Ready4Private <- setClass("Ready4Private", contains = "Ready4Launch")
Ready4Public <- setClass("Ready4Public", contains = "Ready4Launch")
# Make Dataset and Dictionary Dyad class
Ready4Dyad <- methods::setClass("Ready4Dyad", # ready4use
                                contains = "Ready4Launch",
                                slots = c(ds_tb = "tbl_df",
                                          dictionary_r3 = "ready4use_dictionary"),
                                prototype =  list(ds_tb = tibble::tibble(),
                                                  dictionary_r3 = ready4use::ready4use_dictionary()))
#
data("replication_popl_tb")
data("repln_ds_dict_r3", package = "youthvars")
data("aqol_scrg_dict_r3", package = "youthvars")
x_Ready4Dyad <- Ready4Dyad(ds_tb = replication_popl_tb %>%
                             add_adol6d_scores(prefix_1L_chr = "aqol6d_q",
                                               id_var_nm_1L_chr = "fkClientID",
                                               wtd_aqol_var_nm_1L_chr = "aqol6d_total_w"),
                           dictionary_r3 = ready4::renew(repln_ds_dict_r3,
                                                         new_ready4_dict_r3 = aqol_scrg_dict_r3))
# Make ds profile class
Ready4Profile <- methods::setClass("Ready4Profile", #youthvars
                                   contains = "Ready4Dyad",
                                   slots = c(nbr_of_digits_1L_int = "integer",
                                             profiled_vars_chr = "character"),
                                prototype = list(nbr_of_digits_1L_int = 3L,
                                                 profiled_vars_chr = NA_character_))
Ready4Descriptive <- methods::setClass("Ready4Descriptive", #youthvars
                                   contains = "Ready4Profile",
                                   slots = c(descriptives_df = "data.frame"),
                                   prototype = list(descriptives_df = data.frame()))
x_Ready4Profile <- Ready4Profile(x_Ready4Dyad,
                                 profiled_vars_chr = c("d_age","d_sexual_ori_s",
                                                      "d_ATSI","d_studying_working","d_relation_s"),
                                 nbr_of_digits_1L_int = 2L)
  # Add characterize and display / print methods
characterize_Ready4Profile <- function(x,
                                       key_var_nm_1L_chr,
                                       key_var_vals_chr = NULL,
                                       sections_as_row_1L_lgl = F,
                                       test_1L_lgl = F){
  if(is.na(x@profiled_vars_chr[1]))
    x@profiled_vars_chr <- names(x@ds_tb)
  descriptives_df <- make_descv_stats_tbl(data_tb = x@ds_tb,
                                          key_var_nm_1L_chr = key_var_nm_1L_chr,
                                          key_var_vals_chr = key_var_vals_chr,
                                          dictionary_tb = x@dictionary_r3,
                                          sections_as_row_1L_lgl = sections_as_row_1L_lgl,
                                          test_1L_lgl = test_1L_lgl,
                                          variable_nms_chr = x@profiled_vars_chr,
                                          nbr_of_digits_1L_int = x@nbr_of_digits_1L_int)
  return(descriptives_df)
}
metamorphose_Ready4Profile <- function(x,
                                       type_1L_chr = "characterize",
                                       ...){
  if(type_1L_chr == "characterize")
    x_Ready4Descriptive <- Ready4Descriptive(x,
                                             descriptives_df = ready4::characterize(x,
                                                                                    ...))
  return(x_Ready4Descriptive)
}
methods::setMethod("characterize",
                   methods::className("Ready4Profile"#, package = "ready4use"
                                      ),
                   characterize_Ready4Profile)
methods::setMethod("metamorphose",
                   methods::className("Ready4Profile"#, package = "ready4use"
                   ),
                   metamorphose_Ready4Profile)
characterize(x_Ready4Profile, key_var_nm_1L_chr = "round")
x_Descriptives <- metamorphose(x_Ready4Profile, # PICKUPHERE
                               type_1L_chr = "characterize",
                               key_var_nm_1L_chr = "round")
# Make lngl ds (2 measures) class, dyad plus:
Ready4Long <- methods::setClass("Ready4Long", #youthvars
                                contains = "Ready4Profile",
                                slots = c(id_var_nm_1L_chr = "character",
                                          round_var_nm_1L_chr = "character"),
                                   prototype =  list(id_var_nm_1L_chr = "fkClientID",
                                                     round_var_nm_1L_chr = "round"))
x_Ready4Long <- Ready4Long(x_Ready4Profile,
                           id_var_nm_1L_chr = "fkClientID",
                           round_var_nm_1L_chr = "round")
Ready4Long2 <- methods::setClass("Ready4Long2", #youthvars
                                contains = "Ready4Long",
                                slots = c(bl_fup_vals_chr = "character"),
                                prototype = list(bl_fup_vals_chr = c("Baseline","Follow-up")))
x_Ready4Long2 <- Ready4Long2(x_Ready4Long,
                            bl_fup_vals_chr = c("Baseline","Follow-up"))
exhibit_Descriptives <- function(x,
                                 type_1L_chr = "characterize",
                                 output_type_1L_chr = "HTML"){
  if(is.na(x@profiled_vars_chr[1]))
    x@profiled_vars_chr <- names(x@ds_tb)
  if(type_1L_chr == "characterize"){
    x %>%
      print_descv_stats_tbl(bl_fup_vals_chr = round_vals_chr,
                            output_type_1L_chr = output_type_1L_chr,
                            round_var_nm_1L_chr = "round",
                            variable_nms_chr = x@profiled_vars_chr)
  }

}
methods::setMethod("exhibit",
                   methods::className("Ready4Long"#, package = "ready4use"
                   ),
                   exhibit_Ready4Long)

metamorphose(x_Ready4Long,
        type_1L_chr = "characterize",
        key_var_nm_1L_chr = "round") %>%
  exhibit(type_1L_chr = "characterize",
          output_type_1L_chr = "HTML",
          round_vals_chr = c("Baseline","Follow-up"))

  # Add characterize and exhibit methods
  # Add ratify method that uses assert fns
# Make scoring class, dyad plus:
Ready4Score <- methods::setClass("Ready4Score", #scorz
                                 contains = "Ready4Dyad",
                                 slots = c(ds_tb = "tbl_df",
                                           dictionary_r3 = "ready4use_dictionary",
                                           prefix_1L_chr =  "character",
                                           id_var_nm_1L_chr = "character",
                                           scored_var_nm_1L_chr = "character"),
                                prototype = list(ds_tb = tibble::tibble(),
                                                  dictionary_r3 = ready4use::ready4use_dictionary(),
                                                  prefix_1L_chr =  NA_character_,
                                                  id_var_nm_1L_chr = "fkClientID",
                                                  scored_var_nm_1L_chr = NA_character_))
Ready4Aqol6 <- methods::setClass("Ready4Aqol6",
                                 contains = "Ready4Score",
                                 slots = c(ds_tb = "tbl_df",
                                           dictionary_r3 = "ready4use_dictionary",
                                           prefix_1L_chr =  "character",
                                           id_var_nm_1L_chr = "character",
                                           scored_var_nm_1L_chr = "character"),
                                 prototype = list(ds_tb = tibble::tibble(),
                                                  dictionary_r3 = ready4use::ready4use_dictionary(),
                                                  prefix_1L_chr =  "aqol6d_q",
                                                  id_var_nm_1L_chr = "fkClientID",
                                                  scored_var_nm_1L_chr = "aqol6d_total_w"))#(wtd_aqol_var_nm_1L_chr = "aqol6d_total_w")
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
