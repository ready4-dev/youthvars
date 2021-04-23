#' Write all output directories
#' @description write_all_outp_dirs() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write all output directories. The function returns Paths (a list).
#' @param paths_ls Paths (a list)
#' @return Paths (a list)
#' @rdname write_all_outp_dirs
#' @export 
#' @importFrom here here
#' @importFrom purrr walk
#' @keywords internal
write_all_outp_dirs <- function (paths_ls) 
{
    output_data_dir_1L_chr <- paste0(here::here(paths_ls$path_from_top_level_1L_chr), 
        "/", paths_ls$write_to_dir_nm_1L_chr, "/Output")
    reports_dir_1L_chr <- paste0(here::here(paths_ls$path_from_top_level_1L_chr), 
        "/", paths_ls$write_to_dir_nm_1L_chr, "/Reports")
    mkdn_data_dir_1L_chr <- paste0(here::here(paths_ls$path_from_top_level_1L_chr), 
        "/", paths_ls$write_to_dir_nm_1L_chr, "/Markdown")
    descv_outp_dir_1L_chr <- paste0(output_data_dir_1L_chr, "/_Descriptives")
    dv_dir_1L_chr <- paste0(output_data_dir_1L_chr, "/H_Dataverse")
    purrr::walk(c(paste0(here::here(paths_ls$path_from_top_level_1L_chr), 
        "/", paths_ls$write_to_dir_nm_1L_chr), mkdn_data_dir_1L_chr, 
        output_data_dir_1L_chr, reports_dir_1L_chr, descv_outp_dir_1L_chr, 
        dv_dir_1L_chr), ~if (!dir.exists(.x)) 
        dir.create(.x))
    paths_ls <- append(paths_ls, list(output_data_dir_1L_chr = output_data_dir_1L_chr, 
        mkdn_data_dir_1L_chr = mkdn_data_dir_1L_chr, descv_outp_dir_1L_chr = descv_outp_dir_1L_chr, 
        dv_dir_1L_chr = dv_dir_1L_chr))
    return(paths_ls)
}
#' Write descriptive plots
#' @description write_descv_plots() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write descriptive plots. The function returns Descriptive plots paths (a list).
#' @param data_tb Data (a tibble)
#' @param ds_descvs_ls Dataset descriptives (a list)
#' @param descv_outp_dir_1L_chr Descriptive output directory (a character vector of length one)
#' @param lbl_nms_chr Label names (a character vector), Default: c("Household tasks", "Getting around", "Morbility", "Self care", 
#'    "Enjoy close rels", "Family rels", "Community involvement", 
#'    "Despair", "Worry", "Sad", "Agitated", "Energy level", "Control", 
#'    "Coping", "Frequency of pain", "Degree of pain", "Pain interference", 
#'    "Vision", "Hearing", "Communication")
#' @param item_plots_params_ls Item plots params (a list), Default: list(plot_rows_cols_pair_int = c(5L, 4L), heights_int = c(10L, 
#'    1L), width_1L_dbl = 9)
#' @param dim_plots_params_ls Dimension plots params (a list), Default: list(plot_rows_cols_pair_int = c(3L, 2L), heights_int = c(10L, 
#'    1L), width_1L_dbl = 8)
#' @param utl_by_rnd_plots_params_ls Utility by rnd plots params (a list), Default: list(width_1L_dbl = 6, height_1L_dbl = 4)
#' @param combined_plot_params_ls Combined plot params (a list), Default: list(nrow_1L_int = 2L, rel_heights_dbl = c(4, 10), scale_dbl = c(0.9, 
#'    0.9), base_height_dbl = 10)
#' @return Descriptive plots paths (a list)
#' @rdname write_descv_plots
#' @export 
#' @importFrom dplyr select starts_with
#' @importFrom ready4fun get_from_lup_obj
#' @importFrom purrr map
#' @importFrom rlang exec
#' @importFrom ready4show write_mdl_plt_fl
#' @importFrom stats setNames
#' @importFrom cowplot plot_grid save_plot
#' @importFrom ggplot2 theme
#' @keywords internal
write_descv_plots <- function (data_tb, ds_descvs_ls, descv_outp_dir_1L_chr, lbl_nms_chr = c("Household tasks", 
    "Getting around", "Morbility", "Self care", "Enjoy close rels", 
    "Family rels", "Community involvement", "Despair", "Worry", 
    "Sad", "Agitated", "Energy level", "Control", "Coping", "Frequency of pain", 
    "Degree of pain", "Pain interference", "Vision", "Hearing", 
    "Communication"), item_plots_params_ls = list(plot_rows_cols_pair_int = c(5L, 
    4L), heights_int = c(10L, 1L), width_1L_dbl = 9), dim_plots_params_ls = list(plot_rows_cols_pair_int = c(3L, 
    2L), heights_int = c(10L, 1L), width_1L_dbl = 8), utl_by_rnd_plots_params_ls = list(width_1L_dbl = 6, 
    height_1L_dbl = 4), combined_plot_params_ls = list(nrow_1L_int = 2L, 
    rel_heights_dbl = c(4, 10), scale_dbl = c(0.9, 0.9), base_height_dbl = 10)) 
{
    plots_params_ls <- list(qstn_rspns = list(plt_fn = make_itm_resp_plts, 
        fn_args_ls = list(data_tb, col_nms_chr = names(dplyr::select(data_tb, 
            starts_with(ds_descvs_ls$maui_item_pfx_1L_chr))), 
            lbl_nms_chr = lbl_nms_chr, plot_rows_cols_pair_int = item_plots_params_ls$plot_rows_cols_pair_int, 
            heights_int = item_plots_params_ls$heights_int), 
        width_1L_dbl = item_plots_params_ls$width_1L_dbl, height_1L_dbl = sum(item_plots_params_ls$heights_int), 
        path_to_write_to_1L_chr = descv_outp_dir_1L_chr, plt_nm_1L_chr = "qstn_rspns"), 
        wtd_sub_tots = list(plt_fn = make_sub_tot_plts, fn_args_ls = list(data_tb, 
            col_nms_chr = names(dplyr::select(data_tb, dplyr::starts_with("vD"))), 
            plot_rows_cols_pair_int = dim_plots_params_ls$plot_rows_cols_pair_int, 
            heights_int = dim_plots_params_ls$heights_int), width_1L_dbl = dim_plots_params_ls$width_1L_dbl, 
            height_1L_dbl = sum(dim_plots_params_ls$heights_int), 
            path_to_write_to_1L_chr = descv_outp_dir_1L_chr, 
            plt_nm_1L_chr = "wtd_sub_tots"), ll_sub_ttl = list(plt_fn = make_sub_tot_plts, 
            fn_args_ls = list(data_tb, col_nms_chr = names(dplyr::select(data_tb, 
                dplyr::starts_with("vD"))), plot_rows_cols_pair_int = dim_plots_params_ls$plot_rows_cols_pair_int, 
                heights_int = dim_plots_params_ls$heights_int, 
                make_log_log_tfmn_1L_lgl = T), width_1L_dbl = dim_plots_params_ls$width_1L_dbl, 
            height_1L_dbl = sum(dim_plots_params_ls$heights_int), 
            path_to_write_to_1L_chr = descv_outp_dir_1L_chr, 
            plt_nm_1L_chr = "ll_sub_ttl"), utl_by_rnd = list(plt_fn = make_var_by_round_plt, 
            fn_args_ls = list(data_tb, var_nm_1L_chr = ds_descvs_ls$utl_wtd_var_nm_1L_chr, 
                round_var_nm_1L_chr = ds_descvs_ls$round_var_nm_1L_chr, 
                x_label_1L_chr = ds_descvs_ls$dictionary_tb %>% 
                  ready4fun::get_from_lup_obj(match_value_xx = ds_descvs_ls$utl_wtd_var_nm_1L_chr, 
                    match_var_nm_1L_chr = "var_nm_chr", target_var_nm_1L_chr = "var_desc_chr", 
                    evaluate_lgl = F) %>% as.vector()), width_1L_dbl = utl_by_rnd_plots_params_ls$width_1L_dbl, 
            height_1L_dbl = utl_by_rnd_plots_params_ls$height_1L_dbl, 
            path_to_write_to_1L_chr = descv_outp_dir_1L_chr, 
            plt_nm_1L_chr = "utl_by_rnd"))
    descv_plts_paths_ls <- purrr::map(plots_params_ls, ~rlang::exec(ready4show::write_mdl_plt_fl, 
        !!!.x)) %>% stats::setNames(names(plots_params_ls))
    combined_plt <- cowplot::plot_grid(rlang::exec(plots_params_ls$utl_by_rnd$plt_fn, 
        !!!plots_params_ls$utl_by_rnd$fn_args_ls) + ggplot2::theme(legend.position = "none"), 
        rlang::exec(plots_params_ls$wtd_sub_tots$plt_fn, !!!plots_params_ls$wtd_sub_tots$fn_args_ls), 
        nrow = combined_plot_params_ls$nrow_1L_int, rel_heights = combined_plot_params_ls$rel_heights_dbl, 
        scale = combined_plot_params_ls$scale_dbl)
    descv_plts_paths_ls$combined_utl <- paste0(descv_outp_dir_1L_chr, 
        "/combined_utl.png")
    cowplot::save_plot(descv_plts_paths_ls$combined_utl, combined_plt, 
        base_height = combined_plot_params_ls$base_height_dbl)
    return(descv_plts_paths_ls)
}
#' Write descriptive tables
#' @description write_descv_tbls() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write descriptive tables. The function returns Descriptive table (a list).
#' @param data_tb Data (a tibble)
#' @param ds_descvs_ls Dataset descriptives (a list)
#' @param predictors_lup Predictors (a lookup table)
#' @param descv_outp_dir_1L_chr Descriptive output directory (a character vector of length one)
#' @param nbr_of_digits_1L_int Number of digits (an integer vector of length one), Default: 2
#' @return Descriptive table (a list)
#' @rdname write_descv_tbls
#' @export 

#' @keywords internal
write_descv_tbls <- function (data_tb, ds_descvs_ls, predictors_lup, descv_outp_dir_1L_chr, 
    nbr_of_digits_1L_int = 2) 
{
    descv_tbl_ls <- list(cohort_desc_tb = make_descv_stats_tbl(data_tb = data_tb, 
        key_var_nm_1L_chr = ds_descvs_ls$round_var_nm_1L_chr, 
        key_var_vals_chr = ds_descvs_ls$round_vals_chr, dictionary_tb = ds_descvs_ls$dictionary_tb, 
        variable_nms_chr = ds_descvs_ls$cohort_descv_var_nms_chr, 
        nbr_of_digits_1L_int = nbr_of_digits_1L_int), main_outc_tbl_tb = make_descv_stats_tbl(data_tb = data_tb, 
        key_var_nm_1L_chr = ds_descvs_ls$round_var_nm_1L_chr, 
        key_var_vals_chr = ds_descvs_ls$round_vals_chr, dictionary_tb = ds_descvs_ls$dictionary_tb, 
        variable_nms_chr = c(ds_descvs_ls$candidate_predrs_chr, 
            ds_descvs_ls$utl_wtd_var_nm_1L_chr, ds_descvs_ls$utl_unwtd_var_nm_1L_chr), 
        test_1L_lgl = T, nbr_of_digits_1L_int = nbr_of_digits_1L_int), 
        outc_by_partcn_tbl_tb = make_descv_stats_tbl(data_tb = data_tb[data_tb$round == 
            "Baseline", ], key_var_nm_1L_chr = "participation", 
            key_var_vals_chr = data_tb$participation %>% unique(), 
            dictionary_tb = ds_descvs_ls$dictionary_tb, variable_nms_chr = c(ds_descvs_ls$candidate_predrs_chr, 
                ds_descvs_ls$utl_wtd_var_nm_1L_chr, ds_descvs_ls$utl_unwtd_var_nm_1L_chr), 
            test_1L_lgl = T, nbr_of_digits_1L_int = nbr_of_digits_1L_int), 
        bl_cors_tb = transform_ds_for_tstng(data_tb, dep_var_max_val_1L_dbl = Inf, 
            candidate_predrs_chr = ds_descvs_ls$candidate_predrs_chr) %>% 
            make_corstars_tbl_xx(result_chr = "none"), fup_cors_tb = transform_ds_for_tstng(data_tb, 
            dep_var_max_val_1L_dbl = Inf, candidate_predrs_chr = ds_descvs_ls$candidate_predrs_chr, 
            round_val_1L_chr = "Follow-up") %>% make_corstars_tbl_xx(result_chr = "none"), 
        cors_with_utl_tb = make_cors_with_utl_tbl(data_tb, ds_descvs_ls = ds_descvs_ls), 
        ds_descvs_ls = ds_descvs_ls)
    descv_tbl_ls$predr_pars_and_cors_tb <- make_predr_pars_and_cors_tbl(data_tb, 
        ds_descvs_ls = ds_descvs_ls, descv_tbl_ls = descv_tbl_ls, 
        dictionary_tb = ds_descvs_ls$dictionary_tb, nbr_of_digits_1L_int = nbr_of_digits_1L_int, 
        predictors_lup = predictors_lup)
    saveRDS(descv_tbl_ls, paste0(descv_outp_dir_1L_chr, "/descv_tbls_ls.RDS"))
    return(descv_tbl_ls)
}
