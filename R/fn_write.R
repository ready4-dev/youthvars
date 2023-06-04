#' Write all output directories
#' @description write_all_outp_dirs() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write all output directories. The function returns Paths (a list).
#' @param paths_ls Paths (a list)
#' @return Paths (a list)
#' @rdname write_all_outp_dirs
#' @export 
#' @importFrom lifecycle deprecate_soft
#' @importFrom here here
#' @importFrom purrr walk
#' @keywords internal
write_all_outp_dirs <- function (paths_ls) 
{
    lifecycle::deprecate_soft("0.0.0.9087", "ready4show::write_all_outp_dirs()", 
        "ready4show::write_all_outp_dirs()")
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
        mkdn_data_dir_1L_chr = mkdn_data_dir_1L_chr, reports_dir_1L_chr = reports_dir_1L_chr, 
        descv_outp_dir_1L_chr = descv_outp_dir_1L_chr, dv_dir_1L_chr = dv_dir_1L_chr))
    return(paths_ls)
}
#' Write descriptive plots
#' @description write_descv_plots() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write descriptive plots. The function returns Descriptive plots paths (a list).
#' @param data_tb Data (a tibble)
#' @param ds_descvs_ls Dataset descriptives (a list)
#' @param descv_outp_dir_1L_chr Descriptive output directory (a character vector of length one)
#' @param combined_plot_params_ls Combined plot parameters (a list), Default: list(nrow_1L_int = 2L, rel_heights_dbl = c(4, 10), scale_dbl = c(0.9, 
#'    0.9), base_height_dbl = 10)
#' @param consent_1L_chr Consent (a character vector of length one), Default: ''
#' @param consent_indcs_int Consent indices (an integer vector), Default: 1
#' @param dim_plots_params_ls Dimension plots parameters (a list), Default: list(plot_rows_cols_pair_int = c(3L, 2L), heights_int = c(10L, 
#'    1L), width_1L_dbl = 8)
#' @param item_plots_params_ls Item plots parameters (a list), Default: list(plot_rows_cols_pair_int = c(5L, 4L), heights_int = c(10L, 
#'    1L), width_1L_dbl = 9)
#' @param lbl_nms_chr Label names (a character vector), Default: character(0)
#' @param maui_domains_pfxs_1L_chr Multi-attribute utility instrument domains prefixes (a character vector of length one), Default: 'vD'
#' @param options_chr Options (a character vector), Default: c("Y", "N")
#' @param utl_by_rnd_plots_params_ls Utility by round plots parameters (a list), Default: list(width_1L_dbl = 6, height_1L_dbl = 4)
#' @param x_labels_chr X labels (a character vector), Default: character(0)
#' @return Descriptive plots paths (a list)
#' @rdname write_descv_plots
#' @export 
#' @importFrom dplyr select starts_with
#' @importFrom ready4 get_from_lup_obj write_with_consent
#' @importFrom purrr map
#' @importFrom rlang exec
#' @importFrom ready4show write_mdl_plt_fl
#' @importFrom stats setNames
#' @importFrom cowplot plot_grid save_plot
#' @importFrom ggplot2 theme
#' @keywords internal
write_descv_plots <- function (data_tb, ds_descvs_ls, descv_outp_dir_1L_chr, combined_plot_params_ls = list(nrow_1L_int = 2L, 
    rel_heights_dbl = c(4, 10), scale_dbl = c(0.9, 0.9), base_height_dbl = 10), 
    consent_1L_chr = "", consent_indcs_int = 1L, dim_plots_params_ls = list(plot_rows_cols_pair_int = c(3L, 
        2L), heights_int = c(10L, 1L), width_1L_dbl = 8), item_plots_params_ls = list(plot_rows_cols_pair_int = c(5L, 
        4L), heights_int = c(10L, 1L), width_1L_dbl = 9), lbl_nms_chr = character(0), 
    maui_domains_pfxs_1L_chr = "vD", options_chr = c("Y", "N"), 
    utl_by_rnd_plots_params_ls = list(width_1L_dbl = 6, height_1L_dbl = 4), 
    x_labels_chr = character(0)) 
{
    if (identical(lbl_nms_chr, character(0))) 
        lbl_nms_chr <- c("Household tasks", "Getting around", 
            "Morbility", "Self care", "Enjoy close rels", "Family rels", 
            "Community involvement", "Despair", "Worry", "Sad", 
            "Agitated", "Energy level", "Control", "Coping", 
            "Frequency of pain", "Degree of pain", "Pain interference", 
            "Vision", "Hearing", "Communication")
    if (is.null(maui_domains_pfxs_1L_chr)) {
        maui_domains_col_nms_chr <- NULL
    }
    else {
        maui_domains_col_nms_chr <- names(dplyr::select(data_tb, 
            dplyr::starts_with(maui_domains_pfxs_1L_chr)))
    }
    plots_params_ls <- list(qstn_rspns = list(plt_fn = make_itm_resp_plts, 
        fn_args_ls = list(data_tb, col_nms_chr = names(dplyr::select(data_tb, 
            starts_with(ds_descvs_ls$maui_item_pfx_1L_chr))), 
            lbl_nms_chr = lbl_nms_chr, plot_rows_cols_pair_int = item_plots_params_ls$plot_rows_cols_pair_int, 
            heights_int = item_plots_params_ls$heights_int, round_var_nm_1L_chr = ds_descvs_ls$round_var_nm_1L_chr), 
        width_1L_dbl = item_plots_params_ls$width_1L_dbl, height_1L_dbl = sum(item_plots_params_ls$heights_int), 
        path_to_write_to_1L_chr = descv_outp_dir_1L_chr, plt_nm_1L_chr = "qstn_rspns"), 
        wtd_sub_tots = list(plt_fn = make_sub_tot_plts, fn_args_ls = list(data_tb, 
            col_nms_chr = maui_domains_col_nms_chr, plot_rows_cols_pair_int = dim_plots_params_ls$plot_rows_cols_pair_int, 
            round_var_nm_1L_chr = ds_descvs_ls$round_var_nm_1L_chr, 
            heights_int = dim_plots_params_ls$heights_int, x_labels_chr = x_labels_chr), 
            width_1L_dbl = dim_plots_params_ls$width_1L_dbl, 
            height_1L_dbl = sum(dim_plots_params_ls$heights_int), 
            path_to_write_to_1L_chr = descv_outp_dir_1L_chr, 
            plt_nm_1L_chr = "wtd_sub_tots"), ll_sub_tot = list(plt_fn = make_sub_tot_plts, 
            fn_args_ls = list(data_tb, col_nms_chr = maui_domains_col_nms_chr, 
                plot_rows_cols_pair_int = dim_plots_params_ls$plot_rows_cols_pair_int, 
                round_var_nm_1L_chr = ds_descvs_ls$round_var_nm_1L_chr, 
                heights_int = dim_plots_params_ls$heights_int, 
                make_log_log_tfmn_1L_lgl = T, x_labels_chr = x_labels_chr), 
            width_1L_dbl = dim_plots_params_ls$width_1L_dbl, 
            height_1L_dbl = sum(dim_plots_params_ls$heights_int), 
            path_to_write_to_1L_chr = descv_outp_dir_1L_chr, 
            plt_nm_1L_chr = "ll_sub_tot"), utl_by_rnd = list(plt_fn = make_var_by_round_plt, 
            fn_args_ls = list(data_tb, var_nm_1L_chr = ds_descvs_ls$utl_wtd_var_nm_1L_chr, 
                round_var_nm_1L_chr = ds_descvs_ls$round_var_nm_1L_chr, 
                x_label_1L_chr = ds_descvs_ls$dictionary_tb %>% 
                  ready4::get_from_lup_obj(match_value_xx = ds_descvs_ls$utl_wtd_var_nm_1L_chr, 
                    match_var_nm_1L_chr = "var_nm_chr", target_var_nm_1L_chr = "var_desc_chr", 
                    evaluate_1L_lgl = F) %>% as.vector()), width_1L_dbl = utl_by_rnd_plots_params_ls$width_1L_dbl, 
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
    ready4::write_with_consent(consented_fn = cowplot::save_plot, 
        prompt_1L_chr = paste0("Are you sure that you want to write the following plot", 
            " to your machine? \n", descv_plts_paths_ls$combined_utl), 
        consent_1L_chr = consent_1L_chr, consent_indcs_int = consent_indcs_int, 
        consented_args_ls = list(filename = descv_plts_paths_ls$combined_utl, 
            plot = combined_plt, base_height = combined_plot_params_ls$base_height_dbl), 
        consented_msg_1L_chr = paste0("New plot created:\n", 
            descv_plts_paths_ls$combined_utl), declined_msg_1L_chr = "Write request cancelled - no plot has been saved.", 
        options_chr = options_chr, return_1L_lgl = F)
    return(descv_plts_paths_ls)
}
#' Write descriptive tables
#' @description write_descv_tbls() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write descriptive tables. The function returns Descriptive table (a list).
#' @param data_tb Data (a tibble)
#' @param ds_descvs_ls Dataset descriptives (a list)
#' @param predictors_lup Predictors (a lookup table)
#' @param descv_outp_dir_1L_chr Descriptive output directory (a character vector of length one)
#' @param consent_1L_chr Consent (a character vector of length one), Default: ''
#' @param consent_indcs_int Consent indices (an integer vector), Default: 1
#' @param nbr_of_digits_1L_int Number of digits (an integer vector of length one), Default: 2
#' @param options_chr Options (a character vector), Default: c("Y", "N")
#' @param participation_var_1L_chr Participation variable (a character vector of length one), Default: 'participation'
#' @return Descriptive table (a list)
#' @rdname write_descv_tbls
#' @export 
#' @importFrom dplyr filter pull mutate
#' @importFrom rlang sym
#' @importFrom purrr list_modify
#' @importFrom ready4 write_with_consent
write_descv_tbls <- function (data_tb, ds_descvs_ls, predictors_lup, descv_outp_dir_1L_chr, 
    consent_1L_chr = "", consent_indcs_int = 1L, nbr_of_digits_1L_int = 2, 
    options_chr = c("Y", "N"), participation_var_1L_chr = "participation") 
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
        test_1L_lgl = if (ds_descvs_ls$round_vals_chr == "Overall" & 
            identical(ds_descvs_ls$round_var_nm_1L_chr, character(0))) {
            F
        } else {
            T
        }, nbr_of_digits_1L_int = nbr_of_digits_1L_int), outc_by_partcn_tbl_tb = if (ds_descvs_ls$round_vals_chr == 
        "Overall" & identical(ds_descvs_ls$round_var_nm_1L_chr, 
        character(0))) {
        NULL
    } else {
        make_descv_stats_tbl(data_tb = data_tb %>% dplyr::filter(!!rlang::sym(ds_descvs_ls$round_var_nm_1L_chr) == 
            ds_descvs_ls$round_vals_chr[1]), key_var_nm_1L_chr = participation_var_1L_chr, 
            key_var_vals_chr = data_tb %>% dplyr::pull(participation_var_1L_chr) %>% 
                unique(), dictionary_tb = ds_descvs_ls$dictionary_tb, 
            variable_nms_chr = c(ds_descvs_ls$candidate_predrs_chr, 
                ds_descvs_ls$utl_wtd_var_nm_1L_chr, ds_descvs_ls$utl_unwtd_var_nm_1L_chr), 
            test_1L_lgl = T, nbr_of_digits_1L_int = nbr_of_digits_1L_int)
    }, bl_cors_tb = transform_ds_for_tstng(data_tb, depnt_var_nm_1L_chr = ds_descvs_ls$utl_wtd_var_nm_1L_chr, 
        depnt_var_max_val_1L_dbl = Inf, candidate_predrs_chr = ds_descvs_ls$candidate_predrs_chr, 
        round_var_nm_1L_chr = if (ds_descvs_ls$round_vals_chr == 
            "Overall" & identical(ds_descvs_ls$round_var_nm_1L_chr, 
            character(0))) {
            NA_character_
        } else {
            ds_descvs_ls$round_var_nm_1L_chr
        }, round_val_1L_chr = if (ds_descvs_ls$round_vals_chr == 
            "Overall" & identical(ds_descvs_ls$round_var_nm_1L_chr, 
            character(0))) {
            NA_character_
        } else {
            ds_descvs_ls$round_vals_chr[1]
        }) %>% make_corstars_tbl_xx(result_chr = "none"), fup_cors_tb = if (ds_descvs_ls$round_vals_chr == 
        "Overall" & identical(ds_descvs_ls$round_var_nm_1L_chr, 
        character(0))) {
        NULL
    } else {
        transform_ds_for_tstng(data_tb, depnt_var_nm_1L_chr = ds_descvs_ls$utl_wtd_var_nm_1L_chr, 
            depnt_var_max_val_1L_dbl = Inf, candidate_predrs_chr = ds_descvs_ls$candidate_predrs_chr, 
            round_var_nm_1L_chr = ds_descvs_ls$round_var_nm_1L_chr, 
            round_val_1L_chr = ds_descvs_ls$round_vals_chr[2]) %>% 
            make_corstars_tbl_xx(result_chr = "none")
    }, cors_with_utl_tb = make_cors_with_utl_tbl(data_tb %>% 
        dplyr::mutate(catch_all_round_chr = ds_descvs_ls$round_vals_chr[1]), 
        ds_descvs_ls = if (ds_descvs_ls$round_vals_chr == "Overall" & 
            identical(ds_descvs_ls$round_var_nm_1L_chr, character(0))) {
            ds_descvs_ls %>% purrr::list_modify(round_var_nm_1L_chr = "catch_all_round_chr")
        } else {
            ds_descvs_ls
        }), ds_descvs_ls = ds_descvs_ls)
    descv_tbl_ls$predr_pars_and_cors_tb <- make_predr_pars_and_cors_tbl(data_tb %>% 
        dplyr::mutate(catch_all_round_chr = ds_descvs_ls$round_vals_chr[1]), 
        ds_descvs_ls = if (ds_descvs_ls$round_vals_chr == "Overall" & 
            identical(ds_descvs_ls$round_var_nm_1L_chr, character(0))) {
            ds_descvs_ls %>% purrr::list_modify(round_var_nm_1L_chr = "catch_all_round_chr")
        }
        else {
            ds_descvs_ls
        }, descv_tbl_ls = descv_tbl_ls, dictionary_tb = ds_descvs_ls$dictionary_tb, 
        nbr_of_digits_1L_int = nbr_of_digits_1L_int, predictors_lup = predictors_lup)
    ready4::write_with_consent(consented_fn = saveRDS, prompt_1L_chr = paste0("Are you sure that you want to write the following file", 
        " to your machine? \n", paste0(descv_outp_dir_1L_chr, 
            "/descv_tbls_ls.RDS")), consent_1L_chr = consent_1L_chr, 
        consent_indcs_int = consent_indcs_int, consented_args_ls = list(object = descv_tbl_ls, 
            file = paste0(descv_outp_dir_1L_chr, "/descv_tbls_ls.RDS")), 
        consented_msg_1L_chr = paste0("New file created:\n", 
            paste0(descv_outp_dir_1L_chr, "/descv_tbls_ls.RDS")), 
        declined_msg_1L_chr = "Write request cancelled - no new file has been created.", 
        options_chr = options_chr, return_1L_lgl = F)
    return(descv_tbl_ls)
}
#' Write results to comma separated variables file
#' @description write_results_to_csv() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write results to comma separated variables file. The function returns Datasets (a tibble).
#' @param synth_data_spine_ls Synthetic data spine (a list)
#' @param output_dir_1L_chr Output directory (a character vector of length one), Default: '.'
#' @return Datasets (a tibble)
#' @rdname write_results_to_csv
#' @export 
#' @importFrom tibble tibble as_tibble
#' @importFrom purrr map_dfr map_dfc map map_dbl walk2
#' @importFrom stats setNames
#' @importFrom dplyr mutate select everything
#' @keywords internal
write_results_to_csv <- function (synth_data_spine_ls, output_dir_1L_chr = ".") 
{
    measurements_tb <- tibble::tibble(timepoint_nms_chr = synth_data_spine_ls$timepoint_nms_chr, 
        nbr_obs_dbl = synth_data_spine_ls$nbr_obs_dbl)
    var_smry_res_tb <- suppressMessages(purrr::map_dfr(1:length(synth_data_spine_ls$timepoint_nms_chr), 
        ~{
            idx_dbl <- .x
            suppressWarnings({
                synth_data_spine_ls[c(4:6)] %>% purrr::map_dfc(~.x[idx_dbl])
            }) %>% stats::setNames(c("Mean", "SD", "N_Missing")) %>% 
                dplyr::mutate(var_names_chr = synth_data_spine_ls$var_names_chr, 
                  timepoint_nms_chr = synth_data_spine_ls$timepoint_nms_chr[idx_dbl]) %>% 
                dplyr::select(timepoint_nms_chr, var_names_chr, 
                  dplyr::everything())
        }))
    cor_tb_ls <- synth_data_spine_ls$cor_mat_ls %>% purrr::map(~tibble::as_tibble(.x) %>% 
        stats::setNames(synth_data_spine_ls$var_names_chr) %>% 
        dplyr::mutate(var_names_chr = synth_data_spine_ls$var_names_chr) %>% 
        dplyr::select(var_names_chr, dplyr::everything())) %>% 
        stats::setNames(paste0(synth_data_spine_ls$timepoint_nms_chr, 
            "_correlations_tb"))
    var_class_pars_tb <- synth_data_spine_ls[7:9] %>% tibble::as_tibble() %>% 
        dplyr::mutate(min_dbl = purrr::map_dbl(min_max_ls, ~.x[1]), 
            max_dbl = purrr::map_dbl(min_max_ls, ~.x[2])) %>% 
        dplyr::select(var_names_chr, dplyr::everything(), -min_max_ls)
    output_ls <- list(measurements_tb = measurements_tb, var_smry_res_tb = var_smry_res_tb, 
        var_class_pars_tb = var_class_pars_tb) %>% append(cor_tb_ls)
    dss_tb <- tibble::tibble(ds_obj_nm_chr = names(output_ls), 
        title_chr = c("Brief summary table of the number of observations for which data was collected at each study timepoint.", 
            "Summary statistics (Mean, SD and Number Missing) for AQoL6D health utility and six mental health outcome measures for each study timepoint.", 
            "Brief information about the data structure (whether discrete and allowable range) of AQoL6D health utility and six mental health outcome variables.", 
            paste0("Correlation matrix for AQoL6D health utility and six mental health outcome measures at the ", 
                synth_data_spine_ls$timepoint_nms_chr, " study timepoint.")))
    purrr::walk2(output_ls, names(output_ls), ~write.csv(.x, 
        file = paste0(output_dir_1L_chr, "/", .y, ".csv"), row.names = F))
    return(dss_tb)
}
