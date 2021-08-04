assert_ds_is_valid <- function(data_tb,
                               id_var_nm_1L_chr,
                               round_var_nm_1L_chr,
                               round_bl_val_1L_chr,
                               msrmnt_date_var_nm_1L_chr = NULL){
  assertthat::assert_that(length(data_tb %>% dplyr::pull(!!rlang::sym(round_var_nm_1L_chr)) %>%
                                   unique()) == 2, msg = paste0(round_var_nm_1L_chr, " variable must have two values - one for each data collection round."))
  assertthat::assert_that(is.factor(data_tb %>% dplyr::pull(!!rlang::sym(round_var_nm_1L_chr))),
                          msg = paste0(round_var_nm_1L_chr, " variable must be a factor."))
  assertthat::assert_that(round_bl_val_1L_chr %in% levels(data_tb %>%
                                                            dplyr::pull(!!rlang::sym(round_var_nm_1L_chr))), msg = paste0("Levels of ",
                                                                                                                          round_var_nm_1L_chr, " variable must include one that is named ",
                                                                                                                          round_bl_val_1L_chr, "."))
  bl_ds_tb <- data_tb %>% dplyr::filter(!!rlang::sym(round_var_nm_1L_chr) ==
                                          round_bl_val_1L_chr)
  fup_ds_tb <- data_tb %>% dplyr::filter(!!rlang::sym(round_var_nm_1L_chr) !=
                                           round_bl_val_1L_chr)
  bl_uids_xx <- bl_ds_tb %>% dplyr::pull(id_var_nm_1L_chr)
  fup_uids_xx <- fup_ds_tb %>% dplyr::pull(id_var_nm_1L_chr)
  assertthat::assert_that(nrow(bl_ds_tb) == length(unique(bl_uids_xx)) &
                            nrow(fup_ds_tb) == length(unique(fup_uids_xx)), msg = paste0("At each time-point (the ",
                                                                                         round_var_nm_1L_chr, " variable) there must be one unique record identifier (the ",
                                                                                         id_var_nm_1L_chr, " variable)."))
  if (!is.null(msrmnt_date_var_nm_1L_chr))
    assertthat::assert_that(lubridate::is.Date(data_tb %>%
                                                 dplyr::pull(msrmnt_date_var_nm_1L_chr)), msg = paste0(msrmnt_date_var_nm_1L_chr,
                                                                                                       " variable must be of date class."))
}

