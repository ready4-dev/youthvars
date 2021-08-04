#' Assert dataset is valid
#' @description assert_ds_is_valid() is an Assert function that validates that an object conforms to required condition(s). If the object does not meet all required conditions, program execution will be stopped and an error message provided. Specifically, this function implements an algorithm to assert dataset is valid. Function argument data_tb specifies the object on which assert validation checks are to be performed. Argument id_var_nm_1L_chr provides the object containing values used for validation tests. The function is called for its side effects and does not return a value.
#' @param data_tb Data (a tibble)
#' @param id_var_nm_1L_chr Identity variable name (a character vector of length one)
#' @param round_var_nm_1L_chr Round variable name (a character vector of length one)
#' @param round_bl_val_1L_chr Round baseline value (a character vector of length one)
#' @param msrmnt_date_var_nm_1L_chr Measurement date variable name (a character vector of length one), Default: NULL
#' @return NULL
#' @rdname assert_ds_is_valid
#' @export 
#' @importFrom assertthat assert_that
#' @importFrom dplyr pull filter
#' @importFrom rlang sym
#' @importFrom lubridate is.Date
assert_ds_is_valid <- function (data_tb, id_var_nm_1L_chr, round_var_nm_1L_chr, round_bl_val_1L_chr, 
    msrmnt_date_var_nm_1L_chr = NULL) 
{
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
