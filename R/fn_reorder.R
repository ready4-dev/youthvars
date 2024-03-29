#' Reorder tibbles for target correlations
#' @description reorder_tbs_for_target_cors() is a Reorder function that reorders an object to conform to a pre-specified schema. Specifically, this function implements an algorithm to reorder tibbles for target correlations. The function returns Tibbles (a list).
#' @param tbs_ls Tibbles (a list)
#' @param cor_dbl Correlation (a double vector)
#' @param cor_var_chr Correlation variable (a character vector)
#' @param id_var_to_rmv_1L_chr Identity variable to remove (a character vector of length one), Default: 'NA'
#' @return Tibbles (a list)
#' @rdname reorder_tbs_for_target_cors
#' @export 
#' @importFrom MASS mvrnorm
#' @importFrom dplyr slice arrange bind_rows select
#' @importFrom rlang sym
#' @importFrom purrr map
#' @keywords internal
reorder_tbs_for_target_cors <- function (tbs_ls, cor_dbl, cor_var_chr, id_var_to_rmv_1L_chr = NA_character_) 
{
    n_fup_dbl <- nrow(tbs_ls[[2]])
    cor_mat <- matrix(cor_dbl, ncol = 2, nrow = 2)
    diag(cor_mat) <- 1
    mvdat_mat <- MASS::mvrnorm(n = n_fup_dbl, mu = c(0, 0), Sigma = cor_mat, 
        empirical = TRUE)
    rank_x_int <- rank(mvdat_mat[, 1], ties.method = "first")
    rank_y_int <- rank(mvdat_mat[, 2], ties.method = "first")
    matched_tb <- tbs_ls[[1]] %>% dplyr::slice(tbs_ls[[2]]$id) %>% 
        dplyr::arrange(!!rlang::sym(cor_var_chr[1])) %>% dplyr::slice(rank_x_int)
    unmatched_int <- setdiff(1:nrow(tbs_ls[[1]]), tbs_ls[[2]]$id)
    if (!identical(unmatched_int, character(0))) {
        tbs_ls[[1]] <- dplyr::bind_rows(matched_tb, tbs_ls[[1]] %>% 
            dplyr::slice(unmatched_int))
    }
    else {
        tbs_ls[[1]] <- matched_tb
    }
    tbs_ls[[2]] <- tbs_ls[[2]] %>% dplyr::arrange(!!rlang::sym(cor_var_chr[2])) %>% 
        dplyr::slice(rank_y_int)
    if (!is.na(id_var_to_rmv_1L_chr)) 
        tbs_ls <- tbs_ls %>% purrr::map(~.x %>% dplyr::select(-!!rlang::sym(id_var_to_rmv_1L_chr)))
    return(tbs_ls)
}
