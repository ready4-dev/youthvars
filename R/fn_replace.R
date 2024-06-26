#' Replace with missing values
#' @description replace_with_missing_vals() is a Replace function that edits an object, replacing a specified element with another specified element. Specifically, this function implements an algorithm to replace with missing values. The function is called for its side effects and does not return a value.
#' @param data_tbl_tb Data table (a tibble)
#' @param synth_data_spine_ls Synthetic data spine (a list)
#' @param idx_int Index (an integer vector)
#' @return Synthetic (a table)
#' @rdname replace_with_missing_vals
#' @export 
#' @importFrom purrr reduce
#' @importFrom simstudy defMiss genMiss genObs
#' @keywords internal
replace_with_missing_vals <- function (data_tbl_tb, synth_data_spine_ls, idx_int) 
{
    missing_def_tbl <- purrr::reduce(1:length(synth_data_spine_ls$var_names_chr), 
        .init = NULL, ~simstudy::defMiss(.x, varname = synth_data_spine_ls$var_names_chr[.y], 
            formula = synth_data_spine_ls$missing_ls[[idx_int]][.y]/synth_data_spine_ls$nbr_obs_dbl[idx_int], 
            logit.link = FALSE))
    missing_mat <- simstudy::genMiss(data_tbl_tb, missing_def_tbl, 
        idvars = "id")
    synth_tbl <- simstudy::genObs(data_tbl_tb, missing_mat, idvars = "id")
    return(synth_tbl)
}
