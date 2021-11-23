#' YouthvarsDescriptives
#' 
#' Metadata about descriptive statistics to be generated.
#' 
#' @slot descriptives_df Descriptives (a data.frame)
#' @slot ds_tfmn_ls Dataset transformation (a list)
#' @slot key_var_nm_1L_chr Key variable name (a character vector of length one)
#' @slot key_var_vals_chr Key variable values (a character vector)
#' @slot nbr_of_digits_1L_int Number of digits (an integer vector of length one)
#' @slot profiled_vars_chr Profiled variables (a character vector)
#' @slot sections_as_row_1L_lgl Sections as row (a logical vector of length one)
#' @slot test_1L_lgl Test (a logical vector of length one)
#' @slot dissemination_1L_chr Dissemination (a character vector of length one)
#' @import ready4
#' @name YouthvarsDescriptives-class
#' @rdname YouthvarsDescriptives-class
#' @export YouthvarsDescriptives
#' @exportClass YouthvarsDescriptives
YouthvarsDescriptives <- methods::setClass("YouthvarsDescriptives",
contains = "Ready4Module",
slots = c(descriptives_df = "data.frame",ds_tfmn_ls = "list",key_var_nm_1L_chr = "character",key_var_vals_chr = "character",nbr_of_digits_1L_int = "integer",profiled_vars_chr = "character",sections_as_row_1L_lgl = "logical",test_1L_lgl = "logical",dissemination_1L_chr = "character"),
prototype =  list(descriptives_df = data.frame(),ds_tfmn_ls = list(list()),key_var_nm_1L_chr = NA_character_,key_var_vals_chr = NA_character_,nbr_of_digits_1L_int = NA_integer_,profiled_vars_chr = NA_character_,sections_as_row_1L_lgl = NA,test_1L_lgl = NA))


methods::setValidity(methods::className("YouthvarsDescriptives"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
