#' YouthvarsSeries
#' 
#' A longitudinal dataset and its associated dictionary, descriptive statistics and metadata.
#' 
#' @include C4_YouthvarsProfile.R
#' @slot participation_var_1L_chr Participation variable (a character vector of length one)
#' @slot timepoint_vals_chr Timepoint values (a character vector)
#' @slot timepoint_var_nm_1L_chr Timepoint variable name (a character vector of length one)
#' @slot a_Ready4useDyad  (an instance of the Ready4useDyad class)
#' @slot descriptives_ls Descriptives (a list)
#' @slot id_var_nm_1L_chr Identity variable name (a character vector of length one)
#' @slot dissemination_1L_chr Dissemination (a character vector of length one)
#' @name YouthvarsSeries-class
#' @rdname YouthvarsSeries-class
#' @export YouthvarsSeries
#' @exportClass YouthvarsSeries
YouthvarsSeries <- methods::setClass("YouthvarsSeries",
contains = "YouthvarsProfile",
slots = c(participation_var_1L_chr = "character",timepoint_vals_chr = "character",timepoint_var_nm_1L_chr = "character",a_Ready4useDyad = "Ready4useDyad",descriptives_ls = "list",id_var_nm_1L_chr = "character",dissemination_1L_chr = "character"),
prototype =  list(participation_var_1L_chr = NA_character_,timepoint_vals_chr = NA_character_,timepoint_var_nm_1L_chr = NA_character_))


methods::setValidity(methods::className("YouthvarsSeries"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
