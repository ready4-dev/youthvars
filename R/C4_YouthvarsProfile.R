#' YouthvarsProfile
#' 
#' A dataset and its associated dictionary, descriptive statistics and metadata.
#' 
#' @slot a_Ready4useDyad NO MATCH
#' @slot descriptives_ls Descriptives (a list)
#' @slot id_var_nm_1L_chr Identity variable name (a character vector of length one)
#' @slot dissemination_1L_chr Dissemination (a character vector of length one)
#' @import ready4
#' @name YouthvarsProfile-class
#' @rdname YouthvarsProfile-class
#' @export YouthvarsProfile
#' @exportClass YouthvarsProfile
YouthvarsProfile <- methods::setClass("YouthvarsProfile",
contains = "Ready4Module",
slots = c(a_Ready4useDyad = "Ready4useDyad",descriptives_ls = "list",id_var_nm_1L_chr = "character",dissemination_1L_chr = "character"),
prototype =  list(a_Ready4useDyad = ready4use::Ready4useDyad(),descriptives_ls = list(list()),id_var_nm_1L_chr = NA_character_))


methods::setValidity(methods::className("YouthvarsProfile"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
