#' 
#' Renew (update) an object
#' @name renew-YouthvarsDescriptives
#' @description renew method applied to YouthvarsDescriptives
#' @param x An object of class YouthvarsDescriptives
#' @param type_1L_chr Type (a character vector of length one), Default: 'characterize'
#' @param ... Additional arguments
#' @return x (An object of class YouthvarsDescriptives)
#' @rdname renew-methods
#' @aliases renew,YouthvarsDescriptives-method
#' @export 
#' @importFrom ready4 renew
methods::setMethod("renew", "YouthvarsDescriptives", function (x, type_1L_chr = "characterize", ...) 
{
    if (type_1L_chr == "characterize") 
        x@descriptives_df <- characterize(x, ...)
    return(x)
})
#' 
#' Renew (update) an object
#' @name renew-YouthvarsProfile
#' @description renew method applied to YouthvarsProfile
#' @param x An object of class YouthvarsProfile
#' @param type_1L_chr Type (a character vector of length one), Default: 'characterize'
#' @return x (An object of class YouthvarsProfile)
#' @rdname renew-methods
#' @aliases renew,YouthvarsProfile-method
#' @export 
#' @importFrom purrr map
#' @importFrom ready4 renew
methods::setMethod("renew", "YouthvarsProfile", function (x, type_1L_chr = "characterize") 
{
    if (type_1L_chr == "characterize") 
        x@descriptives_ls <- purrr::map(x@descriptives_ls, ~ready4::renew(.x, 
            y_Ready4useDyad = x@a_Ready4useDyad))
    return(x)
})
