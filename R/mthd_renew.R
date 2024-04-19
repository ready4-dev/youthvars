#' 
#' Renew (update) values
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
#' Renew (update) values
#' @name renew-YouthvarsProfile
#' @description renew method applied to YouthvarsProfile
#' @param x An object of class YouthvarsProfile
#' @param nbr_of_digits_1L_int Number of digits (an integer vector of length one), Default: 3
#' @param profile_chr Profile (a character vector), Default: character(0)
#' @param type_1L_chr Type (a character vector of length one), Default: 'characterize'
#' @param what_1L_chr What (a character vector of length one), Default: 'descriptives_ls'
#' @param ... Additional arguments
#' @return x (An object of class YouthvarsProfile)
#' @rdname renew-methods
#' @aliases renew,YouthvarsProfile-method
#' @export 
#' @importFrom purrr map
#' @importFrom ready4 renew
#' @importFrom methods callNextMethod
methods::setMethod("renew", "YouthvarsProfile", function (x, nbr_of_digits_1L_int = 3L, profile_chr = character(0), 
    type_1L_chr = "characterize", what_1L_chr = "descriptives_ls", 
    ...) 
{
    if (what_1L_chr == "descriptives_ls") {
        if (type_1L_chr == "manufacture" | (type_1L_chr == "characterize" && 
            identical(x@descriptives_ls, list(list())))) {
            x@descriptives_ls <- manufacture(x, nbr_of_digits_1L_int = nbr_of_digits_1L_int, 
                profile_chr = profile_chr, what_1L_chr = what_1L_chr, 
                ...)
        }
        if (type_1L_chr == "characterize") 
            x@descriptives_ls <- purrr::map(x@descriptives_ls, 
                ~ready4::renew(.x, y_Ready4useDyad = x@a_Ready4useDyad))
    }
    else {
        x <- methods::callNextMethod()
    }
    return(x)
})
