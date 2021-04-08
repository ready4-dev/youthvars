
#' youthvars S3 class for Assessment of Quality of Life Six Dimension Health Utility - Adolescent Version (AQoL6d Adolescent))
#' @description Create a new valid instance of the youthvars S3 class for Assessment of Quality of Life Six Dimension Health Utility - Adolescent Version (AQoL6d Adolescent))
#' @param x A prototype for the youthvars S3 class for Assessment of Quality of Life Six Dimension Health Utility - Adolescent Version (AQoL6d Adolescent)), Default: make_pt_youthvars_aqol6d_adol()
#' @return A validated instance of the youthvars S3 class for Assessment of Quality of Life Six Dimension Health Utility - Adolescent Version (AQoL6d Adolescent))
#' @details youthvars S3 class for Assessment of Quality of Life Six Dimension Health Utility - Adolescent Version (AQoL6d Adolescent))
#' @rdname youthvars_aqol6d_adol
#' @export 

youthvars_aqol6d_adol <- function(x = make_pt_youthvars_aqol6d_adol()){ 
validate_youthvars_aqol6d_adol(make_new_youthvars_aqol6d_adol(x))
}
#' Make new youthvars S3 class for Assessment of Quality of Life Six Dimension Health Utility - Adolescent Version (AQoL6d Adolescent))
#' @description Create a new unvalidated instance of the youthvars S3 class for Assessment of Quality of Life Six Dimension Health Utility - Adolescent Version (AQoL6d Adolescent))
#' @param x A prototype for the youthvars S3 class for Assessment of Quality of Life Six Dimension Health Utility - Adolescent Version (AQoL6d Adolescent))
#' @return An unvalidated instance of the youthvars S3 class for Assessment of Quality of Life Six Dimension Health Utility - Adolescent Version (AQoL6d Adolescent))
#' @details youthvars S3 class for Assessment of Quality of Life Six Dimension Health Utility - Adolescent Version (AQoL6d Adolescent))
#' @rdname make_new_youthvars_aqol6d_adol
#' @export 

make_new_youthvars_aqol6d_adol <- function(x){ 
stopifnot(is.numeric(x))
class(x) <- append(c("youthvars_aqol6d_adol",setdiff(make_pt_youthvars_aqol6d_adol() %>% class(),class(x))),
class(x))
x
}
#' Make prototype youthvars S3 class for Assessment of Quality of Life Six Dimension Health Utility - Adolescent Version (AQoL6d Adolescent))
#' @description Create a new prototype for the youthvars S3 class for Assessment of Quality of Life Six Dimension Health Utility - Adolescent Version (AQoL6d Adolescent))

#' @return A prototype for youthvars S3 class for Assessment of Quality of Life Six Dimension Health Utility - Adolescent Version (AQoL6d Adolescent))
#' @details youthvars S3 class for Assessment of Quality of Life Six Dimension Health Utility - Adolescent Version (AQoL6d Adolescent))
#' @rdname make_pt_youthvars_aqol6d_adol
#' @export 

make_pt_youthvars_aqol6d_adol <- function(){ 
numeric(0)
}
#' Validate youthvars S3 class for Assessment of Quality of Life Six Dimension Health Utility - Adolescent Version (AQoL6d Adolescent))
#' @description Validate an instance of the youthvars S3 class for Assessment of Quality of Life Six Dimension Health Utility - Adolescent Version (AQoL6d Adolescent))
#' @param x An unvalidated instance of the youthvars S3 class for Assessment of Quality of Life Six Dimension Health Utility - Adolescent Version (AQoL6d Adolescent))
#' @return A prototpe for youthvars S3 class for Assessment of Quality of Life Six Dimension Health Utility - Adolescent Version (AQoL6d Adolescent))
#' @details youthvars S3 class for Assessment of Quality of Life Six Dimension Health Utility - Adolescent Version (AQoL6d Adolescent))
#' @rdname validate_youthvars_aqol6d_adol
#' @export 

validate_youthvars_aqol6d_adol <- function(x){
if(any(x[!is.na(x)] < 0.03)){
stop("All non-missing values in valid youthvars_aqol6d_adol object must be greater than or equal to 0.03.",
call. = FALSE)
}

 if(any(x[!is.na(x)] > 1)){
stop("All non-missing values in valid youthvars_aqol6d_adol object must be less than or equal to 1.",
call. = FALSE)
}

x}
#' Is youthvars S3 class for Assessment of Quality of Life Six Dimension Health Utility - Adolescent Version (AQoL6d Adolescent))
#' @description Check whether an object is a valid instance of the youthvars S3 class for Assessment of Quality of Life Six Dimension Health Utility - Adolescent Version (AQoL6d Adolescent))
#' @param x An object of any type
#' @return A logical value, TRUE if a valid instance of the youthvars S3 class for Assessment of Quality of Life Six Dimension Health Utility - Adolescent Version (AQoL6d Adolescent))
#' @details youthvars S3 class for Assessment of Quality of Life Six Dimension Health Utility - Adolescent Version (AQoL6d Adolescent))
#' @rdname is_youthvars_aqol6d_adol
#' @export 

is_youthvars_aqol6d_adol <- function(x) inherits(validate_youthvars_aqol6d_adol(x), "youthvars_aqol6d_adol")
