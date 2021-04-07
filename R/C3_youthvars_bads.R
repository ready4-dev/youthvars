
#' First Bounce S3 class for Behavioural Activation for Depression Scale (BADS) scores
#' @description Create a new valid instance of the First Bounce S3 class for Behavioural Activation for Depression Scale (BADS) scores
#' @param x A prototype for the First Bounce S3 class for Behavioural Activation for Depression Scale (BADS) scores, Default: make_pt_youthvars_bads()
#' @return A validated instance of the First Bounce S3 class for Behavioural Activation for Depression Scale (BADS) scores
#' @details First Bounce S3 class for Behavioural Activation for Depression Scale (BADS) scores
#' @rdname youthvars_bads
#' @export 

youthvars_bads <- function(x = make_pt_youthvars_bads()){ 
validate_youthvars_bads(make_new_youthvars_bads(x))
}
#' Make new First Bounce S3 class for Behavioural Activation for Depression Scale (BADS) scores
#' @description Create a new unvalidated instance of the First Bounce S3 class for Behavioural Activation for Depression Scale (BADS) scores
#' @param x A prototype for the First Bounce S3 class for Behavioural Activation for Depression Scale (BADS) scores
#' @return An unvalidated instance of the First Bounce S3 class for Behavioural Activation for Depression Scale (BADS) scores
#' @details First Bounce S3 class for Behavioural Activation for Depression Scale (BADS) scores
#' @rdname make_new_youthvars_bads
#' @export 

make_new_youthvars_bads <- function(x){ 
stopifnot(is.integer(x))
class(x) <- append(c("youthvars_bads",setdiff(make_pt_youthvars_bads() %>% class(),class(x))),
class(x))
x
}
#' Make prototype First Bounce S3 class for Behavioural Activation for Depression Scale (BADS) scores
#' @description Create a new prototype for the First Bounce S3 class for Behavioural Activation for Depression Scale (BADS) scores

#' @return A prototype for First Bounce S3 class for Behavioural Activation for Depression Scale (BADS) scores
#' @details First Bounce S3 class for Behavioural Activation for Depression Scale (BADS) scores
#' @rdname make_pt_youthvars_bads
#' @export 

make_pt_youthvars_bads <- function(){ 
integer(0)
}
#' Validate First Bounce S3 class for Behavioural Activation for Depression Scale (BADS) scores
#' @description Validate an instance of the First Bounce S3 class for Behavioural Activation for Depression Scale (BADS) scores
#' @param x An unvalidated instance of the First Bounce S3 class for Behavioural Activation for Depression Scale (BADS) scores
#' @return A prototpe for First Bounce S3 class for Behavioural Activation for Depression Scale (BADS) scores
#' @details First Bounce S3 class for Behavioural Activation for Depression Scale (BADS) scores
#' @rdname validate_youthvars_bads
#' @export 

validate_youthvars_bads <- function(x){
if(any(x[!is.na(x)] < 0)){
stop("All non-missing values in valid youthvars_bads object must be greater than or equal to 0.",
call. = FALSE)
}

 if(any(x[!is.na(x)] > 150)){
stop("All non-missing values in valid youthvars_bads object must be less than or equal to 150.",
call. = FALSE)
}

x}
#' Is First Bounce S3 class for Behavioural Activation for Depression Scale (BADS) scores
#' @description Check whether an object is a valid instance of the First Bounce S3 class for Behavioural Activation for Depression Scale (BADS) scores
#' @param x An object of any type
#' @return A logical value, TRUE if a valid instance of the First Bounce S3 class for Behavioural Activation for Depression Scale (BADS) scores
#' @details First Bounce S3 class for Behavioural Activation for Depression Scale (BADS) scores
#' @rdname is_youthvars_bads
#' @export 

is_youthvars_bads <- function(x) inherits(validate_youthvars_bads(x), "youthvars_bads")
