
#' youthvars S3 class for Behavioural Activation for Depression Scale (BADS) scores
#' @description Create a new valid instance of the youthvars S3 class for Behavioural Activation for Depression Scale (BADS) scores
#' @param x A prototype for the youthvars S3 class for Behavioural Activation for Depression Scale (BADS) scores, Default: make_pt_youthvars_bads()
#' @return A validated instance of the youthvars S3 class for Behavioural Activation for Depression Scale (BADS) scores
#' @details youthvars S3 class for Behavioural Activation for Depression Scale (BADS) scores
#' @rdname youthvars_bads
#' @export 
youthvars_bads <- function(x = make_pt_youthvars_bads()){ 
validate_youthvars_bads(make_new_youthvars_bads(x))
}
#' make new youthvars bads youthvars S3 class for Behavioural Activation for Depression Scale (BADS) scores
#' @description Create a new unvalidated instance of the youthvars S3 class for Behavioural Activation for Depression Scale (BADS) scores
#' @param x A prototype for the youthvars S3 class for Behavioural Activation for Depression Scale (BADS) scores
#' @return An unvalidated instance of the youthvars S3 class for Behavioural Activation for Depression Scale (BADS) scores
#' @details youthvars S3 class for Behavioural Activation for Depression Scale (BADS) scores
#' @rdname make_new_youthvars_bads
#' @export 
#' @keywords internal
make_new_youthvars_bads <- function(x){ 
stopifnot(is.integer(x))
class(x) <- append(c("youthvars_bads",setdiff(make_pt_youthvars_bads() %>% class(),class(x))),
class(x))
x
}
#' make prototype youthvars bads youthvars S3 class for Behavioural Activation for Depression Scale (BADS) scores

#' @return A prototype for youthvars S3 class for Behavioural Activation for Depression Scale (BADS) scores
#' 
#' @rdname youthvars_bads
#' @export 
make_pt_youthvars_bads <- function(){ 
integer(0)
}
#' validate youthvars bads youthvars S3 class for Behavioural Activation for Depression Scale (BADS) scores
#' @description Validate an instance of the youthvars S3 class for Behavioural Activation for Depression Scale (BADS) scores
#' @param x An unvalidated instance of the youthvars S3 class for Behavioural Activation for Depression Scale (BADS) scores
#' @return A prototpe for youthvars S3 class for Behavioural Activation for Depression Scale (BADS) scores
#' @details youthvars S3 class for Behavioural Activation for Depression Scale (BADS) scores
#' @rdname validate_youthvars_bads
#' @export 
#' @keywords internal
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
#' is youthvars bads youthvars S3 class for Behavioural Activation for Depression Scale (BADS) scores
#' @param x An object of any type
#' @return A logical value, TRUE if a valid instance of the youthvars S3 class for Behavioural Activation for Depression Scale (BADS) scores
#' 
#' @rdname youthvars_bads
#' @export 
is_youthvars_bads <- function(x) inherits(validate_youthvars_bads(x), "youthvars_bads")
