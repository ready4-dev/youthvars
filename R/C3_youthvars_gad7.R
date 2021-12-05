
#' youthvars S3 class for Generalised Anxiety Disorder Scale (GAD-7) scores
#' @description Create a new valid instance of the youthvars S3 class for Generalised Anxiety Disorder Scale (GAD-7) scores
#' @param x A prototype for the youthvars S3 class for Generalised Anxiety Disorder Scale (GAD-7) scores, Default: make_pt_youthvars_gad7()
#' @return A validated instance of the youthvars S3 class for Generalised Anxiety Disorder Scale (GAD-7) scores
#' @details youthvars S3 class for Generalised Anxiety Disorder Scale (GAD-7) scores
#' @rdname youthvars_gad7
#' @export 
youthvars_gad7 <- function(x = make_pt_youthvars_gad7()){ 
validate_youthvars_gad7(make_new_youthvars_gad7(x))
}
#' make new youthvars GAD-7 youthvars S3 class for Generalised Anxiety Disorder Scale (GAD-7) scores
#' @description Create a new unvalidated instance of the youthvars S3 class for Generalised Anxiety Disorder Scale (GAD-7) scores
#' @param x A prototype for the youthvars S3 class for Generalised Anxiety Disorder Scale (GAD-7) scores
#' @return An unvalidated instance of the youthvars S3 class for Generalised Anxiety Disorder Scale (GAD-7) scores
#' @details youthvars S3 class for Generalised Anxiety Disorder Scale (GAD-7) scores
#' @rdname make_new_youthvars_gad7
#' @export 
#' @keywords internal
make_new_youthvars_gad7 <- function(x){ 
stopifnot(is.integer(x))
class(x) <- append(c("youthvars_gad7",setdiff(make_pt_youthvars_gad7() %>% class(),class(x))),
class(x))
x
}
#' make prototype youthvars GAD-7 youthvars S3 class for Generalised Anxiety Disorder Scale (GAD-7) scores

#' @return A prototype for youthvars S3 class for Generalised Anxiety Disorder Scale (GAD-7) scores
#' 
#' @rdname youthvars_gad7
#' @export 
make_pt_youthvars_gad7 <- function(){ 
integer(0)
}
#' validate youthvars GAD-7 youthvars S3 class for Generalised Anxiety Disorder Scale (GAD-7) scores
#' @description Validate an instance of the youthvars S3 class for Generalised Anxiety Disorder Scale (GAD-7) scores
#' @param x An unvalidated instance of the youthvars S3 class for Generalised Anxiety Disorder Scale (GAD-7) scores
#' @return A prototpe for youthvars S3 class for Generalised Anxiety Disorder Scale (GAD-7) scores
#' @details youthvars S3 class for Generalised Anxiety Disorder Scale (GAD-7) scores
#' @rdname validate_youthvars_gad7
#' @export 
#' @keywords internal
validate_youthvars_gad7 <- function(x){
if(any(x[!is.na(x)] < 0)){
stop("All non-missing values in valid youthvars_gad7 object must be greater than or equal to 0.",
call. = FALSE)
}

 if(any(x[!is.na(x)] > 21)){
stop("All non-missing values in valid youthvars_gad7 object must be less than or equal to 21.",
call. = FALSE)
}

x}
#' is youthvars GAD-7 youthvars S3 class for Generalised Anxiety Disorder Scale (GAD-7) scores
#' @param x An object of any type
#' @return A logical value, TRUE if a valid instance of the youthvars S3 class for Generalised Anxiety Disorder Scale (GAD-7) scores
#' 
#' @rdname youthvars_gad7
#' @export 
is_youthvars_gad7 <- function(x) inherits(validate_youthvars_gad7(x), "youthvars_gad7")
