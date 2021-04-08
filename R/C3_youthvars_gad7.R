
#' First Bounce S3 class for Generalised Anxiety Disorder Scale (GAD-7) scores
#' @description Create a new valid instance of the First Bounce S3 class for Generalised Anxiety Disorder Scale (GAD-7) scores
#' @param x A prototype for the First Bounce S3 class for Generalised Anxiety Disorder Scale (GAD-7) scores, Default: make_pt_youthvars_gad7()
#' @return A validated instance of the First Bounce S3 class for Generalised Anxiety Disorder Scale (GAD-7) scores
#' @details First Bounce S3 class for Generalised Anxiety Disorder Scale (GAD-7) scores
#' @rdname youthvars_gad7
#' @export 

youthvars_gad7 <- function(x = make_pt_youthvars_gad7()){ 
validate_youthvars_gad7(make_new_youthvars_gad7(x))
}
#' Make new First Bounce S3 class for Generalised Anxiety Disorder Scale (GAD-7) scores
#' @description Create a new unvalidated instance of the First Bounce S3 class for Generalised Anxiety Disorder Scale (GAD-7) scores
#' @param x A prototype for the First Bounce S3 class for Generalised Anxiety Disorder Scale (GAD-7) scores
#' @return An unvalidated instance of the First Bounce S3 class for Generalised Anxiety Disorder Scale (GAD-7) scores
#' @details First Bounce S3 class for Generalised Anxiety Disorder Scale (GAD-7) scores
#' @rdname make_new_youthvars_gad7
#' @export 

make_new_youthvars_gad7 <- function(x){ 
stopifnot(is.integer(x))
class(x) <- append(c("youthvars_gad7",setdiff(make_pt_youthvars_gad7() %>% class(),class(x))),
class(x))
x
}
#' Make prototype First Bounce S3 class for Generalised Anxiety Disorder Scale (GAD-7) scores
#' @description Create a new prototype for the First Bounce S3 class for Generalised Anxiety Disorder Scale (GAD-7) scores

#' @return A prototype for First Bounce S3 class for Generalised Anxiety Disorder Scale (GAD-7) scores
#' @details First Bounce S3 class for Generalised Anxiety Disorder Scale (GAD-7) scores
#' @rdname make_pt_youthvars_gad7
#' @export 

make_pt_youthvars_gad7 <- function(){ 
integer(0)
}
#' Validate First Bounce S3 class for Generalised Anxiety Disorder Scale (GAD-7) scores
#' @description Validate an instance of the First Bounce S3 class for Generalised Anxiety Disorder Scale (GAD-7) scores
#' @param x An unvalidated instance of the First Bounce S3 class for Generalised Anxiety Disorder Scale (GAD-7) scores
#' @return A prototpe for First Bounce S3 class for Generalised Anxiety Disorder Scale (GAD-7) scores
#' @details First Bounce S3 class for Generalised Anxiety Disorder Scale (GAD-7) scores
#' @rdname validate_youthvars_gad7
#' @export 

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
#' Is First Bounce S3 class for Generalised Anxiety Disorder Scale (GAD-7) scores
#' @description Check whether an object is a valid instance of the First Bounce S3 class for Generalised Anxiety Disorder Scale (GAD-7) scores
#' @param x An object of any type
#' @return A logical value, TRUE if a valid instance of the First Bounce S3 class for Generalised Anxiety Disorder Scale (GAD-7) scores
#' @details First Bounce S3 class for Generalised Anxiety Disorder Scale (GAD-7) scores
#' @rdname is_youthvars_gad7
#' @export 

is_youthvars_gad7 <- function(x) inherits(validate_youthvars_gad7(x), "youthvars_gad7")