
#' youthvars ready4 sub-module (S3 class) for Overall Anxiety Severity and Impairment Scale (OASIS) scores
#' @description Create a new valid instance of the youthvars ready4 sub-module (S3 class) for Overall Anxiety Severity and Impairment Scale (OASIS) scores
#' @param x A prototype for the youthvars ready4 sub-module (S3 class) for Overall Anxiety Severity and Impairment Scale (OASIS) scores, Default: make_pt_youthvars_oasis()
#' @return A validated instance of the youthvars ready4 sub-module (S3 class) for Overall Anxiety Severity and Impairment Scale (OASIS) scores
#' @details youthvars ready4 sub-module (S3 class) for Overall Anxiety Severity and Impairment Scale (OASIS) scores
#' @rdname youthvars_oasis
#' @export 
youthvars_oasis <- function(x = make_pt_youthvars_oasis()){ 
validate_youthvars_oasis(make_new_youthvars_oasis(x))
}
#' make new youthvars oasis youthvars ready4 sub-module (S3 class) for Overall Anxiety Severity and Impairment Scale (OASIS) scores
#' @description Create a new unvalidated instance of the youthvars ready4 sub-module (S3 class) for Overall Anxiety Severity and Impairment Scale (OASIS) scores
#' @param x A prototype for the youthvars ready4 sub-module (S3 class) for Overall Anxiety Severity and Impairment Scale (OASIS) scores
#' @return An unvalidated instance of the youthvars ready4 sub-module (S3 class) for Overall Anxiety Severity and Impairment Scale (OASIS) scores
#' @details youthvars ready4 sub-module (S3 class) for Overall Anxiety Severity and Impairment Scale (OASIS) scores
#' @rdname make_new_youthvars_oasis
#' @export 
#' @keywords internal
make_new_youthvars_oasis <- function(x){ 
stopifnot(is.integer(x))
class(x) <- append(c("youthvars_oasis",setdiff(make_pt_youthvars_oasis() %>% class(),class(x))),
class(x))
x
}
#' make prototype youthvars oasis youthvars ready4 sub-module (S3 class) for Overall Anxiety Severity and Impairment Scale (OASIS) scores

#' @return A prototype for youthvars ready4 sub-module (S3 class) for Overall Anxiety Severity and Impairment Scale (OASIS) scores
#' 
#' @rdname youthvars_oasis
#' @export 
make_pt_youthvars_oasis <- function(){ 
integer(0)
}
#' validate youthvars oasis youthvars ready4 sub-module (S3 class) for Overall Anxiety Severity and Impairment Scale (OASIS) scores
#' @description Validate an instance of the youthvars ready4 sub-module (S3 class) for Overall Anxiety Severity and Impairment Scale (OASIS) scores
#' @param x An unvalidated instance of the youthvars ready4 sub-module (S3 class) for Overall Anxiety Severity and Impairment Scale (OASIS) scores
#' @return A prototpe for youthvars ready4 sub-module (S3 class) for Overall Anxiety Severity and Impairment Scale (OASIS) scores
#' @details youthvars ready4 sub-module (S3 class) for Overall Anxiety Severity and Impairment Scale (OASIS) scores
#' @rdname validate_youthvars_oasis
#' @export 
#' @keywords internal
validate_youthvars_oasis <- function(x){
if(any(x[!is.na(x)] < 0)){
stop("All non-missing values in valid youthvars_oasis object must be greater than or equal to 0.",
call. = FALSE)
}

 if(any(x[!is.na(x)] > 20)){
stop("All non-missing values in valid youthvars_oasis object must be less than or equal to 20.",
call. = FALSE)
}

x}
#' is youthvars oasis youthvars ready4 sub-module (S3 class) for Overall Anxiety Severity and Impairment Scale (OASIS) scores
#' @param x An object of any type
#' @return A logical value, TRUE if a valid instance of the youthvars ready4 sub-module (S3 class) for Overall Anxiety Severity and Impairment Scale (OASIS) scores
#' 
#' @rdname youthvars_oasis
#' @export 
is_youthvars_oasis <- function(x) inherits(validate_youthvars_oasis(x), "youthvars_oasis")
