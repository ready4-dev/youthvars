
#' youthvars S3 class for Screen for Child Anxiety Related Disorders (SCARED) scores
#' @description Create a new valid instance of the youthvars S3 class for Screen for Child Anxiety Related Disorders (SCARED) scores
#' @param x A prototype for the youthvars S3 class for Screen for Child Anxiety Related Disorders (SCARED) scores, Default: make_pt_youthvars_scared()
#' @return A validated instance of the youthvars S3 class for Screen for Child Anxiety Related Disorders (SCARED) scores
#' @details youthvars S3 class for Screen for Child Anxiety Related Disorders (SCARED) scores
#' @rdname youthvars_scared
#' @export 
youthvars_scared <- function(x = make_pt_youthvars_scared()){ 
validate_youthvars_scared(make_new_youthvars_scared(x))
}
#' make new youthvars scared youthvars S3 class for Screen for Child Anxiety Related Disorders (SCARED) scores
#' @description Create a new unvalidated instance of the youthvars S3 class for Screen for Child Anxiety Related Disorders (SCARED) scores
#' @param x A prototype for the youthvars S3 class for Screen for Child Anxiety Related Disorders (SCARED) scores
#' @return An unvalidated instance of the youthvars S3 class for Screen for Child Anxiety Related Disorders (SCARED) scores
#' @details youthvars S3 class for Screen for Child Anxiety Related Disorders (SCARED) scores
#' @rdname make_new_youthvars_scared
#' @export 
make_new_youthvars_scared <- function(x){ 
stopifnot(is.integer(x))
class(x) <- append(c("youthvars_scared",setdiff(make_pt_youthvars_scared() %>% class(),class(x))),
class(x))
x
}
#' make prototype youthvars scared youthvars S3 class for Screen for Child Anxiety Related Disorders (SCARED) scores
#' @description Create a new prototype for the youthvars S3 class for Screen for Child Anxiety Related Disorders (SCARED) scores

#' @return A prototype for youthvars S3 class for Screen for Child Anxiety Related Disorders (SCARED) scores
#' @details youthvars S3 class for Screen for Child Anxiety Related Disorders (SCARED) scores
#' @rdname make_pt_youthvars_scared
#' @export 
make_pt_youthvars_scared <- function(){ 
integer(0)
}
#' validate youthvars scared youthvars S3 class for Screen for Child Anxiety Related Disorders (SCARED) scores
#' @description Validate an instance of the youthvars S3 class for Screen for Child Anxiety Related Disorders (SCARED) scores
#' @param x An unvalidated instance of the youthvars S3 class for Screen for Child Anxiety Related Disorders (SCARED) scores
#' @return A prototpe for youthvars S3 class for Screen for Child Anxiety Related Disorders (SCARED) scores
#' @details youthvars S3 class for Screen for Child Anxiety Related Disorders (SCARED) scores
#' @rdname validate_youthvars_scared
#' @export 
validate_youthvars_scared <- function(x){
if(any(x[!is.na(x)] < 0)){
stop("All non-missing values in valid youthvars_scared object must be greater than or equal to 0.",
call. = FALSE)
}

 if(any(x[!is.na(x)] > 82)){
stop("All non-missing values in valid youthvars_scared object must be less than or equal to 82.",
call. = FALSE)
}

x}
#' is youthvars scared youthvars S3 class for Screen for Child Anxiety Related Disorders (SCARED) scores
#' @description Check whether an object is a valid instance of the youthvars S3 class for Screen for Child Anxiety Related Disorders (SCARED) scores
#' @param x An object of any type
#' @return A logical value, TRUE if a valid instance of the youthvars S3 class for Screen for Child Anxiety Related Disorders (SCARED) scores
#' @details youthvars S3 class for Screen for Child Anxiety Related Disorders (SCARED) scores
#' @rdname is_youthvars_scared
#' @export 
is_youthvars_scared <- function(x) inherits(validate_youthvars_scared(x), "youthvars_scared")
