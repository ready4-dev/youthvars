
#' youthvars ready4 sub-module (S3 class) for Kessler Psychological Distress Scale (K10) - Australian Scoring System scores
#' @description Create a new valid instance of the youthvars ready4 sub-module (S3 class) for Kessler Psychological Distress Scale (K10) - Australian Scoring System scores
#' @param x A prototype for the youthvars ready4 sub-module (S3 class) for Kessler Psychological Distress Scale (K10) - Australian Scoring System scores, Default: make_pt_youthvars_k10_aus()
#' @return A validated instance of the youthvars ready4 sub-module (S3 class) for Kessler Psychological Distress Scale (K10) - Australian Scoring System scores
#' @details youthvars ready4 sub-module (S3 class) for Kessler Psychological Distress Scale (K10) - Australian Scoring System scores
#' @rdname youthvars_k10_aus
#' @export 
youthvars_k10_aus <- function(x = make_pt_youthvars_k10_aus()){ 
validate_youthvars_k10_aus(make_new_youthvars_k10_aus(x))
}
#' make new youthvars K10 Australia youthvars ready4 sub-module (S3 class) for Kessler Psychological Distress Scale (K10) - Australian Scoring System scores
#' @description Create a new unvalidated instance of the youthvars ready4 sub-module (S3 class) for Kessler Psychological Distress Scale (K10) - Australian Scoring System scores
#' @param x A prototype for the youthvars ready4 sub-module (S3 class) for Kessler Psychological Distress Scale (K10) - Australian Scoring System scores
#' @return An unvalidated instance of the youthvars ready4 sub-module (S3 class) for Kessler Psychological Distress Scale (K10) - Australian Scoring System scores
#' @details youthvars ready4 sub-module (S3 class) for Kessler Psychological Distress Scale (K10) - Australian Scoring System scores
#' @rdname make_new_youthvars_k10_aus
#' @export 
#' @keywords internal
make_new_youthvars_k10_aus <- function(x){ 
stopifnot(is.integer(x))
class(x) <- append(c("youthvars_k10_aus",setdiff(make_pt_youthvars_k10_aus() %>% class(),class(x))),
class(x))
x
}
#' make prototype youthvars K10 Australia youthvars ready4 sub-module (S3 class) for Kessler Psychological Distress Scale (K10) - Australian Scoring System scores

#' @return A prototype for youthvars ready4 sub-module (S3 class) for Kessler Psychological Distress Scale (K10) - Australian Scoring System scores
#' 
#' @rdname youthvars_k10_aus
#' @export 
make_pt_youthvars_k10_aus <- function(){ 
integer(0)
}
#' validate youthvars K10 Australia youthvars ready4 sub-module (S3 class) for Kessler Psychological Distress Scale (K10) - Australian Scoring System scores
#' @description Validate an instance of the youthvars ready4 sub-module (S3 class) for Kessler Psychological Distress Scale (K10) - Australian Scoring System scores
#' @param x An unvalidated instance of the youthvars ready4 sub-module (S3 class) for Kessler Psychological Distress Scale (K10) - Australian Scoring System scores
#' @return A prototpe for youthvars ready4 sub-module (S3 class) for Kessler Psychological Distress Scale (K10) - Australian Scoring System scores
#' @details youthvars ready4 sub-module (S3 class) for Kessler Psychological Distress Scale (K10) - Australian Scoring System scores
#' @rdname validate_youthvars_k10_aus
#' @export 
#' @keywords internal
validate_youthvars_k10_aus <- function(x){
if(any(x[!is.na(x)] < 10)){
stop("All non-missing values in valid youthvars_k10_aus object must be greater than or equal to 10.",
call. = FALSE)
}

 if(any(x[!is.na(x)] > 50)){
stop("All non-missing values in valid youthvars_k10_aus object must be less than or equal to 50.",
call. = FALSE)
}

x}
#' is youthvars K10 Australia youthvars ready4 sub-module (S3 class) for Kessler Psychological Distress Scale (K10) - Australian Scoring System scores
#' @param x An object of any type
#' @return A logical value, TRUE if a valid instance of the youthvars ready4 sub-module (S3 class) for Kessler Psychological Distress Scale (K10) - Australian Scoring System scores
#' 
#' @rdname youthvars_k10_aus
#' @export 
is_youthvars_k10_aus <- function(x) inherits(validate_youthvars_k10_aus(x), "youthvars_k10_aus")
