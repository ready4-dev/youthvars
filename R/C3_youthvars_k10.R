
#' youthvars ready4 sub-module (S3 class) for Kessler Psychological Distress Scale (K10) - US Scoring System scores
#' @description Create a new valid instance of the youthvars ready4 sub-module (S3 class) for Kessler Psychological Distress Scale (K10) - US Scoring System scores
#' @param x A prototype for the youthvars ready4 sub-module (S3 class) for Kessler Psychological Distress Scale (K10) - US Scoring System scores, Default: make_pt_youthvars_k10()
#' @return A validated instance of the youthvars ready4 sub-module (S3 class) for Kessler Psychological Distress Scale (K10) - US Scoring System scores
#' @details youthvars ready4 sub-module (S3 class) for Kessler Psychological Distress Scale (K10) - US Scoring System scores
#' @rdname youthvars_k10
#' @export 
youthvars_k10 <- function(x = make_pt_youthvars_k10()){ 
validate_youthvars_k10(make_new_youthvars_k10(x))
}
#' make new youthvars K10 youthvars ready4 sub-module (S3 class) for Kessler Psychological Distress Scale (K10) - US Scoring System scores
#' @description Create a new unvalidated instance of the youthvars ready4 sub-module (S3 class) for Kessler Psychological Distress Scale (K10) - US Scoring System scores
#' @param x A prototype for the youthvars ready4 sub-module (S3 class) for Kessler Psychological Distress Scale (K10) - US Scoring System scores
#' @return An unvalidated instance of the youthvars ready4 sub-module (S3 class) for Kessler Psychological Distress Scale (K10) - US Scoring System scores
#' @details youthvars ready4 sub-module (S3 class) for Kessler Psychological Distress Scale (K10) - US Scoring System scores
#' @rdname make_new_youthvars_k10
#' @export 
#' @keywords internal
make_new_youthvars_k10 <- function(x){ 
stopifnot(is.integer(x))
class(x) <- append(c("youthvars_k10",setdiff(make_pt_youthvars_k10() %>% class(),class(x))),
class(x))
x
}
#' make prototype youthvars K10 youthvars ready4 sub-module (S3 class) for Kessler Psychological Distress Scale (K10) - US Scoring System scores

#' @return A prototype for youthvars ready4 sub-module (S3 class) for Kessler Psychological Distress Scale (K10) - US Scoring System scores
#' 
#' @rdname youthvars_k10
#' @export 
make_pt_youthvars_k10 <- function(){ 
integer(0)
}
#' validate youthvars K10 youthvars ready4 sub-module (S3 class) for Kessler Psychological Distress Scale (K10) - US Scoring System scores
#' @description Validate an instance of the youthvars ready4 sub-module (S3 class) for Kessler Psychological Distress Scale (K10) - US Scoring System scores
#' @param x An unvalidated instance of the youthvars ready4 sub-module (S3 class) for Kessler Psychological Distress Scale (K10) - US Scoring System scores
#' @return A prototpe for youthvars ready4 sub-module (S3 class) for Kessler Psychological Distress Scale (K10) - US Scoring System scores
#' @details youthvars ready4 sub-module (S3 class) for Kessler Psychological Distress Scale (K10) - US Scoring System scores
#' @rdname validate_youthvars_k10
#' @export 
#' @keywords internal
validate_youthvars_k10 <- function(x){
if(any(x[!is.na(x)] < 0)){
stop("All non-missing values in valid youthvars_k10 object must be greater than or equal to 0.",
call. = FALSE)
}

 if(any(x[!is.na(x)] > 40)){
stop("All non-missing values in valid youthvars_k10 object must be less than or equal to 40.",
call. = FALSE)
}

x}
#' is youthvars K10 youthvars ready4 sub-module (S3 class) for Kessler Psychological Distress Scale (K10) - US Scoring System scores
#' @param x An object of any type
#' @return A logical value, TRUE if a valid instance of the youthvars ready4 sub-module (S3 class) for Kessler Psychological Distress Scale (K10) - US Scoring System scores
#' 
#' @rdname youthvars_k10
#' @export 
is_youthvars_k10 <- function(x) inherits(validate_youthvars_k10(x), "youthvars_k10")
