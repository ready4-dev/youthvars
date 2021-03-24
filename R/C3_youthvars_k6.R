
#' First Bounce S3 class for Kessler Psychological Distress Scale (K6) - US Scoring System scores
#' @description Create a new valid instance of the First Bounce S3 class for Kessler Psychological Distress Scale (K6) - US Scoring System scores
#' @param x A prototype for the First Bounce S3 class for Kessler Psychological Distress Scale (K6) - US Scoring System scores, Default: make_pt_youthvars_k6()
#' @return A validated instance of the First Bounce S3 class for Kessler Psychological Distress Scale (K6) - US Scoring System scores
#' @details First Bounce S3 class for Kessler Psychological Distress Scale (K6) - US Scoring System scores
#' @rdname youthvars_k6
#' @export 

youthvars_k6 <- function(x = make_pt_youthvars_k6()){ 
validate_youthvars_k6(make_new_youthvars_k6(x))
}
#' Make new First Bounce S3 class for Kessler Psychological Distress Scale (K6) - US Scoring System scores
#' @description Create a new unvalidated instance of the First Bounce S3 class for Kessler Psychological Distress Scale (K6) - US Scoring System scores
#' @param x A prototype for the First Bounce S3 class for Kessler Psychological Distress Scale (K6) - US Scoring System scores
#' @return An unvalidated instance of the First Bounce S3 class for Kessler Psychological Distress Scale (K6) - US Scoring System scores
#' @details First Bounce S3 class for Kessler Psychological Distress Scale (K6) - US Scoring System scores
#' @rdname make_new_youthvars_k6
#' @export 

make_new_youthvars_k6 <- function(x){ 
stopifnot(is.integer(x))
class(x) <- append(c("youthvars_k6",setdiff(make_pt_youthvars_k6() %>% class(),class(x))),
class(x))
x
}
#' Make prototype First Bounce S3 class for Kessler Psychological Distress Scale (K6) - US Scoring System scores
#' @description Create a new prototype for the First Bounce S3 class for Kessler Psychological Distress Scale (K6) - US Scoring System scores

#' @return A prototype for First Bounce S3 class for Kessler Psychological Distress Scale (K6) - US Scoring System scores
#' @details First Bounce S3 class for Kessler Psychological Distress Scale (K6) - US Scoring System scores
#' @rdname make_pt_youthvars_k6
#' @export 

make_pt_youthvars_k6 <- function(){ 
integer(0)
}
#' Validate First Bounce S3 class for Kessler Psychological Distress Scale (K6) - US Scoring System scores
#' @description Validate an instance of the First Bounce S3 class for Kessler Psychological Distress Scale (K6) - US Scoring System scores
#' @param x An unvalidated instance of the First Bounce S3 class for Kessler Psychological Distress Scale (K6) - US Scoring System scores
#' @return A prototpe for First Bounce S3 class for Kessler Psychological Distress Scale (K6) - US Scoring System scores
#' @details First Bounce S3 class for Kessler Psychological Distress Scale (K6) - US Scoring System scores
#' @rdname validate_youthvars_k6
#' @export 

validate_youthvars_k6 <- function(x){
if(any(x < 0)){
stop("All values in valid youthvars_k6 object must be greater than or equal to 0.",
call. = FALSE)
}
 if(any(x > 24)){
stop("All values in valid youthvars_k6 object must be less than or equal to 24.",
call. = FALSE)
}
x}
#' Is First Bounce S3 class for Kessler Psychological Distress Scale (K6) - US Scoring System scores
#' @description Check whether an object is a valid instance of the First Bounce S3 class for Kessler Psychological Distress Scale (K6) - US Scoring System scores
#' @param x An object of any type
#' @return A logical value, TRUE if a valid instance of the First Bounce S3 class for Kessler Psychological Distress Scale (K6) - US Scoring System scores
#' @details First Bounce S3 class for Kessler Psychological Distress Scale (K6) - US Scoring System scores
#' @rdname is_youthvars_k6
#' @export 

is_youthvars_k6 <- function(x) inherits(validate_youthvars_k6(x), "youthvars_k6")
