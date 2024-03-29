
#' youthvars ready4 sub-module (S3 class) for Patient Health Questionnaire (PHQ-9) scores
#' @description Create a new valid instance of the youthvars ready4 sub-module (S3 class) for Patient Health Questionnaire (PHQ-9) scores
#' @param x A prototype for the youthvars ready4 sub-module (S3 class) for Patient Health Questionnaire (PHQ-9) scores, Default: make_pt_youthvars_phq9()
#' @return A validated instance of the youthvars ready4 sub-module (S3 class) for Patient Health Questionnaire (PHQ-9) scores
#' @details youthvars ready4 sub-module (S3 class) for Patient Health Questionnaire (PHQ-9) scores
#' @rdname youthvars_phq9
#' @export 
youthvars_phq9 <- function(x = make_pt_youthvars_phq9()){ 
validate_youthvars_phq9(make_new_youthvars_phq9(x))
}
#' make new youthvars PHQ-9 youthvars ready4 sub-module (S3 class) for Patient Health Questionnaire (PHQ-9) scores
#' @description Create a new unvalidated instance of the youthvars ready4 sub-module (S3 class) for Patient Health Questionnaire (PHQ-9) scores
#' @param x A prototype for the youthvars ready4 sub-module (S3 class) for Patient Health Questionnaire (PHQ-9) scores
#' @return An unvalidated instance of the youthvars ready4 sub-module (S3 class) for Patient Health Questionnaire (PHQ-9) scores
#' @details youthvars ready4 sub-module (S3 class) for Patient Health Questionnaire (PHQ-9) scores
#' @rdname make_new_youthvars_phq9
#' @export 
#' @keywords internal
make_new_youthvars_phq9 <- function(x){ 
stopifnot(is.integer(x))
class(x) <- append(c("youthvars_phq9",setdiff(make_pt_youthvars_phq9() %>% class(),class(x))),
class(x))
x
}
#' make prototype youthvars PHQ-9 youthvars ready4 sub-module (S3 class) for Patient Health Questionnaire (PHQ-9) scores

#' @return A prototype for youthvars ready4 sub-module (S3 class) for Patient Health Questionnaire (PHQ-9) scores
#' 
#' @rdname youthvars_phq9
#' @export 
make_pt_youthvars_phq9 <- function(){ 
integer(0)
}
#' validate youthvars PHQ-9 youthvars ready4 sub-module (S3 class) for Patient Health Questionnaire (PHQ-9) scores
#' @description Validate an instance of the youthvars ready4 sub-module (S3 class) for Patient Health Questionnaire (PHQ-9) scores
#' @param x An unvalidated instance of the youthvars ready4 sub-module (S3 class) for Patient Health Questionnaire (PHQ-9) scores
#' @return A prototpe for youthvars ready4 sub-module (S3 class) for Patient Health Questionnaire (PHQ-9) scores
#' @details youthvars ready4 sub-module (S3 class) for Patient Health Questionnaire (PHQ-9) scores
#' @rdname validate_youthvars_phq9
#' @export 
#' @keywords internal
validate_youthvars_phq9 <- function(x){
if(any(x[!is.na(x)] < 0)){
stop("All non-missing values in valid youthvars_phq9 object must be greater than or equal to 0.",
call. = FALSE)
}

 if(any(x[!is.na(x)] > 27)){
stop("All non-missing values in valid youthvars_phq9 object must be less than or equal to 27.",
call. = FALSE)
}

x}
#' is youthvars PHQ-9 youthvars ready4 sub-module (S3 class) for Patient Health Questionnaire (PHQ-9) scores
#' @param x An object of any type
#' @return A logical value, TRUE if a valid instance of the youthvars ready4 sub-module (S3 class) for Patient Health Questionnaire (PHQ-9) scores
#' 
#' @rdname youthvars_phq9
#' @export 
is_youthvars_phq9 <- function(x) inherits(validate_youthvars_phq9(x), "youthvars_phq9")
