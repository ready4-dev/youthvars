
#' youthvars ready4 sub-module (S3 class) for Child Health Utility Nine Dimension Health Utility - Australian Adolescent Scoring (CHU-9D Australian Adolescent)
#' @description Create a new valid instance of the youthvars ready4 sub-module (S3 class) for Child Health Utility Nine Dimension Health Utility - Australian Adolescent Scoring (CHU-9D Australian Adolescent)
#' @param x A prototype for the youthvars ready4 sub-module (S3 class) for Child Health Utility Nine Dimension Health Utility - Australian Adolescent Scoring (CHU-9D Australian Adolescent), Default: make_pt_youthvars_chu9d_adolaus()
#' @return A validated instance of the youthvars ready4 sub-module (S3 class) for Child Health Utility Nine Dimension Health Utility - Australian Adolescent Scoring (CHU-9D Australian Adolescent)
#' @details youthvars ready4 sub-module (S3 class) for Child Health Utility Nine Dimension - Australian Adolescent Scoring (CHU-9D Australian Adolescent)
#' @rdname youthvars_chu9d_adolaus
#' @export 
youthvars_chu9d_adolaus <- function(x = make_pt_youthvars_chu9d_adolaus()){ 
validate_youthvars_chu9d_adolaus(make_new_youthvars_chu9d_adolaus(x))
}
#' make new youthvars chu9d adolaus youthvars ready4 sub-module (S3 class) for Child Health Utility Nine Dimension Health Utility - Australian Adolescent Scoring (CHU-9D Australian Adolescent)
#' @description Create a new unvalidated instance of the youthvars ready4 sub-module (S3 class) for Child Health Utility Nine Dimension Health Utility - Australian Adolescent Scoring (CHU-9D Australian Adolescent)
#' @param x A prototype for the youthvars ready4 sub-module (S3 class) for Child Health Utility Nine Dimension Health Utility - Australian Adolescent Scoring (CHU-9D Australian Adolescent)
#' @return An unvalidated instance of the youthvars ready4 sub-module (S3 class) for Child Health Utility Nine Dimension Health Utility - Australian Adolescent Scoring (CHU-9D Australian Adolescent)
#' @details youthvars ready4 sub-module (S3 class) for Child Health Utility Nine Dimension - Australian Adolescent Scoring (CHU-9D Australian Adolescent)
#' @rdname make_new_youthvars_chu9d_adolaus
#' @export 
#' @keywords internal
make_new_youthvars_chu9d_adolaus <- function(x){ 
stopifnot(is.numeric(x))
class(x) <- append(c("youthvars_chu9d_adolaus",setdiff(make_pt_youthvars_chu9d_adolaus() %>% class(),class(x))),
class(x))
x
}
#' make prototype youthvars chu9d adolaus youthvars ready4 sub-module (S3 class) for Child Health Utility Nine Dimension Health Utility - Australian Adolescent Scoring (CHU-9D Australian Adolescent)

#' @return A prototype for youthvars ready4 sub-module (S3 class) for Child Health Utility Nine Dimension Health Utility - Australian Adolescent Scoring (CHU-9D Australian Adolescent)
#' 
#' @rdname youthvars_chu9d_adolaus
#' @export 
make_pt_youthvars_chu9d_adolaus <- function(){ 
numeric(0)
}
#' validate youthvars chu9d adolaus youthvars ready4 sub-module (S3 class) for Child Health Utility Nine Dimension Health Utility - Australian Adolescent Scoring (CHU-9D Australian Adolescent)
#' @description Validate an instance of the youthvars ready4 sub-module (S3 class) for Child Health Utility Nine Dimension Health Utility - Australian Adolescent Scoring (CHU-9D Australian Adolescent)
#' @param x An unvalidated instance of the youthvars ready4 sub-module (S3 class) for Child Health Utility Nine Dimension Health Utility - Australian Adolescent Scoring (CHU-9D Australian Adolescent)
#' @return A prototpe for youthvars ready4 sub-module (S3 class) for Child Health Utility Nine Dimension Health Utility - Australian Adolescent Scoring (CHU-9D Australian Adolescent)
#' @details youthvars ready4 sub-module (S3 class) for Child Health Utility Nine Dimension - Australian Adolescent Scoring (CHU-9D Australian Adolescent)
#' @rdname validate_youthvars_chu9d_adolaus
#' @export 
#' @keywords internal
validate_youthvars_chu9d_adolaus <- function(x){
if(any(x[!is.na(x)] < -0.2118)){
stop("All non-missing values in valid youthvars_chu9d_adolaus object must be greater than or equal to -0.2118.",
call. = FALSE)
}

 if(any(x[!is.na(x)] > 1)){
stop("All non-missing values in valid youthvars_chu9d_adolaus object must be less than or equal to 1.",
call. = FALSE)
}

x}
#' is youthvars chu9d adolaus youthvars ready4 sub-module (S3 class) for Child Health Utility Nine Dimension Health Utility - Australian Adolescent Scoring (CHU-9D Australian Adolescent)
#' @param x An object of any type
#' @return A logical value, TRUE if a valid instance of the youthvars ready4 sub-module (S3 class) for Child Health Utility Nine Dimension Health Utility - Australian Adolescent Scoring (CHU-9D Australian Adolescent)
#' 
#' @rdname youthvars_chu9d_adolaus
#' @export 
is_youthvars_chu9d_adolaus <- function(x) inherits(validate_youthvars_chu9d_adolaus(x), "youthvars_chu9d_adolaus")
