
#' youthvars ready4 sub-module (S3 class) for Social and Occupational Functioning Assessment Scale (SOFAS)
#' @description Create a new valid instance of the youthvars ready4 sub-module (S3 class) for Social and Occupational Functioning Assessment Scale (SOFAS)
#' @param x A prototype for the youthvars ready4 sub-module (S3 class) for Social and Occupational Functioning Assessment Scale (SOFAS), Default: make_pt_youthvars_sofas()
#' @return A validated instance of the youthvars ready4 sub-module (S3 class) for Social and Occupational Functioning Assessment Scale (SOFAS)
#' @details youthvars ready4 sub-module (S3 class) for Social and Occupational Functioning Assessment Scale (SOFAS)
#' @rdname youthvars_sofas
#' @export 
youthvars_sofas <- function(x = make_pt_youthvars_sofas()){ 
validate_youthvars_sofas(make_new_youthvars_sofas(x))
}
#' make new youthvars sofas youthvars ready4 sub-module (S3 class) for Social and Occupational Functioning Assessment Scale (SOFAS)
#' @description Create a new unvalidated instance of the youthvars ready4 sub-module (S3 class) for Social and Occupational Functioning Assessment Scale (SOFAS)
#' @param x A prototype for the youthvars ready4 sub-module (S3 class) for Social and Occupational Functioning Assessment Scale (SOFAS)
#' @return An unvalidated instance of the youthvars ready4 sub-module (S3 class) for Social and Occupational Functioning Assessment Scale (SOFAS)
#' @details youthvars ready4 sub-module (S3 class) for Social and Occupational Functioning Assessment Scale (SOFAS)
#' @rdname make_new_youthvars_sofas
#' @export 
#' @keywords internal
make_new_youthvars_sofas <- function(x){ 
stopifnot(is.integer(x))
class(x) <- append(c("youthvars_sofas",setdiff(make_pt_youthvars_sofas() %>% class(),class(x))),
class(x))
x
}
#' make prototype youthvars sofas youthvars ready4 sub-module (S3 class) for Social and Occupational Functioning Assessment Scale (SOFAS)

#' @return A prototype for youthvars ready4 sub-module (S3 class) for Social and Occupational Functioning Assessment Scale (SOFAS)
#' 
#' @rdname youthvars_sofas
#' @export 
make_pt_youthvars_sofas <- function(){ 
integer(0)
}
#' validate youthvars sofas youthvars ready4 sub-module (S3 class) for Social and Occupational Functioning Assessment Scale (SOFAS)
#' @description Validate an instance of the youthvars ready4 sub-module (S3 class) for Social and Occupational Functioning Assessment Scale (SOFAS)
#' @param x An unvalidated instance of the youthvars ready4 sub-module (S3 class) for Social and Occupational Functioning Assessment Scale (SOFAS)
#' @return A prototpe for youthvars ready4 sub-module (S3 class) for Social and Occupational Functioning Assessment Scale (SOFAS)
#' @details youthvars ready4 sub-module (S3 class) for Social and Occupational Functioning Assessment Scale (SOFAS)
#' @rdname validate_youthvars_sofas
#' @export 
#' @keywords internal
validate_youthvars_sofas <- function(x){
if(any(x[!is.na(x)] < 0)){
stop("All non-missing values in valid youthvars_sofas object must be greater than or equal to 0.",
call. = FALSE)
}

 if(any(x[!is.na(x)] > 100)){
stop("All non-missing values in valid youthvars_sofas object must be less than or equal to 100.",
call. = FALSE)
}

x}
#' is youthvars sofas youthvars ready4 sub-module (S3 class) for Social and Occupational Functioning Assessment Scale (SOFAS)
#' @param x An object of any type
#' @return A logical value, TRUE if a valid instance of the youthvars ready4 sub-module (S3 class) for Social and Occupational Functioning Assessment Scale (SOFAS)
#' 
#' @rdname youthvars_sofas
#' @export 
is_youthvars_sofas <- function(x) inherits(validate_youthvars_sofas(x), "youthvars_sofas")
