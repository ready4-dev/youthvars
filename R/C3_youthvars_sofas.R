
#' First Bounce S3 class for Social and Occupational Functioning Assessment Scale (SOFAS)
#' @description Create a new valid instance of the First Bounce S3 class for Social and Occupational Functioning Assessment Scale (SOFAS)
#' @param x A prototype for the First Bounce S3 class for Social and Occupational Functioning Assessment Scale (SOFAS), Default: make_pt_youthvars_sofas()
#' @return A validated instance of the First Bounce S3 class for Social and Occupational Functioning Assessment Scale (SOFAS)
#' @details First Bounce S3 class for Social and Occupational Functioning Assessment Scale (SOFAS)
#' @rdname youthvars_sofas
#' @export 

youthvars_sofas <- function(x = make_pt_youthvars_sofas()){ 
validate_youthvars_sofas(make_new_youthvars_sofas(x))
}
#' Make new First Bounce S3 class for Social and Occupational Functioning Assessment Scale (SOFAS)
#' @description Create a new unvalidated instance of the First Bounce S3 class for Social and Occupational Functioning Assessment Scale (SOFAS)
#' @param x A prototype for the First Bounce S3 class for Social and Occupational Functioning Assessment Scale (SOFAS)
#' @return An unvalidated instance of the First Bounce S3 class for Social and Occupational Functioning Assessment Scale (SOFAS)
#' @details First Bounce S3 class for Social and Occupational Functioning Assessment Scale (SOFAS)
#' @rdname make_new_youthvars_sofas
#' @export 

make_new_youthvars_sofas <- function(x){ 
stopifnot(is.integer(x))
class(x) <- append(c("youthvars_sofas",setdiff(make_pt_youthvars_sofas() %>% class(),class(x))),
class(x))
x
}
#' Make prototype First Bounce S3 class for Social and Occupational Functioning Assessment Scale (SOFAS)
#' @description Create a new prototype for the First Bounce S3 class for Social and Occupational Functioning Assessment Scale (SOFAS)

#' @return A prototype for First Bounce S3 class for Social and Occupational Functioning Assessment Scale (SOFAS)
#' @details First Bounce S3 class for Social and Occupational Functioning Assessment Scale (SOFAS)
#' @rdname make_pt_youthvars_sofas
#' @export 

make_pt_youthvars_sofas <- function(){ 
integer(0)
}
#' Validate First Bounce S3 class for Social and Occupational Functioning Assessment Scale (SOFAS)
#' @description Validate an instance of the First Bounce S3 class for Social and Occupational Functioning Assessment Scale (SOFAS)
#' @param x An unvalidated instance of the First Bounce S3 class for Social and Occupational Functioning Assessment Scale (SOFAS)
#' @return A prototpe for First Bounce S3 class for Social and Occupational Functioning Assessment Scale (SOFAS)
#' @details First Bounce S3 class for Social and Occupational Functioning Assessment Scale (SOFAS)
#' @rdname validate_youthvars_sofas
#' @export 

validate_youthvars_sofas <- function(x){
if(any(x < 0)){
stop("All values in valid youthvars_sofas object must be greater than or equal to 0.",
call. = FALSE)
}
 if(any(x > 100)){
stop("All values in valid youthvars_sofas object must be less than or equal to 100.",
call. = FALSE)
}
x}
#' Is First Bounce S3 class for Social and Occupational Functioning Assessment Scale (SOFAS)
#' @description Check whether an object is a valid instance of the First Bounce S3 class for Social and Occupational Functioning Assessment Scale (SOFAS)
#' @param x An object of any type
#' @return A logical value, TRUE if a valid instance of the First Bounce S3 class for Social and Occupational Functioning Assessment Scale (SOFAS)
#' @details First Bounce S3 class for Social and Occupational Functioning Assessment Scale (SOFAS)
#' @rdname is_youthvars_sofas
#' @export 

is_youthvars_sofas <- function(x) inherits(validate_youthvars_sofas(x), "youthvars_sofas")
