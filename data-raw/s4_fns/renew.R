renew_YouthvarsDescriptives <- function(x, # renewSlot
                                        type_1L_chr = "characterize",
                                        ...){
  if(type_1L_chr == "characterize")
    x@descriptives_df <- characterize(x,
                                      ...)
  return(x)
}
renew_YouthvarsProfile <- function(x,
                                   nbr_of_digits_1L_int = 3L,
                                   profile_chr = character(0),
                                   type_1L_chr = "characterize",
                                   what_1L_chr = "descriptives_ls",
                                   ...){
  if(what_1L_chr == "descriptives_ls"){
    if(type_1L_chr == "manufacture" | (type_1L_chr == "characterize" && identical(x@descriptives_ls, list(list())))){
      x@descriptives_ls <- manufacture(x,
                                       nbr_of_digits_1L_int = nbr_of_digits_1L_int,
                                       profile_chr = profile_chr,
                                       what_1L_chr = what_1L_chr,
                                       ...)
    }
    if(type_1L_chr == "characterize")
      x@descriptives_ls <- purrr::map(x@descriptives_ls,
                                      ~ ready4::renew(.x,
                                                      y_Ready4useDyad = x@a_Ready4useDyad))

  }else{
   x <- methods::callNextMethod()
  }

  return(x)
}
