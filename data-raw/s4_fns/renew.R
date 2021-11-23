renew_YouthvarsDescriptives <- function(x, # renewSlot
                                        type_1L_chr = "characterize",
                                        ...){
  if(type_1L_chr == "characterize")
    x@descriptives_df <- characterize(x,
                                      ...)
  return(x)
}
renew_YouthvarsProfile <- function(x,
                                   type_1L_chr = "characterize"){
  if(type_1L_chr == "characterize")
    x@descriptives_ls <- purrr::map(x@descriptives_ls,
                                    ~ready4::renew(.x,
                                                   y_Ready4useDyad = x@a_Ready4useDyad))
  return(x)
}
