##
x <- ready4use::Ready4useRepos(gh_repo_1L_chr = "ready4-dev/ready4",
                               gh_tag_1L_chr = "Documentation_0.0")
y <- ingest(x)
prototype_lup <- y@b_Ready4useIngest@objects_ls$prototype_lup %>% tibble::add_case(type_chr = "data.frame",val_chr = "data.frame()",
                                                                  pt_ns_chr = "base", fn_to_call_chr ="",
                                                                  default_val_chr = "data.frame()",
                                                                  old_class_lgl = F) %>%
  dplyr::arrange(pt_ns_chr,type_chr)
y <- Ready4useRecord(a_Ready4usePointer = ready4use::Ready4usePointer(b_Ready4useRepos = x),
                     b_Ready4useIngest = Ready4useIngest(objects_ls = list(prototype_lup = prototype_lup)))

share(y,
      type_1L_chr = "gh")
# x_xx$x_ready4fun_manifest <- renew.ready4fun_manifest(x_xx$x_ready4fun_manifest,
#                                                         tf_to_singular_chr = c(cntrl = "cntrls",
#                                                                                corstar = "corstars",
#                                                                                dvQ = "dvQs"),
#                                                         type_1L_chr = "abbreviations",
#                                                         long_name_chr = c("birth",
#                                                                           "control",
#                                                                           "starred correlation",#,
#                                                                           "diagonal", "questionnaire dimension item",
#                                                                           "GAD-7", "K-6", "dimension scaling constant",
#                                                                           "multi-attribute utility instrument",
#                                                                           "orientation", "PHQ-9",
#                                                                           "response", "round","unweighted"))

# x_xx$x_ready4fun_manifest %>% procure.ready4fun_manifest(type_1L_chr = "problems")
# x_ready4fun_manifest$x_ready4fun_manifest <- ready4fun::update_msng_abbrs(x_ready4fun_manifest$x_ready4fun_manifest ,
#                                   are_words_chr = x_ready4fun_manifest$x_ready4fun_manifest$problems_ls$missing_obj_types_chr)
# x_ready4fun_manifest$x_ready4fun_manifest %>% write_new_words_vec() -> x_ready4fun_manifest$x_ready4fun_manifest
# x_ready4use_manifest$x_ready4fun_manifest <- x_ready4fun_manifest$x_ready4fun_manifest

# usethis::use_package("knitrBootstrap")
