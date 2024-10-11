##
library(ready4use)
X <- ready4use::Ready4useRepos(gh_repo_1L_chr = "ready4-dev/ready4",
                               gh_tag_1L_chr = "Documentation_0.0")
Y <- ingest(X)
# prototype_lup <- procure(procureSlot(y,
#                                      "b_Ready4useIngest"),
#                          "prototype_lup")
# prototype_lup <- prototype_lup %>%
#   tibble::add_case(type_chr = "data.frame",val_chr = "data.frame()",
#                    pt_ns_chr = "base",
#                    fn_to_call_chr ="",
#                    default_val_chr = "data.frame()",
#                    old_class_lgl = F) %>%
#   dplyr::arrange(pt_ns_chr,type_chr)
# y <- renewSlot(y,
#                new_val_xx = Ready4useIngest(objects_ls = list(prototype_lup = prototype_lup)),
               # slot_nm_1L_chr = "b_Ready4useIngest")
# abbreviations_lup <- Y@b_Ready4useIngest@objects_ls$abbreviations_lup %>% ready4fun::renew.ready4fun_abbreviations(filter_cdn_1L_chr = "!startsWith(short_name_chr,'youthvars')") %>% dplyr::filter(rowSums(is.na(.)) != ncol(.))
# classes_lup <- Y@b_Ready4useIngest@objects_ls$classes_lup %>% dplyr::filter(!startsWith(type_chr,"youthvars_"))
# prototype_lup <- Y@b_Ready4useIngest@objects_ls$prototype_lup %>% dplyr::filter(!startsWith(type_chr,"youthvars_"))
treat_as_words_chr <- #c(Y@b_Ready4useIngest@objects_ls$treat_as_words_chr, "descriptives") %>% sort()
c("accessors", paste0("aqol", 1:20),
  "ATSI",               "autocorrelation",    "autocorrelations",   "backend",            "betareg",            "blog",
  "bootstrap",          "boruta",             "capitalise",         "cff",                "constructor",        "covariate",          "covariates",         "cran",               "cumulatives",
  "datasets",           "datestamp",          "DCOPmiss",           "DCOPmissno",         "descriptives",       "deterministic",      "DILmiss",            "DILmissno",          "disengaged",
  "DMHmiss",            "DMHmissno",          "download",           "DPmiss",             "DPmissno",           "DRLmiss",            "DRLmissno",          "DSENmiss",           "DSENmissno",
  paste0("dvD",1:6), paste0("dvQ",1:20),
  "email",
  "Equ",                "fabels",             "fkClientID",         "halton",             "html",               "isochrone",          "isochrones",         "lifecycle",          "metadata",
  "multi",              "neuropsychological", "non",                "pdf",                "pdfs",               "pkgdown",            "png",                "polyline",           "predictors",
  paste0("q",1:20), paste0("Q",1:20),
  "R",                  "rdocx",              "rds",                "ready4",             "ready4show",
  "recode",             "removeTriangle",     "scal",               "shareable",          "standardised",       "STATA",              "summarise",          "tableby",            "timepoint",
  "tsibble",            "tsibbles",           "uaqol6Dusing8D",     "uaqol6Dusing8Da",    "ungroup",            "unscored",           "url",                "urls",               "v1",
  "validator",          "vD1",                "vD2",                "vD3",                "vD4",                "vD5",                "vD6",                "workflow",           "yaml",
  "yhat",               "zenodo"
)
#   "aqol1", "aqol10","aqol11"             "aqol12"             "aqol13"             "aqol14"             "aqol15"             "aqol16"
# "aqol17"             "aqol18"             "aqol19"             "aqol2"              "aqol20"             "aqol3"              "aqol4"              "aqol5"              "aqol6"
# "aqol7"              "aqol8"              "aqol9"
#"dvD1"               "dvD2"               "dvD3"               "dvD4"               "dvD5"               "dvD6"
# "dvQ1"               "dvQ10"              "dvQ11"
# "dvQ12"              "dvQ13"              "dvQ14"              "dvQ15"              "dvQ16"              "dvQ17"              "dvQ18"              "dvQ19"              "dvQ2"
# "dvQ20"              "dvQ3"               "dvQ4"               "dvQ5"               "dvQ6"               "dvQ7"               "dvQ8"               "dvQ9"

# "q1"                 "Q1"                 "q10"                "Q10"                "q11"                "Q11"                "q12"                "Q12"                "q13"
# "Q13"                "q14"                "Q14"                "q15"                "Q15"                "q16"                "Q16"                "q17"                "Q17"
# "q18"                "Q18"                "q19"                "Q19"                "q2"                 "Q2"                 "q20"                "Q20"                "q3"
# "Q3"                 "q4"                 "Q4"                 "q5"                 "Q5"                 "q6"                 "Q6"                 "q7"                 "Q7"
# "q8"                 "Q8"                 "q9"                 "Q9"


Y <- renewSlot(Y,
               new_val_xx = Ready4useIngest(objects_ls = list(
                 treat_as_words_chr = treat_as_words_chr
                 # abbreviations_lup = abbreviations_lup,
                 # classes_lup = classes_lup,
                 # prototype_lup = prototype_lup
                                                              )),
               slot_nm_1L_chr = "b_Ready4useIngest")
Y <- share(Y,
           type_1L_chr = "prefer_gh") # DON'T FORGET TO RECREATE NAMESPACE IF CHANGING / ADDING S3 CLASSES
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
