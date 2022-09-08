library(ready4fun)
library(ready4show)
library(ready4use)
fns_env_ls <- ready4fun::read_fns(c("data-raw/fns/","data-raw/mthds/"),
                                  fns_env = new.env(parent = globalenv()))
x <- ready4fun::make_pkg_desc_ls(pkg_title_1L_chr = "Describe And Validate Youth Mental Health Datasets",
                                 pkg_desc_1L_chr = "Tools to describe and quality assure types of data commonly present in youth mental health collections.
                            The main motivation for this package is to facilitate automated data integrity checks, ensure that methods are applied to the appropriate data structures and streamline reporting of descriptive statistics.
  This development version of the youthvars package has been made available as part of the process of testing and documenting the package.
                            If you have any questions, please contact the authors (matthew.hamilton@orygen.org.au).",
                                 authors_prsn = c(utils::person(given = "Matthew",family = "Hamilton",email = "matthew.hamilton@orygen.org.au", role = c("aut", "cre"),comment = c(ORCID = "0000-0001-7407-9194")),
                                                  utils::person(given = "Caroline",family = "Gao",email = "caroline.gao@orygen.org.au", role = c("aut"),comment = c(ORCID = "0000-0002-0987-2759")),
                                                  utils::person("Orygen", role = c("cph", "fnd")),
                                                  utils::person("Headspace", role = c( "fnd")),
                                                  utils::person("National Health and Medical Research Council", role = c( "fnd"))),
                                 urls_chr = c("https://ready4-dev.github.io/youthvars/",
                                              "https://github.com/ready4-dev/youthvars",
                                              "https://www.ready4-dev.com/")) %>%
  ready4fun::make_manifest(addl_pkgs_ls = ready4fun::make_addl_pkgs_ls(#depends_chr = "ready4",
                                                                       suggests_chr = c("rmarkdown"),
                                                                       imports_chr = c("car","knitrBootstrap")),#,"car", "caret", 'lme4','quantreg',"recipes"
                           build_ignore_ls = ready4fun::make_build_ignore_ls(file_nms_chr = c("initial_setup.R")),
                           check_type_1L_chr = "ready4",
                           copyright_holders_chr = "Orygen",
                           custom_dmt_ls = ready4fun::make_custom_dmt_ls(user_manual_fns_chr = c(#"add_adol6d_scores",
                                                                                                 "add_interval_var",
                                                                                                 "add_participation_var",
                                                                                                 "assert_ds_is_valid",
                                                                                                 "make_corstars_tbl_xx",
                                                                                                 "make_descv_stats_tbl",
                                                                                                 "make_final_repln_ds_dict",
                                                                                                 "make_formula",
                                                                                                 "make_itm_resp_plts",
                                                                                                 "make_sub_tot_plts",
                                                                                                 "make_tfd_repln_ds_dict_r3",
                                                                                                 "make_var_by_round_plt",
                                                                                                 "print_descv_stats_tbl",
                                                                                                 "transform_ds_for_tstng",
                                                                                                 "transform_raw_ds_for_analysis",
                                                                                                 #"write_all_outp_dirs",
                                                                                                 "write_desv_plots",
                                                                                                 "write_descv_tbls")),##
                           dev_pkgs_chr = c("ready4","ready4use","ready4show"),
                           lifecycle_stage_1L_chr = "experimental",
                           path_to_pkg_logo_1L_chr = "../../../../../Documentation/Images/youthvars-logo/default.png",
                           piggyback_to_1L_chr = "ready4-dev/ready4",
                           ready4_type_1L_chr = "description",
                           zenodo_badge_1L_chr = "[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.5646550.svg)](https://doi.org/10.5281/zenodo.5646550)")
y <- ready4class::ready4class_constructor() %>%
  dplyr::bind_rows(tibble::tribble(
    ~ make_s3_lgl, ~ name_stub_chr, ~ pt_ls, ~ pt_chkr_pfx_ls, ~ pt_ns_ls, ~ vals_ls, ~ allowed_vals_ls, ~ min_max_vals_ls, ~ start_end_vals_ls, ~ class_desc_chr, ~ parent_class_chr, ~ slots_ls, ~ meaningful_nms_ls, ~ inc_clss_ls, ~ asserts_ls,
    TRUE, "aqol6d_adol", list("numeric"), list("is."),list("base"), NULL, NULL,list(c(0.03, 1)), NULL, "youthvars S3 class for Assessment of Quality of Life Six Dimension Health Utility - Adolescent Version (AQoL6d Adolescent))", NA_character_, NULL, NULL, NULL, NULL,
    TRUE, "phq9", list("integer"), list("is."),list("base"), NULL, NULL,list(c(0, 27)), NULL, "youthvars S3 class for Patient Health Questionnaire (PHQ-9) scores", NA_character_, NULL, NULL, NULL, NULL,
    TRUE, "bads", list("integer"), list("is."),list("base"), NULL, NULL,list(c(0, 150)), NULL, "youthvars S3 class for Behavioural Activation for Depression Scale (BADS) scores", NA_character_, NULL, NULL, NULL, NULL,
    TRUE, "gad7", list("integer"), list("is."),list("base"), NULL, NULL,list(c(0, 21)), NULL, "youthvars S3 class for Generalised Anxiety Disorder Scale (GAD-7) scores", NA_character_, NULL, NULL, NULL, NULL,
    TRUE, "oasis", list("integer"), list("is."),list("base"), NULL, NULL,list(c(0, 20)), NULL, "youthvars S3 class for Overall Anxiety Severity and Impairment Scale (OASIS) scores", NA_character_, NULL, NULL, NULL, NULL,
    TRUE, "scared", list("integer"), list("is."),list("base"),NULL, NULL,list(c(0, 82)), NULL, "youthvars S3 class for Screen for Child Anxiety Related Disorders (SCARED) scores", NA_character_, NULL, NULL, NULL, NULL,
    TRUE, "k6", list("integer"), list("is."),list("base"), NULL, NULL,list(c(0, 24)), NULL, "youthvars S3 class for Kessler Psychological Distress Scale (K6) - US Scoring System scores", NA_character_, NULL, NULL, NULL, NULL,
    TRUE, "sofas", list("integer"), list("is."),list("base"), NULL, NULL,list(c(0, 100)), NULL, "youthvars S3 class for Social and Occupational Functioning Assessment Scale (SOFAS)", NA_character_, NULL, NULL, NULL, NULL))
y <- dplyr::bind_rows(y,
                      ready4class::make_pt_ready4class_constructor(make_s3_lgl = FALSE,
                                                                   name_stub_chr = "Descriptives",
                                                                   slots_ls = list("descriptives_df",
                                                                                   "ds_tfmn_ls",
                                                                                   "key_var_nm_1L_chr",
                                                                                   "key_var_vals_chr",
                                                                                   "nbr_of_digits_1L_int",
                                                                                   "profiled_vars_chr",
                                                                                   "sections_as_row_1L_lgl",
                                                                                   "test_1L_lgl"
                                                                   ) %>% list(),
                                                                   pt_ls = list("data.frame",
                                                                                "list",
                                                                                "character",
                                                                                "character",
                                                                                "integer",
                                                                                "character",
                                                                                "logical",
                                                                                "logical") %>% list(),
                                                                   class_desc_chr= "Metadata about descriptive statistics to be generated.",
                                                                   parent_class_chr = "Ready4Module"),
                      ready4class::make_pt_ready4class_constructor(make_s3_lgl = FALSE,
                                                                   name_stub_chr = "Profile",
                                                                   slots_ls = list("a_Ready4useDyad",
                                                                                   "descriptives_ls",
                                                                                   "id_var_nm_1L_chr") %>% list(), # Change
                                                                   pt_ls = list("Ready4useDyad","list",
                                                                                "character") %>% list(),
                                                                   class_desc_chr = "A dataset and its associated dictionary, descriptive statistics and metadata.",
                                                                   parent_class_chr = "Ready4Module"),
                      ready4class::make_pt_ready4class_constructor(make_s3_lgl = FALSE,
                                                                   name_stub_chr = "Series",
                                                                   slots_ls = list("participation_var_1L_chr",
                                                                                   "timepoint_vals_chr",
                                                                                   "timepoint_var_nm_1L_chr") %>% list(), # Change
                                                                   pt_ls = list("character","character",
                                                                                "character") %>% list(),
                                                                   class_desc_chr = "A longitudinal dataset and its associated dictionary, descriptive statistics and metadata.",
                                                                   parent_class_chr = "YouthvarsProfile"))



##
##
replication_popl_tb <- read.csv("data-raw/csvs/fake_pop_tb.csv") %>%
  dplyr::mutate(c_sofas = as.integer(round(c_sofas,0))) %>%
  dplyr::mutate(round = factor(round, labels = c("Baseline",
                                                 "Follow-up"))) %>%
  dplyr::mutate(d_relation_s = dplyr::case_when(d_relation_s %in% c("REPLACE_ME_1","REPLACE_ME_2") ~ "Not in a relationship",
                                                T ~ "In a relationship")) %>%
  youthu::add_dates_from_dstr(bl_start_date_dtm = Sys.Date() - lubridate::days(600),##
                              bl_end_date_dtm = Sys.Date() - lubridate::days(420),
                              duration_args_ls = list(a = 60, b = 140, mean = 90, sd = 10),
                              duration_fn = truncnorm::rtruncnorm,
                              date_var_nm_1L_chr = "d_interview_date") %>%
  dplyr::select(-duration_prd) %>%
  fns_env_ls$fns_env$transform_raw_ds_for_analysis() %>%
  dplyr::rename(phq9_total = PHQ9,
                bads_total = BADS,
                gad7_total = GAD7,
                oasis_total = OASIS,
                scared_total = SCARED,
                k6_total = K6,
                c_sofas = SOFAS)
scored_data_tb <- fns_env_ls$fns_env$add_adol6d_scores(replication_popl_tb,
                                                       prefix_1L_chr = "aqol6d_q",
                                                       id_var_nm_1L_chr = "fkClientID",
                                                       wtd_aqol_var_nm_1L_chr = "aqol6d_total_w",
                                                       total_aqol_var_nm_1L_chr = "aqol6d_total_c")
Hmisc::label(scored_data_tb[["aqol6d_total_c"]]) <- "AQOL-6D (unweighted total)"
Hmisc::label(scored_data_tb[["aqol6d_total_w"]]) <- "AQOL-6D (weighted total)"
dictionary_tb <- ready4use::make_pt_ready4use_dictionary(var_nm_chr = names(scored_data_tb),
                                                         var_ctg_chr = c("identifier","temporal","temporal",
                                                                         rep("demographic",14),
                                                                         "service provider",
                                                                         rep("clinical symptom",2),
                                                                         c("psychological distress",
                                                                           rep("depression",2),
                                                                           rep("anxiety",3)),
                                                                         "functioning",
                                                                         #rep("Demographic",3),
                                                                         rep("multi-attribute utility instrument question",20),
                                                                         rep("utility item disvalue",20),
                                                                         rep("utility dimension disvalue",6),
                                                                         rep("utility dimension score (adult)",6),
                                                                         "utility overall score (disvalue scale)",
                                                                         "utility overall score (life-death scale)",
                                                                         rep("utility overall score (adolescent disutility scale)",2), # Includes Testing Duplicate
                                                                         "utility overall score (instrument)",
                                                                         "utility overall score (instrument - rotated)",
                                                                         "utility overall score (final weighted)",
                                                                         "multi-attribute utility instrument unweighted total score"
                                                         ),
                                                         var_desc_chr = c("unique client identifier",
                                                                          "round of data collection",
                                                                          "date of data collection",
                                                                          "age",
                                                                          "age Group",
                                                                          "gender (grouped)",
                                                                          "gender",
                                                                          "sex at birth",
                                                                          "sexual orientation",
                                                                          "Aboriginal or Torres Strait Islander",
                                                                          "Culturally And Linguistically Diverse",
                                                                          "country Of birth",
                                                                          "speaks English at home",
                                                                          "native English speaker",
                                                                          "region of residence (metropolitan or regional)",
                                                                          "education and employment status",
                                                                          "relationship status",
                                                                          "service centre name",
                                                                          "primary diagnosis",
                                                                          "clinical stage",
                                                                          "Kessler Psychological Distress Scale (6 Dimension)",
                                                                          "Patient Health Questionnaire",
                                                                          "Behavioural Activation for Depression Scale",
                                                                          "Generalised Anxiety Disorder Scale",
                                                                          "Overall Anxiety Severity and Impairment Scale",
                                                                          "Screen for Child Anxiety Related Disorders",
                                                                          "Social and Occupational Functioning Assessment Scale",
                                                                          paste0("Assessment of Quality of Life (6 Dimension) question ",1:20),
                                                                          paste0("Assessment of Quality of Life (6 Dimension) item disvalue",1:20),
                                                                          lapply(scored_data_tb, Hmisc::label) %>% purrr::flatten_chr() %>% purrr::keep(c(rep(F,67),rep(T,20)))
                                                         ),
                                                         var_type_chr = names(scored_data_tb) %>% purrr::map_chr(~{
                                                           class_chr <- class(scored_data_tb %>% dplyr::pull(.x))
                                                           class_chr[class_chr!="labelled"][1]
                                                         })) %>% ready4use::ready4use_dictionary()
Hmisc::label(dictionary_tb) = as.list(c("Variable","Category", "Description", "Class"))
dictionary_tb <- dictionary_tb %>%
  dplyr::arrange(var_ctg_chr,var_nm_chr)
datasets_ls <- list(
dictionary_tb %>%
  dplyr::filter(var_nm_chr %in% names(replication_popl_tb)) %>%
  ready4fun::make_pkg_ds_ls(db_1L_chr = "repln_ds_dict_r3",
                            title_1L_chr = "Data dictionary for study population dataset",
                            desc_1L_chr = "A data dictionary of the variables used in the source and replication (synthetic) datasets for the First Bounce transfer to utility study"),
dictionary_tb %>%
  dplyr::filter((!var_nm_chr %in% names(replication_popl_tb)) | startsWith(var_nm_chr, "aqol")) %>%
  ready4fun::make_pkg_ds_ls(db_1L_chr = "aqol_scrg_dict_r3",
                            title_1L_chr = "Data dictionary for AQoL scoring",
                            desc_1L_chr = "A data dictionary of the variables used in scoring AQoL 6D utility questionnaire responses."),
replication_popl_tb %>%
  dplyr::select(-c(d_agegroup,Gender, CALD, Region)) %>%
  #add_labels_from_dictionary(dictionary_tb = dictionary_tb) %>%
  ready4fun::make_pkg_ds_ls(db_1L_chr = "replication_popl_tb",
                            title_1L_chr = "Synthetic population replication dataset",
                            desc_1L_chr = "A purely synthetic dataset, representative of the original study data, that can be used for replication runs of package algorithms.",
                            abbreviations_lup = tibble::tibble(short_name_chr = dictionary_tb$var_nm_chr,
                                                               long_name_chr = dictionary_tb$var_desc_chr,
                                                               plural_lgl = F),
                            simple_lup_1L_lgl = T),
tibble::tibble(short_name_chr = c("BADS","GAD7","K6","OASIS","PHQ9","SCARED","SOFAS"),
               long_name_chr = short_name_chr %>% purrr::map_chr(~paste0(.x, " total score")),
               min_val_dbl = rep(0,7),
               max_val_dbl = c(150,21,24,20,27,82,100),
               class_chr = "integer",
               increment_dbl = rep(1,7),
               class_fn_chr = paste0("youthvars::youthvars_",tolower(short_name_chr)),
               mdl_scaling_dbl = 0.01,
               covariate_lgl = c(rep(F,6),T)) %>%
  ready4fun::make_pkg_ds_ls(db_1L_chr = "predictors_lup",
                            title_1L_chr = "Predictors lookup table",
                            desc_1L_chr = "A lookup table of the short name and long name of each predictor used in the models included with the youthu package.")

)
z <- ready4pack::make_pt_ready4pack_manifest(x,
                                             constructor_r3 = y,
                                             pkg_ds_ls_ls = datasets_ls) %>%
  ready4pack::ready4pack_manifest()
z <- ready4::author(z)
devtools::build_vignettes()
