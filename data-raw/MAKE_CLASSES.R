classes_to_make_tb <- ready4class::ready4_constructor_tbl() %>%
  dplyr::bind_rows(tibble::tribble(
    ~ make_s3_lgl, ~ name_stub_chr, ~ pt_ls, ~ pt_chkr_pfx_ls, ~ pt_ns_ls, ~ vals_ls, ~ allowed_vals_ls, ~ min_max_vals_ls, ~ start_end_vals_ls, ~ class_desc_chr, ~ parent_class_chr, ~ slots_ls, ~ meaningful_nms_ls, ~ inc_clss_ls, ~ asserts_ls,
    TRUE, "aqol6d_adol", list("numeric"), list("is."),list("base"), NULL, NULL,list(c(0.03, 1)), NULL, "First Bounce S3 class for Assessment of Quality of Life Six Dimension Health Utility - Adolescent Version (AQoL6d Adolescent))", NA_character_, NULL, NULL, NULL, NULL,
    TRUE, "phq9", list("integer"), list("is."),list("base"), NULL, NULL,list(c(0, 27)), NULL, "First Bounce S3 class for Patient Health Questionnaire (PHQ-9) scores", NA_character_, NULL, NULL, NULL, NULL,
    TRUE, "bads", list("integer"), list("is."),list("base"), NULL, NULL,list(c(0, 150)), NULL, "First Bounce S3 class for Behavioural Activation for Depression Scale (BADS) scores", NA_character_, NULL, NULL, NULL, NULL,
    TRUE, "gad7", list("integer"), list("is."),list("base"), NULL, NULL,list(c(0, 21)), NULL, "First Bounce S3 class for Generalised Anxiety Disorder Scale (GAD-7) scores", NA_character_, NULL, NULL, NULL, NULL,
    TRUE, "oasis", list("integer"), list("is."),list("base"), NULL, NULL,list(c(0, 20)), NULL, "First Bounce S3 class for Overall Anxiety Severity and Impairment Scale (OASIS) scores", NA_character_, NULL, NULL, NULL, NULL,
    TRUE, "scared", list("integer"), list("is."),list("base"),NULL, NULL,list(c(0, 82)), NULL, "First Bounce S3 class for Screen for Child Anxiety Related Disorders (SCARED) scores", NA_character_, NULL, NULL, NULL, NULL,
    TRUE, "k6", list("integer"), list("is."),list("base"), NULL, NULL,list(c(0, 24)), NULL, "First Bounce S3 class for Kessler Psychological Distress Scale (K6) - US Scoring System scores", NA_character_, NULL, NULL, NULL, NULL,
    TRUE, "sofas", list("integer"), list("is."),list("base"), NULL, NULL,list(c(0, 100)), NULL, "First Bounce S3 class for Social and Occupational Functioning Assessment Scale (SOFAS)", NA_character_, NULL, NULL, NULL, NULL)
  )
name_pfx_1L_chr <- "youthvars_"
