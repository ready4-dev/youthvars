library(magrittr)
ready4fun::read_fns("data-raw/fns")
source("data-raw/MAKE_CLASSES.R")
object_type_lup <- ready4fun::get_rds_from_dv("object_type_lup")
abbreviations_lup <- ready4fun::get_rds_from_dv("abbreviations_lup")
fn_type_lup_tb <- ready4fun::get_rds_from_dv("fn_type_lup_tb")
# Edits go here

# Push updates to dataverse
abbreviations_lup %>%
  ready4use::write_paired_ds_fls_to_dv(fl_nm_1L_chr = "abbreviations_lup",
                            desc_1L_chr = "Abbreviations lookup table")
fn_type_lup_tb %>%
  ready4use::write_paired_ds_fls_to_dv(fl_nm_1L_chr = "fn_type_lup_tb",
                            desc_1L_chr = "Function type lookup table")
# NOTE: Don't forget to review and publish the updated dataset.
# Previous edits
# abbreviations_lup <- abbreviations_lup %>%
#   ready4fun::update_abbr_lup(short_name_chr = c(paste0(name_pfx_1L_chr,classes_to_make_tb$name_stub_chr),
#                                                 "adol","aqol","aqol6d","aqol6dU",
#                                                 "dim","disv","eq","lev",
#                                                 "q",
#                                                 "scrg",
#                                                 "unscrd","vldn"),
#                              long_name_chr = c(classes_to_make_tb$class_desc_chr,
#                                                "adolescent",
#                                                "Assessment of Quality of Life",
#                                                "Assessment of Quality of Life Six Dimension",
#                                                "Assessment of Quality of Life Six Dimension Health Utility",
#                                                "dimension",
#                                                "disvalue",
#                                                "equation",
#                                                "level",
#                                                "question",
#                                                "scoring",
#                                                "unscored",
#                                                "validation"),
#                              no_plural_chr = c("Assessment of Quality of Life",
#                                                "Assessment of Quality of Life Six Dimension",
#                                                "Assessment of Quality of Life Six Dimension Health Utility",
#                                                "validation"))
# abbreviations_lup <- abbreviations_lup %>%
#   ready4fun::update_abbr_lup(short_name_chr = c("bw","descv","scl","stat"),
#                              long_name_chr = c("black and white","descriptive","scale","statistic"))
# fn_type_lup_tb <- fn_type_lup_tb %>%
#   ready4fun::add_rows_to_fn_type_lup(fn_type_nm_chr = ready4fun::get_new_fn_types(abbreviations_lup = abbreviations_lup,
#                                                                                   fn_type_lup_tb = fn_type_lup_tb,
#                                                                                   object_type_lup = object_type_lup),
#                                      fn_type_desc_chr = c("Performs a numeric calculation.","Imputes data."),
#                                      is_generic_lgl = F,
#                                      is_method_lgl = F)

