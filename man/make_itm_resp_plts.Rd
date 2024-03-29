% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fn_make.R
\name{make_itm_resp_plts}
\alias{make_itm_resp_plts}
\title{Make item response plots}
\usage{
make_itm_resp_plts(
  data_tb,
  col_nms_chr,
  lbl_nms_chr,
  plot_rows_cols_pair_int,
  heights_int,
  round_var_nm_1L_chr = "round",
  y_label_1L_chr = "Percentage"
)
}
\arguments{
\item{data_tb}{Data (a tibble)}

\item{col_nms_chr}{Column names (a character vector)}

\item{lbl_nms_chr}{Label names (a character vector)}

\item{plot_rows_cols_pair_int}{Plot rows columns pair (an integer vector)}

\item{heights_int}{Heights (an integer vector)}

\item{round_var_nm_1L_chr}{Round variable name (a character vector of length one), Default: 'round'}

\item{y_label_1L_chr}{Y label (a character vector of length one), Default: 'Percentage'}
}
\value{
Composite (a plot)
}
\description{
make_itm_resp_plts() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make item response plots. The function returns Composite (a plot).
}
