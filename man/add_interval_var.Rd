% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fn_add.R
\name{add_interval_var}
\alias{add_interval_var}
\title{Add interval variable}
\usage{
add_interval_var(
  data_tb,
  id_var_nm_1L_chr = "fkClientID",
  msrmnt_date_var_nm_1L_chr = "d_interview_date",
  time_unit_1L_chr = "days",
  bl_date_var_nm_1L_chr = "bl_date_dtm",
  interval_var_nm_1L_chr = "interval_dbl",
  tmp_row_nbr_var_nm_1L_chr = "tmp_row_nbr_int",
  drop_bl_date_var_1L_lgl = F
)
}
\arguments{
\item{data_tb}{Data (a tibble)}

\item{id_var_nm_1L_chr}{Identity variable name (a character vector of length one), Default: 'fkClientID'}

\item{msrmnt_date_var_nm_1L_chr}{Measurement date variable name (a character vector of length one), Default: 'd_interview_date'}

\item{time_unit_1L_chr}{Time unit (a character vector of length one), Default: 'days'}

\item{bl_date_var_nm_1L_chr}{Baseline date variable name (a character vector of length one), Default: 'bl_date_dtm'}

\item{interval_var_nm_1L_chr}{Interval variable name (a character vector of length one), Default: 'interval_dbl'}

\item{tmp_row_nbr_var_nm_1L_chr}{Temporary row number variable name (a character vector of length one), Default: 'tmp_row_nbr_int'}

\item{drop_bl_date_var_1L_lgl}{Drop baseline date variable (a logical vector of length one), Default: F}
}
\value{
Updated data (a tibble)
}
\description{
add_interval_var() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add interval variable. The function returns Updated data (a tibble).
}
