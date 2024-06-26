% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fn_add.R
\name{add_uids_to_tbs_ls}
\alias{add_uids_to_tbs_ls}
\title{Add unique identifiers to tibbles list}
\usage{
add_uids_to_tbs_ls(tbs_ls, prefix_1L_chr, id_var_nm_1L_chr = "fkClientID")
}
\arguments{
\item{tbs_ls}{Tibbles (a list)}

\item{prefix_1L_chr}{Prefix (a character vector of length one)}

\item{id_var_nm_1L_chr}{Identity variable name (a character vector of length one), Default: 'fkClientID'}
}
\value{
Tibbles (a list)
}
\description{
add_uids_to_tbs_ls() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add unique identifiers to tibbles list. The function returns Tibbles (a list).
}
\keyword{internal}
