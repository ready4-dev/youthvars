% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fn_add.R
\name{add_unwtd_dim_tots}
\alias{add_unwtd_dim_tots}
\title{Add unweighted dimension totals}
\usage{
add_unwtd_dim_tots(items_tb, domain_items_ls, domain_pfx_1L_chr)
}
\arguments{
\item{items_tb}{Items (a tibble)}

\item{domain_items_ls}{Domain items (a list)}

\item{domain_pfx_1L_chr}{Domain prefix (a character vector of length one)}
}
\value{
Items and domains (a tibble)
}
\description{
add_unwtd_dim_tots() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add unweighted dimension totals. The function returns Items and domains (a tibble).
}
\keyword{internal}
