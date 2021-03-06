% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/print_paged_sf.R, R/save_paged_sf.R
\name{print_paged_sf}
\alias{print_paged_sf}
\alias{print_paged_sf}
\alias{print_paged_sf}
\title{Print sf objects with paged tables}
\usage{
print_paged_sf(x, n = ifelse(options("max.print")[[1]] == 99999, 10,
  options("max.print")[[1]]))

print_paged_sf(x, n = ifelse(options("max.print")[[1]] == 99999, 10,
  options("max.print")[[1]]))
}
\arguments{
\item{x}{A sf-class data object}

\item{n}{Number of records to show in the printed table}

\item{x}{A sf-class data object}

\item{n}{Number of records to show in the printed table}
}
\description{
Print sf objects with paged tables

Print sf objects with paged table.
}
