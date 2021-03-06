% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vega_refs_is.R
\name{is.vega_input}
\alias{is.vega_input}
\title{Validate if the object is a vega input object}
\usage{
is.vega_input(obj, error = FALSE)
}
\arguments{
\item{obj}{an input object}

\item{error}{Throw an error on parse failure? If TRUE, then the function returns NULL
on success (i.e., call only for the side-effect of an error on failure, like
stopifnot).}
}
\value{
logical
}
\description{
Validate if the object is a vega input object
}
