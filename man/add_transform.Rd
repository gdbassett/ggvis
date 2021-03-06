% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_ref.R
\name{add_transform}
\alias{add_transform}
\title{Add a vega transform to a vega vis}
\usage{
add_transform(vis, name, type, ...)
}
\arguments{
\item{vis}{a ggvis object}

\item{name}{string a data or mark object name}

\item{type}{string type of transform to add}

\item{...}{arguments to transforms}
}
\value{
a ggvis object
}
\description{
The transform is added to the first data then group mark found matching 'name'.  If none are found, no transform is added.
}
