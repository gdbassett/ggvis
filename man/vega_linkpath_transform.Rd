% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vega_refs_transform_linkpath.R
\name{vega_linkpath_transform}
\alias{vega_linkpath_transform}
\title{create a vega linkpath transform object}
\usage{
vega_linkpath_transform(type, sourceX = NULL, sourceY = NULL,
  targetX = NULL, targetY = NULL, orient = NULL, shape = NULL,
  as = NULL)
}
\arguments{
\item{type}{All transforms require a type property, specifying the name of the transform.}

\item{sourceX}{Field The data field for the source x-coordinate. The default is `source.x`.}

\item{sourceY}{Field The data field for the source y-coordinate. The default is `source.y`.}

\item{targetX}{Field The data field for the target x-coordinate. The default is `target.x`.}

\item{targetY}{Field The data field for the target y-coordinate. The default is `target.y`.}

\item{orient}{String The orientation of the link path. One of `vertical` (default), `horizontal` or `radial`. If a `radial` orientation is specified, x and y coordinate parameters will instead be interpreted as an angle (in radians) and radius, respectively.}

\item{shape}{String The shape of the link path. One of `line` (default), `arc`, `curve`, `diagonal`, or `orthogonal`.}

\item{as}{String The output field for the link path. The default is `"path"`.}
}
\value{
a transform object
}
\description{
https://vega.github.io/vega/docs/transforms/linkpath/index.html
NOTE: Some parameters are required to be 'arrays'.  In the case where a parameter must be an array but is a single line, wrap it in 'I()', (asis), to ensure it is properly handled.
}
\details{
The **linkpath** transform is used to route a visual link between two nodes. The most common use case is to draw edges in a tree or network layout. By default links are simply straight lines between source and target nodes; however, with additional shape and orientation information, a variety of link paths can be expressed. This transform writes one property to each datum, providing an [SVG path string](https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial/Paths) for the link path.
}
