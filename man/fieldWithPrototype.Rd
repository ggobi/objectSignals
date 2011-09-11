\name{fieldWithPrototype}
\alias{fieldWithPrototype}
\title{Fields with prototypes}
\usage{fieldWithPrototype(name, class, value)
}
\description{A convenience for declaring a default value for a field, in the
vein of \code{\link[methods]{prototype}} for S4 classes, except
the default value is quoted and evaluated lazily.}
\value{A list suitable for use with \code{\link{setRefClass}}}
\author{Michael lawrence}
\arguments{\item{name}{The name of the field}
\item{class}{The class of the field}
\item{value}{Default value that when evaluated
initializes the field}
}
\examples{Brush.gen <- setRefClass("Brush",
fields = fieldWithPrototype("color", "character", "red"))
brush <- Brush.gen$new()
brush$color
brush$color <- "blue"
brush$color}

