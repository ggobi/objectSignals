\name{lazyField}
\alias{lazyField}
\title{Lazily initialize the fileds}
\usage{lazyField(name, class, expr)}
\description{Declares a lazily initialized field, with the class and initializer.}
\value{A list suitable for use with \code{\link{setRefClass}}}
\author{Michael lawrence}
\arguments{\item{name}{The name of the field}
\item{class}{The class of the field}
\item{expr}{Expression that when evaluated initializes the field}}
