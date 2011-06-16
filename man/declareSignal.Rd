\name{declareSignal}
\alias{declareSignal}
\title{Declaring a signal field}
\usage{declareSignal(expr)}
\description{Declares a signal field that is lazily populated when the field is
first accessed. This avoids the need for the
constructor/initializer to explicitly create the signal.}
\value{A list of field definitions, suitable for passing to
\code{\link{setRefClass}}.}
\author{Michael Lawrence}
\arguments{\item{expr}{The expression that names the signal and specifies its
signature. See the example.}}
\examples{setRefClass("Dataset", fields = c(elements = "list",
declareSignal(elementsChanged(which))))}
