\name{signalingFields}
\alias{signalingFields}
\title{Signaling Fields}
\usage{signalingFields(fields, signalName="changed")}
\description{Convenience function for defining a set of reference class fields that
signals when set.}
\value{A list that is easily concatenated into the field list}
\author{Michael Lawrence}
\arguments{\item{fields}{list of names of the field and associated fileds class}
\item{signalName}{Name of the signal}}
\examples{Brush.gen <- setRefClass("Brush",
fields = signalingFields(list(color = "character",
age = "numeric")))
brush <- Brush.gen$new(color = "red", age = 2)
brush$changed$connect(function(){print("all changed")})
brush$age <- 3
brush$color <- "blue"}
