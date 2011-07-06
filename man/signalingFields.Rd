\name{signalingFields}
\alias{signalingFields}
\title{Signaling Fields}
\usage{signalingFields(fields, signalName="changed")}
\description{Convenience function for defining a set of reference class fields that
signals when set.}
\details{When constructing signaling fields in this way, each field has the ability
to register its own signal and at the same time, there is one top level signal
which could be emitted no matter which field changes. Please see the example
to learn to register global signal and individual signal.}
\value{A list that is easily concatenated into the field list}
\author{Michael Lawrence, Tengfei Yin}
\arguments{\item{fields}{list of names of the field and associated fields class}
\item{signalName}{Name of the signal}}
\examples{Brush.gen <- setRefClass("Brush",
                       fields = signalingFields(list(color = "character",
                          age = "numeric"), signalName = "brushChanged"))
brush <- Brush.gen$new(color = "red", age = 2)
brush$brushChanged$connect(function(x){print(paste(x,
"changed, so emit any-changed signal"))})
brush$colorChanged$connect(function() print("colorChanged signal emited"))
brush$age <- 3
brush$color <- "blue"}
