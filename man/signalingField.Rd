\name{signalingField}
\alias{signalingField}
\title{Signaling Field}
\usage{signalingField(name, class, signalName=paste(name, "Changed", sep =
    ""))}
\description{Convenience function for defining a reference class field that
signals when set.}
\value{A list that is easily concatenated into the field list}
\author{Michael Lawrence}
\arguments{\item{name}{Name of the field}
\item{class}{Class name of the field}
\item{signalName}{Name of the signal}}
\examples{Brush.gen <- setRefClass("Brush",
fields = signalingField("color", "character"))
brush <- Brush.gen$new(color = "blue")
brush$colorChanged$connect(function() print(brush$color))
brush$color <- "red"}
