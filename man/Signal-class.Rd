\name{Signal-class}
\alias{class:Signal}
\alias{Signal-class}
% Constructor
\alias{Signal}
% Generic and methods:
\alias{length,Signal-method}
\alias{listeners}
\alias{listeners,Signal-method}
\docType{class}
\title{Signal objects}
\description{Creates a \code{Signal} object, with which a mutable object can report
changes to its state. Interested clients register a function that
is called whenever the signal is emitted. This allows those
clients to respond to changes in the object state.}
\details{A \code{Signal} object is usually created by a constructor and
stored as a field in a reference class object. Clients then access
the signal either directly or through an accessor.

The \code{Signal} reference class has the following methods:

\describe{
\item{connect(FUN, namedArgs = FALSE, ...)}{Connect \code{FUN} as a
handler for the signal. A unique identifier is returned, which
can be used to later disconnect the handler.
Handler invocation follows these rules:
\itemize{
\item{\code{namedArgs=TRUE} arguments are named in call to handler.
Otherwise, they are unnamed and matching is by position.}
\item{If a handler is missing a signal argument, the argument is
dropped when calling the handler.}
\item{A handler may have arguments not in the signal signature.}
\item{Arguments in \code{...} are appended to the handler call.}
}
}
\item{disconnect(id)}{Disconnects the handler registered with the
identifier \code{id}.}
\item{emit(<signal signature>)}{Emits the signal, calling all of its
handlers with the passed arguments. The signature depends on how the
signal was constructed. All signal args must be passed to \code{emit},
unless they have a default.
}
\item{block()}{Blocks emission of the signal. All emission requests are
ignored.}
\item{unblock()}{Unblock the signal.}
\item{buffer()}{Buffer emissions, waiting to pass them to the handlers
until \code{flush} is called.}
\item{flush()}{Flush the emission buffer, calling every handler on every
buffered emission.}
\item{accumulator(value)}{If called with no arguments, get the function,
if any, used to combine events in the buffer into a single event.
Otherwise, \code{value} replaces the current function. The accumulator
function should take one or two arguments. If it takes one argument, it
is invoked upon a flush and is passed the list of events in the buffer.
An event is simply a list containing the arguments passed to \code{emit}.
If the accumulator function takes two arguments, the function is invoked
upon every emission, when buffering is active and there is one event in
the buffer. The first argument is the currently buffered event and the
second is the new event that the function should merge into the first.
The returned event then replaces the event in the buffer.}   
}}


\section{Constructor}{
  \describe{
    \item{}{
      \code{Signal(...)}
      Create an instance of the reference class \code{Signal}
      \describe{
	\item{\code{...}}{Arguments that express the signature of the signal}
      }
    }
  }
}

\section{Accessors}{
  \describe{
    \item{}{
      \code{length(x)}:
      The number of listeners in signal x.
    }
    \item{}{
      \code{listeners(object)}:
      A list of listeners in signal x.
    }
  }
}

\author{Michael Lawrence, Tengfei Yin}
\examples{
Signal(x, y)
signal <- Signal(x, y, z = NA)
signal$connect(function(n, x, option = "none") message("x:", x),
               namedArgs = TRUE)
signal$connect(function(z, ...) message("z:", z, " x:", list(...)$x),
               namedArgs = TRUE)
signal$emit(0, 1)
##'
signal$connect(function(x, y, option = "none")
               message("y:", y, " op:", option), TRUE)
signal$connect(function(x, y, option = "none")
               message("op:", option), option = "test")
signal$connect(function(x, y, option = "none")
               message("op:", option), FALSE, "test")
id <- signal$connect(function(x, y, option = "none")
                     message("op:", option), TRUE, "test")
##'
signal$emit(0, 1)
##'
signal$disconnect(id)
signal$emit(0, 2)
##'
signal <- Signal(x)
signal$connect(function(i) print(i))
##'
signal$block()
signal$emit(0)
signal$unblock()
signal$emit(0)
##'
signal$buffer()
signal$emit(0); signal$emit(1); signal$emit(3)
signal$flush()
##'
signal$accumulator(function(prev, cur) {
  prev$x <- c(prev$x, cur$x)
  prev
})
signal$buffer()
signal$emit(0); signal$emit(1); signal$emit(3)
signal$flush()
## accessors
length(signal)
listeners(signal)
}



