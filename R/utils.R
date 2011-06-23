## ======================================================================
## utils
## ======================================================================


##' Convenience function for defining a reference class field that
##' signals when set.
##'
##' @title Signaling Field
##' @param name Name of the field
##' @param class Class name of the field
##' @param signalName Name of the signal
##' @return A list that is easily concatenated into the field list
##' @author Michael Lawrence
##' @examples Brush.gen <- setRefClass("Brush",
##' fields = signalingField("color", "character"))
##' brush <- Brush.gen$new(color = "blue")
##' brush$colorChanged$connect(function() print(brush$color))
##' brush$color <- "red"
##' @export
signalingField <- function(name, class,
                           signalName = paste(name, "Changed", sep = ""))
{
  .name <- paste(".", name, sep = "")
  body <- substitute({
    if (missing(val))
      .name
    else {
      if (!is(val, .class))
        stop("Cannot set an object of type '", class(val), "' on '", name,
             "', a field of type '", .class, "'")
      changed <- !identical(.name, val)
      .name <<- val
      if (changed) {
        if (is.null(body(signal$emit)))
          signal <<- Signal() # lazy construction of signal
        signal$emit()
      }
    }
  }, list(.name = as.name(.name), name = name, signal = as.name(signalName),
          .class = class))
  structure(list(as.function(c(alist(val=), body)), class, "Signal"),
            names = c(name, .name, signalName))
}

##' Convenience function for defining a set of reference class fields that
##' signals when set.
##'
##' @title Signaling Fields 
##' @param fields list of names of the field and associated fileds class 
##' @param signalName Name of the signal
##' @return A list that is easily concatenated into the field list
##' @author Michael Lawrence
##' @examples Brush.gen <- setRefClass("Brush",
##'                         fields = signalingFields(list(color = "character",
##'                           age = "numeric")))
##' brush <- Brush.gen$new(color = "red", age = 2)
##' brush$changed$connect(function(){print("all changed")})
##' brush$age <- 3
##' brush$color <- "blue"
##' @export
signalingFields <- function(fields, signalName = "changed") {
  if (!length(fields))
    return(list())
  .fieldNames <- paste(".", names(fields), sep = "")
  activeFields <- mapply(function(fieldClass, fieldName, .fieldName) {
    as.function(c(alist(val=), substitute({
      if (missing(val)) {
        .fieldName
      } else {
        tmpVal <- try(as(val, fieldClass, strict = FALSE), silent = TRUE)
        if (is(tmpVal, "try-error"))
          stop("Cannot set an object of type '", class(val), "' on '",
               fieldName, "', a field of type '", fieldClass, "'")
        else if (!isTRUE(msg <- validObject(tmpVal, TRUE)))
          stop("Attempt to set invalid value on '", fieldName, "': ", msg)
        val <- tmpVal
        .fieldName <<- val
        signalName$emit(fieldName)
      }
    }, list(.fieldName = as.name(.fieldName),
            fieldClass = fieldClass, fieldName = fieldName,
            signalName = as.name(signalName)))))
  }, fields, names(fields), .fieldNames)
  c(activeFields, structure(fields, names = .fieldNames),
    lazyField(signalName,"Signal", Signal(name))) 
}

## call("declareSignal", as.name(signalName))
## eval(substitute(declareSignal(signal(name)), list(signal = as.name(signalName))))


##' Declares a lazily initialized field, with the class and initializer.
##'
##' @title Lazily initialize the fileds
##' @param name The name of the field
##' @param class The class of the field
##' @param expr Expression that when evaluated initializes the field
##' @return A list suitable for use with \code{\link{setRefClass}}
##' @author Michael lawrence
##' @export

lazyField <- function(name, class, expr) {
  .name <- paste(".", name, sep = "")
  .init <- paste(".init", name, sep = ".")
  expr <- substitute(expr)
  body <- substitute({
    if (missing(val)) {
      if (!length(.init)) {
        .name <<- expr
        .init <<- TRUE
      }
      .name
    }
    else {
      if (!is(val, .class))
        stop("Cannot set an object of type '", class(val), "' on '", name,
             "', a field of type '", .class, "'")
      .name <<- val
      .init <<- TRUE
    }
  }, list(.name = as.name(.name), name = name, .class = class, expr = expr,
          .init = as.name(.init)))
  structure(list(as.function(c(alist(val=), body)), class, "logical"),
            names = c(name, .name, .init))
}

##' Declares a signal field that is lazily populated when the field is
##' first accessed. This avoids the need for the
##' constructor/initializer to explicitly create the signal.
##'
##' @title Declaring a signal field
##' @param expr The expression that names the signal and specifies its
##' signature. See the example.
##' @return A list of field definitions, suitable for passing to
##' \code{\link{setRefClass}}.
##' @author Michael Lawrence
##' @examples
##' setRefClass("Dataset", fields = c(elements = "list",
##'   declareSignal(elementsChanged(which))))
##' @export
declareSignal <- function(expr) {
  expr <- substitute(expr)
  name <- deparse(expr[[1]])
  expr[[1]] <- quote(Signal)
  do.call(lazyField, list(name, "Signal", expr))
}

