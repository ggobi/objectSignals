## ======================================================================
## utils
## ======================================================================

### FIXME: do we really want to keep signalingField() around?

##' Convenience function for defining a reference class field that
##' signals when set.
##'
##' @title Signaling Field
##' @param name Name of the field
##' @param class Class name of the field
##' @param signalName Name of the signal
##' @return A list that is easily concatenated into the field list
##' @author Michael Lawrence
##' @example objectSignals/inst/examples/signalingField.R
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
      else val <- as(val, .class, strict = FALSE)
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
##' When constructing signaling fields in this way, each field has the
##' ability to register its own signal and at the same time, there is
##' one top level signal which could be emitted no matter which field
##' changes. Please see the example to learn to register global signal
##' and individual signal.
##'
##' @title Signaling Fields 
##' @param fields list of names of the field and associated fields class 
##' @param signalName Name of the signal
##' @return A list that is easily concatenated into the field list
##' @author Michael Lawrence, Tengfei Yin
##' @example objectSignals/inst/examples/signalingFields.R
##' @export
signalingFields <- function(fields, prototype = list(), signalName = "changed")
{
  if (!length(fields))
    return(list())
  .fieldNames <- paste(".", names(fields), sep = "")
  .initNames <- paste(".init.", names(fields), sep = "")
  hasPrototype <- names(fields) %in% names(prototype)
  if (any(sapply(fields, is.function) & hasPrototype))
    stop("An active binding field cannot have a prototype")
  activeFields <- mapply(function(fieldClass, fieldName, .fieldName, initName,
                                  hasPrototype, prototype, thisSignal)
  {
    as.function(c(alist(val=), substitute({
      if (missing(val)) {
        if (hasPrototype && !length(initName)) {
          .fieldName <<- prototype
          initName <<- TRUE
        }
        .fieldName
      } else {
        if (!is.function(fieldClass)) {
          if (!is(val, fieldClass))
            stop("Cannot set an object of type '", class(val), "' on '",
                 fieldName, "', a field of type '", fieldClass, "'")
          else val <- as(val, fieldClass, strict = FALSE)
        }
        ## careful here; if field is active binding, it might not change
        oldVal <- .fieldName
        .fieldName <<- val
        if (hasPrototype) initName <<- TRUE
        if (!identical(oldVal, .fieldName)) {
          signalName$emit(fieldName)
          thisSignal$emit()
        }
      }
    }, list(.fieldName = as.name(.fieldName),
            fieldClass = fieldClass, fieldName = fieldName,
            thisSignal = as.name(thisSignal),
            initName = as.name(initName),
            hasPrototype = hasPrototype,
            prototype = prototype,
            signalName = as.name(signalName)))))
  }, fields, names(fields), .fieldNames, .initNames, hasPrototype,
     prototype[names(fields)], paste(names(fields), "Changed", sep = ""))
  indSigs <- lapply(names(fields), function(nm) {
    nm <- paste(nm, "Changed", sep = "")
    fieldWithPrototype(nm, "Signal", Signal())
  })
  c(activeFields, structure(fields, names = .fieldNames),
    structure(rep("logical", sum(hasPrototype)),
              names = .initNames[hasPrototype]),
    fieldWithPrototype(signalName, "Signal", Signal(name)),
    unlist(indSigs))
}


##' A convenience for declaring a default value for a field, in the
##' vein of \code{\link[methods]{prototype}} for S4 classes, except
##' the default value is quoted and evaluated lazily.
##'
##' @title Fields with prototypes
##' @param name The name of the field
##' @param class The class of the field
##' @param value Default value that when evaluated
##' initializes the field
##' @return A list suitable for use with \code{\link{setRefClass}}
##' @author Michael lawrence
##' @export

fieldWithPrototype <- function(name, class, value) {
  .name <- paste(".", name, sep = "")
  .init <- paste(".init", name, sep = ".")
  value <- substitute(value)
  body <- substitute({
    if (missing(val)) {
      if (!length(.init)) {
        .name <<- value
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
  }, list(.name = as.name(.name), name = name, .class = class, value = value,
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
  do.call(fieldWithPrototype, list(name, "Signal", expr))
}


