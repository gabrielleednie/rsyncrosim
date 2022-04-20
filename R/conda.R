# Copyright (c) 2021 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Conda path of SyncroSim Session
#'
#' Checks or sets the SyncroSim \code{\link{Session}} Conda installation Path.
#' If no path is specified and a package requires Conda environments, then the
#' default Conda installation path will be used.
#'
#' @param session \code{\link{Session}} object or character (i.e. filepath to a 
#' session). If \code{NULL}, \code{session()} will be used
#' @param value character. If not \code{NULL} (default), the SyncroSim Session 
#' will use the given path to the Conda installation. 
#' 
#' @return 
#' A character: The set path to the Conda installation.
#' 
#' @examples 
#' \donttest{
#' # Set up a SyncroSim Session
#' mySession <- session()
#' 
#' # Check the Conda path of a SyncroSim Session
#' conda(mySession)
#' 
#' # Set the Conda path for a SyncroSim Session
#' conda(mySession) <- "C:/Users/Admin/miniconda3"
#' }
#' 
#' @export
setGeneric("conda", function(session = NULL) standardGeneric("conda"))

#' @rdname conda
setMethod("conda", signature(session = "Session"), function(session){
  tt <- command(list(conda = NULL, config = NULL), session)
  return(tt)
})

#' @rdname conda
setMethod("conda", signature(session = "missingOrNULLOrChar"), function(session) {
  if (class(session) == "character") {
    session <- .session(session)
  } else {
    session <- .session()
  }
  if ((class(session) == "character") && (session == SyncroSimNotFound(warn = FALSE))) {
    return(SyncroSimNotFound())
  }
  tt <- command(list(conda = NULL, config = NULL), session)
  return(tt)
})

#' @rdname conda
#' @export
setGeneric("conda<-", function(session, value) standardGeneric("conda<-"))
#' @rdname conda
setReplaceMethod(
  f = "conda",
  signature = "character",
  definition = function(session, value) {
    return(session)
  }
)

#' @rdname conda
setReplaceMethod(
  f = "conda",
  signature = "Session",
  definition = function(session, value) {
    if (is.null(value)) {
      tt <- command(list(conda = NULL, clear = NULL), session)
      if (!identical(tt, "Conda path successfully removed.")){
        stop(tt)
      }
    } else if (is.character(value)) {
      if (!dir.exists(value)) {
        warning(paste0("Conda installation not found: ", value,
                       " Either install Conda in the specified folder or set value to NULL to use the default path. See ?conda for details."))
        return(SyncroSimNotFound(warn = FALSE)) # Should probably be different exception, change later
      }
      tt <- command(list(conda = NULL, path = value), session)
      if (!identical(tt, "Conda path successfully set.")) {
        stop(tt)
      }
    }
    # session@conda <- value
    print(tt)
    return(session)
  }
)