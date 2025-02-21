# Copyright (c) 2021 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Add row(s) to a data.frame
#'
#' This function is mostly used internally to add rows to data.frames
#' associated with SyncroSim Datasheets retrieved from the command line.
#'
#' @details
#' Preserves the types and factor levels of the targetDataframe.
#' Fills missing values if possible using factor levels.
#' If value is a named vector or list, it will be converted to a single row 
#' data.frame. If value is an unnamed vector or list, the number of elements 
#' should equal the number of columns in the targetDataframe; elements are 
#' assumed to be in same order as data.frame columns.
#'
#' @param targetDataframe data.frame
#' @param value data.frame, character string, vector, or list. Columns or elements
#'     in value should be a subset of columns in targetDataframe
#' 
#' @return 
#' A dataframe with new rows.
#' 
#' @examples
#' # Create an example data.frame
#' oldDataframe <- as.data.frame(mtcars)
#' 
#' # Add a single row to the example data.frame
#' newDataframe <- addRow(oldDataframe, list(mpg = 100, wt = 10))
#' 
#' # Create an example data.frame with more than one row of data
#' multipleRows <- data.frame(mpg = c(40, 50, 75), wt = c(4, 7, 6))
#' 
#' # Add the old example data.frame to the new example data.frame
#' newDataframe <- addRow(oldDataframe, multipleRows)
#' 
#' @export
setGeneric("addRow", function(targetDataframe, value) standardGeneric("addRow"))

#' @rdname addRow
setMethod("addRow",
  signature = "data.frame",
  definition = function(targetDataframe, value) {
    inNames <- names(value)
    if (class(value)[1] == "character") {
      value <- as.data.frame(t(value), stringsAsFactors = FALSE)
    }

    # if value is list
    if (class(value)[1] == "list") {
      value <- as.data.frame(value, stringsAsFactors = FALSE)

      if (nrow(value) != 1) {
        stop("Can't convert value to a single row data.frame.")
      }
    }

    if (length(setdiff(names(value), names(targetDataframe))) > 0) {
      if (is.null(inNames)) {
        if (ncol(value) == ncol(targetDataframe)) {
          names(value) <- names(targetDataframe)
        } else {
          stop("The number of elements in value does not equal the number of columns in the targetDataframe. Provide names, or ensure the correct number of elements in value.")
        }
      } else {
        stop("Column names not recognized: ", paste(setdiff(names(value), names(targetDataframe))), collapse = ",")
      }
    }
    for (i in seq(length.out = ncol(value))) {
      cName <- names(value)[i]
      if (is.factor(targetDataframe[[cName]])) {
        notAllowed <- setdiff(value[[cName]], levels(targetDataframe[[cName]]))
        if (length(notAllowed) > 0) {
          stop("Invalid values for ", cName, " : ", paste(notAllowed, collapse = ","))
        }
      } else {
        if (is.factor(value[[cName]])) {
          value[[cName]] <- as.character(value[[cName]])
        }
        class(value[[cName]]) <- class(targetDataframe[[cName]])
      }
    }

    # Note - will not add row if that exact row already exists.
    out <- merge(targetDataframe, value, all = TRUE)

    # Now fill in missing factor values if possible
    # for (i in seq(length.out = ncol(out))) {
    #   cName <- names(out)[i]
    #   if (is.factor(targetDataframe[[cName]])) {
    #     if (length(levels(targetDataframe[[cName]])) == 1) {
    #       out[[cName]] <- levels(targetDataframe[[cName]])[1]
    #     }
    #     out[[cName]] <- factor(out[[cName]], levels = levels(targetDataframe[[cName]]))
    #   }
    # }

    out <- subset(out, select = names(targetDataframe))
    return(out)
  }
)
