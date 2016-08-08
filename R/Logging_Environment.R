#' logging
#'
#' @export
remoteLyraR.Env <- new.env()

#' logging
#'
#' @export
#' @param stringLine String or vector of strings to be added to the log#'
#' @description
#' Just some fill-in for now
#'
#' @details
#' Just some fill-in for now
#'
#' @return
#' Just some fill in for now
appendToLog <- function(stringLine) {
  #assign("log",c(get("log",envir = remoteLyraR.Env),stringLine),envir = remoteLyraR.Env)
  remoteLyraR.Env$log <- append(remoteLyraR.Env$log,stringLine)
  print(remoteLyraR.Env$log)
  }
