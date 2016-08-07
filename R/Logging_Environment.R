#' logging
#'
#' @export
remoteLyraR.Env <- new.env()

### Initiate Submission Log File
createLogFIle <-  function() {
  # Sets up the log file in the remoteLyraR environment
  #
  # Args:
  #
  # Returns:
  #
    file.name <- paste("Batch_", gsub(" ", "_", Sys.time(), fixed = TRUE), ".log", sep = "")
    file.name <- gsub("-", "_", file.name, fixed = TRUE)
    file.name <- gsub(":", "_", file.name, fixed = TRUE)

    assign("log.filename", file.name, envir = remoteLyraR.Env)
    assign("log", "", envir = remoteLyraR.Env)
  }

###
appendToLog <- function(stringLine,type="MSG",stop=FALSE,print=FALSE) {
  # Appends a new line to the log file
  #
  # Args:
  #   stringLine: The line to be added to the log file
  #   type: Type of message. 'MSG' (default): for standard message. 'Error': For an error. 'Warning': For a warning.
  #   stop: Stop running code if true
  #
  # Returns:
  #
  if (type == "Error") {
    stringLineAdj <- paste("Error:",stringLine)
  } else if (type == "Warning")
    stringLineAdj <- paste("Warning:",stringLine)
  } else {
    stringLineAdj <- stringLine
  }

  remoteLyraR.Env$log <- append(remoteLyraR.Env$log,stringLine)

  if (stop == TRUE) {
    stop(stringLine)
  else if (print == TRUE) {
    print(stringLine)
  }


  }
