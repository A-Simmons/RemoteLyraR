#' checkConnection
#'
#' @export
#' @param username Username for your QUT HPC account
#' @param password PAssword for your QUT HPC account
#' @param host Address to QUT HPC
#' @param port Port to use for SSH. (Default = 22).
#'
#'
#' @description
#' Just some fill-in for now
#'
#' @details
#' Just some fill-in for now
#'
#' @return
#' Just some fill in for now
checkConnection <- function(username, password, host = "lyra.qut.edu.au", port = 22) {
  # Determines if a connection can be made with the host device using the provided credentials.
  #
  # Args:
  #   username: Username to log in to the host.
  #   password: Password to log in to the host.
  #   host: Address for the host device. Default="lyra.qut.edu.au" (QUT's HPC).
  #   port: Port to use for SSH. Deault = 22.
  #
  # Returns:
  #   A flag for success.
  #     0 = Success
  #     1 = Connection failed
  #
  # TODO:
  #   Create additional flags to differentiate failure types.
  #     1: Host couldn't be reached
  #     2: Credentials not accepted

    appendToLog(paste("Sending credentials to", host, "over port", port))
    if (.Platform$OS.type == "windows") {
      parsed_String <-
        submitCommandToLyra.Windows("exit", username, password, host, port)
    } else if (.Platform$OS.type == "unix") {
      parsed_String <-
        submitCommandToLyra.Unix("exit", username, password, host, port)
    } else {
      appendToLog("Platform not supported. Stopping.")
      stop("Your platform is not supported")
    }
    appendToLog(parsed_String)

    if ((length(
      grep('debug1: Authentication succeeded', parsed_String, value = TRUE)
    ) == 0) &&
    (length(grep('Access granted', parsed_String, value = TRUE)) == 0)) {
      return(1) # Connection could not be made
    } else {
      return(0) # Connection made
    }
  }
