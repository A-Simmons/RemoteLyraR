#' checkConnection
#'
#' @export
checkConnection <-
  function(username,password,host = "lyra.qut.edu.au",port = 22) {
    appendToLog(c("Sending credentials to",host,"over port",port))
    if (.Platform$OS.type == "windows") {
      parsed_String <-
        submitCommandToLyra.Windows("exit",username,password,host,port)
    } else if (.Platform$OS.type == "unix") {
      parsed_String <-
        submitCommandToLyra.Unix("exit",username,password,host,port)
    } else {
      stop("Your platform is not supported")
    }

    if ((length(
      grep('debug1: Authentication succeeded',parsed_String,value = TRUE)
    ) == 0) &&
    (length(grep('Access granted',parsed_String,value = TRUE)) == 0)) {
      return(1) # Connection could not be made
    } else {
      return(0) # Connection made
    }
  }
