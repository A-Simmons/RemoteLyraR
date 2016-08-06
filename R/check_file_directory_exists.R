#' checkRemoteFolderFileExists
#'
#' @export
checkRemoteFolderFileExists <-  function(directory, file, username, password, host = "lyra.qut.edu.au", port = 22) {
    # Checks if a folder or file can be found on a remote device.
    #
    # Args:
    #   directory: The path of the desired folder or to the desired file.
    #   file: File to be checked exists. If empty, checks the path exists.
    #   username: Username to log in to the host.
    #   password: Password to log in to the host.
    #   host: Address for the host device. Default="lyra.qut.edu.au" (QUT's HPC).
    #   port: Port to use for SSH. Deault = 22.
    #
    # Returns:
    #   A flag for success.
    #     0 = Success
    #     1 = Failed to find file/folder

    # Generate Command
    if (missing(file)) {
      grep.pattern.notfound <- "FOLDER_NOT_FOUND"
      command = paste("[[ -d ", directory, " ]] && echo FOLDER_FOUND || echo FOLDER_NOT_FOUND", sep = "")
    } else {
      grep.pattern.notfound <- "FILE_NOT_FOUND"
      command = paste("[[ -f ./", directory, "/", file, " ]] && echo FILE_FOUND || echo FILE_NOT_FOUND", sep = "")
    }

    # Submit Command to HPC
    if (.Platform$OS.type == "windows") {
      parsed_String <- submitCommandToLyra.Windows(command, username, password, host, port)
      count <- 0
    } else if (.Platform$OS.type == "unix") {
      parsed_String <- submitCommandToLyra.Unix(command, username, password, host, port)
      count <- 1
    } else {
      stop("Your platform is not supported")
    }

    # Parse String for Result
    if (length(grep(grep.pattern.notfound, parsed_String, value = TRUE)) > count) {
      return(1) # Failed to find file/folder
    } else {
      return(0) # Found file/folder
    }

  }
