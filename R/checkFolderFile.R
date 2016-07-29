#' checkFolderExists
#'
#' @export
checkRemoteFolderFileExists <-
  function(directory,file,username,password,host = "lyra.qut.edu.au",port = 22) {

    # Generate Command
    if (missing(file)) {
      grep.pattern.notfound <- "FOLDER_NOT_FOUND"
      command = paste("[[ -f ./",directory,"/",file, " ]] && echo FILE_FOUND || echo FILE_NOT_FOUND",sep =
                        "")
    } else {
      grep.pattern.notfound <- "FILE_NOT_FOUND"
      command = paste("[[ -d ",directory," ]] && echo FOLDER_FOUND || echo FOLDER_NOT_FOUND",sep =
                        "")
    }

    # Submit Command to HPC
    if (.Platform$OS.type == "windows") {
      parsed_String <-
        submitCommandToLyra.Windows(command,username,password,host,port)
      count <- 0
    } else if (.Platform$OS.type == "unix") {
      parsed_String <-
        submitCommandToLyra.Unix(command,username,password,host,port)
      count <- 1
    } else {
      stop("Your platform is not supported")
    }

    # Parse String for Result
    if (length(grep(grep.pattern.notfound,parsed_String,value = TRUE)) > count) {
      return(1) # Failed to find file/folder
    } else {
      return(0) # Found file/folder
    }

  }
