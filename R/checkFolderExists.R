### Check that server can be accessed
checkFolderExists <- function(directory,username,password,host="lyra.qut.edu.au",port=22) {
  command=paste("[[ -d /",directory," ]] && echo FOLDER_FOUND || echo FOLDER_NOT_FOUND",sep="")


  if (.Platform$OS.type == "windows") {
    parsed_String <- submitCommandToLyra.Windows(command,username,password,host,port)
    count <- 0
  } else if (.Platform$OS.type == "unix") {
    parsed_String <- submitCommandToLyra.Unix(command,username,password,host,port)
    count <- 1
  } else {
    error("Your platform is not supported")
  }
  if (length(grep('FOLDER_NOT_FOUND',parsed_String,value=TRUE)) > count ) {
    stop(paste("The directory: /",directory," could not be found.",sep=""))
  } else if (length(grep('FOLDER_FOUND',parsed_String,value=TRUE)) > count ) {
    print("Folder Located.")
  }
}


