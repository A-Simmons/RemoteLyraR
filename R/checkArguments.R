### Check that server can be accessed
checkConnection <- function(username,password,host="lyra.qut.edu.au",port=22) {
  if (.Platform$OS.type == "windows") {
    # WINDOWS VERSION TO BE WRITTEN
  } else if (.Platform$OS.type == "unix") {
    parsed_String <- submitCommandToLyra.Unix("exit",username,password,host,port)
  } else {
    stop("Your platform is not supported")
  }

  if (length(grep('debug1: Authentication succeeded',parsed_String,value=TRUE)) == 0 ) {
    # Connection could not be made
    stop("Connection could not be created using the provided credentials.")
  } else {
    print("Connection with LYRA established.")
    print("Credentials verified.")
  }
}

### Check that server can be accessed
checkFolderExists <- function(directory,username,password,host="lyra.qut.edu.au",port=22) {
  command=paste("[[ -d /",directory," ]] && echo FOLDER_FOUND || echo FOLDER_NOT_FOUND",sep="")


  if (.Platform$OS.type == "windows") {
    # WINDOWS VERSION TO BE WRITTEN
  } else if (.Platform$OS.type == "unix") {
    parsed_String <- submitCommandToLyra.Unix(command,username,password,host,port)
  } else {
    error("Your platform is not supported")
  }
  if (length(grep('FOLDER_NOT_FOUND',parsed_String,value=TRUE)) > 1 ) {
    stop(paste("The directory: /",directory," could not be found.",sep=""))
  } else if (length(grep('FOLDER_FOUND',parsed_String,value=TRUE)) > 1 ) {
    print("Folder Located.")
  }
}


### Check that server can be accessed
checkScriptFileExists <- function(directory,file,username,password,host="lyra.qut.edu.au",port=22) {
  command=paste("[[ -f ./",directory,"/",file, " ]] && echo FILE_FOUND || echo FILE_NOT_FOUND",sep="")

  if (.Platform$OS.type == "windows") {
    # WINDOWS VERSION TO BE WRITTEN
  } else if (.Platform$OS.type == "unix") {
    parsed_String <- submitCommandToLyra.Unix(command,username,password,host,port)
  } else {
    error("Your platform is not supported")
  }

  if (length(grep('FILE_NOT_FOUND',parsed_String,value=TRUE)) > 1 ) {
    stop(paste("The script file: /",file," could not be found.",sep=""))
  } else if (length(grep('FILE_FOUND',parsed_String,value=TRUE)) > 1 ) {
    print("Script file Located.")
  }
}
