### Check that server can be accessed
checkScriptFileExists <- function(directory,file,username,password,host="lyra.qut.edu.au",port=22) {
  command=paste("[[ -f ./",directory,"/",file, " ]] && echo FILE_FOUND || echo FILE_NOT_FOUND",sep="")

  if (.Platform$OS.type == "windows") {
    parsed_String <- submitCommandToLyra.Windows(command,username,password,host,port)
    count <- 0
  } else if (.Platform$OS.type == "unix") {
    parsed_String <- submitCommandToLyra.Unix(command,username,password,host,port)
    count <- 1
  } else {
    stop("Your platform is not supported")
  }


  if (length(grep('FILE_NOT_FOUND',parsed_String,value=TRUE)) > count ) {
    stop(paste("The script file: /",file," could not be found.",sep=""))
  } else if (length(grep('FILE_FOUND',parsed_String,value=TRUE)) > count ) {
    print("Script file Located.")
  }
}
