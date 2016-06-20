submitCommandToLyra <- function(command,username,password,host="lyra.qut.edu.au",port=22) {
  if (.Platform$OS.type == "windows") {
    submitCommandToLyra.Windows(command,username,password,host,port)
  } else if (.Platform$OS.type == "unix") {
    submitCommandToLyra.Unix(command,username,password,host,port)
  } else {
    error("Your platform is not supported")
  }
}

submitCommandFileToLyra.Windows <- function(filePath,username,password,host,port) {
  # Parameters for plink
  cmd.plinkCmd <- "plink"
  cmd.username <- paste('-l',username)
  cmd.password <- paste('-pw',password)
  cmd.port <- paste('-P',port)
  cmd.file <- paste('-m',filePath)
  cmd.host <- host
  cmd.cd <- ""

  # If PuTTY link is not in current directory, add the cd command to shell script

  # Concatenate commands for shell
  cmd <- paste(cmd.cd, cmd.plinkCmd, cmd.username, cmd.password, cmd.port, cmd.file, cmd.host)

  # Send command to shell
  shell(cmd)
}

submitCommandToLyra.Windows <- function(command,username,password,host,port) {
  # Collapse vector of commands into a string
  if (length(command) >1) {
    command <- paste(command, collapse='\n')
  }

  # Save commands to file for plink to load
  fileName <- 'hpc_CMDs'
  write(command,paste(getwd(),fileName,sep="/"))

  # Call plink
  submitCommandFileToLyra.Windows(fileName,username,password,host,port,pathToPlink)

  # Clean up
  file.remove(fileName)
}

submitCommandToLyra.Unix <- function(command,username,password,host,port) {
  # Parameters for plink
  cmd.sshCmd <- "cd utils/ ; ./ssh.exp"
  cmd.command <- paste("\'",command,"\'",sep="")
  cmd.cd <- ""
  
  # If PuTTY link is not in current directory, add the cd command to shell script

  # Concatenate commands for shell
  cmd <- paste(cmd.cd, cmd.sshCmd, host, username, password, port, cmd.command)
  #print(cmd)
  # Send command to shell
  some_String = system(cmd,intern=TRUE,ignore.stderr=TRUE)
  #print(some_String)
  return(some_String)
}
