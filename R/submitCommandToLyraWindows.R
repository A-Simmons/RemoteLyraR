submitCommandToLyra.Windows <- function(command,username,password,host,port) {
  # Parameters for plink
  cmd.cd <- paste("cd", addressToScripts("exe"))
  cmd.plinkCmd <- "& plink -v"
  cmd.username <- paste('-l',username)
  cmd.password <- paste('-pw',password)
  cmd.port <- paste('-P',port)
  cmd.host <- host
  cmd.command <- paste("\"",command,"\"",sep="")

  # Concatenate commands for shell
  cmd <- paste(cmd.cd, cmd.plinkCmd, cmd.username, cmd.password, cmd.port, cmd.host, cmd.command)
  # Send command to shell
  some_String <- shell(cmd,intern=TRUE)
  return(some_String)
}
