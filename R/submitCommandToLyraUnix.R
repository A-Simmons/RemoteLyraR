submitCommandToLyra.Unix <- function(command,username,password,host,port) {
  # Parameters for ssh.exp
  cmd.sshCmd <- "; ./ssh.exp"
  cmd.command <- paste("\'",command,"\'",sep="")
  cmd.cd <- paste("cd", addressToScripts("sh"))

  # Concatenate commands for shell
  cmd <- paste(cmd.cd, cmd.sshCmd, host, username, password, port, cmd.command)

  # Send command to shell
  some_String <- system(cmd,intern=TRUE,ignore.stderr=TRUE)

  return(some_String)
}
