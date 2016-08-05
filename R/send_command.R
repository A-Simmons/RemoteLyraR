
submitCommandToLyra <- function(command, username, password, host="lyra.qut.edu.au", port=22) {
  # Submits a command over SSH to the specified host. This function chooses between using two helper functions, one for Windows and one for unix-like devices.
  #
  # Args:
  #   command: A string of the command to be sent.
  #   username: Username to log in to the host.
  #   password: Password to log in to the host.
  #   host: Address for the host device. Default="lyra.qut.edu.au" (QUT's HPC).
  #   port: Port to use for SSH. Deault = 22.
  #
  # Returns:
  #   String vector of the output from the standard output of the console.

  if (.Platform$OS.type == "windows") {
    std.return<-submitCommandToLyra.Windows(command, username, password, host, port)
  } else if (.Platform$OS.type == "unix") {
    std.return<-submitCommandToLyra.Unix(command, username, password, host, port)
  } else {
    stop("Your platform is not supported")
  }
  return(std.return)
}

submitCommandToLyra.Windows <- function(command, username, password, host="lyra.qut.edu.au", port=22) {
  # Submits a command over SSH to the specified host for local WINDOWS devices. The command is sent using an expect shell script ssh.exp found in /inst/bin/.
  #
  # Args:
  #   command: A string of the command to be sent.
  #   username: Username to log in to the host.
  #   password: Password to log in to the host.
  #   host: Address for the host device. Default="lyra.qut.edu.au" (QUT's HPC).
  #   port: Port to use for SSH. Deault = 22.
  #
  # Returns:
  #   String vector of the output from the standard output of the console.
  # TODO:
  #   Write this function properly to utilise Cygwin

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

submitCommandToLyra.Unix <- function(command, username, password, host, port) {
  # Submits a command over SSH to the specified host for local UNIX-like devices. The command is sent using an expect shell script ssh.exp found in /inst/bin/.
  #
  # Args:
  #   command: A string of the command to be sent.
  #   username: Username to log in to the host.
  #   password: Password to log in to the host.
  #   host: Address for the host device. Default="lyra.qut.edu.au" (QUT's HPC).
  #   port: Port to use for SSH. Deault = 22.
  #
  # Returns:
  #   String vector of the output from the standard output of the console.

  # Parameters for ssh.exp
  cmd.sshCmd <- "; ./ssh.exp"
  cmd.command <- paste("\'", command, "\'", sep="")
  cmd.cd <- paste("cd", addressToScripts("sh"))

  # Concatenate commands for shell
  cmd <- paste(cmd.cd, cmd.sshCmd, host, username, password, port, cmd.command)

  # Send command to shell
  some_String <- system(cmd, intern=TRUE, ignore.stderr=TRUE)

  return(some_String)
}
