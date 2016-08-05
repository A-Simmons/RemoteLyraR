#' scpToLyra
#'
#' @export
scpToLyra <- function(source, dest, username, password, host="lyra.qut.edu.au", port=22) {
  # Using the secure copy protocol (scp), copy files from a local location to the remote device. SCP requires the folder to exist on the host device.
  #
  # Args:
  #   source: Address to the local files to copy
  #   dest: Address on the remote device to copy the files to.
  #   username: Username to log in to the host.
  #   password: Password to log in to the host.
  #   host: Address for the host device. Default="lyra.qut.edu.au" (QUT's HPC).
  #   port: Port to use for SSH. Deault = 22.
  #
  # Returns:
  #   String vector of the output from the standard output of the console.
  #
  # TODO:
  #   Add a check that the remote destination exists before attempting to copy. 


  if (.Platform$OS.type == "windows") {
    std.return<-scpToLyra.Windows(source, username, password, host, port)
  } else if (.Platform$OS.type == "unix") {
    std.return<-scpToLyra.Unix(source, dest, username, password, host, port)
  } else {
    stop("Your platform is not supported")
  }
  return(std.return)
}

scpToLyra.Windows <- function(command, username, password, host="lyra.qut.edu.au", port=22) {
  # Parameters for plink
  cmd.cd <- paste("cd", addressToScripts("exe"))
  cmd.plinkCmd <- "& plink -v"
  cmd.username <- paste('-l', username)
  cmd.password <- paste('-pw', password)
  cmd.port <- paste('-P', port)
  cmd.host <- host
  cmd.command <- paste("\"", command, "\"", sep="")

  # Concatenate commands for shell
  cmd <- paste(cmd.cd, cmd.plinkCmd, cmd.username, cmd.password, cmd.port, cmd.host, cmd.command)
  # Send command to shell
  some_String <- shell(cmd, intern=TRUE)
  return(some_String)
}

scpToLyra.Unix <- function(source, dest, username, password, host, port) {
  # Parameters for ssh.exp
  cmd.scpCmd <- "; ./scp.exp"
  cmd.cd <- paste("cd", addressToScripts("sh"))

  # Concatenate commands for shell
  cmd <- paste(cmd.cd, cmd.scpCmd, paste(host,dest,sep=":"), username, password, source)

  # Send command to shell
  some_String <- system(cmd, intern=TRUE, ignore.stderr=TRUE)

  return(some_String)
}
