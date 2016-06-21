#' Submit command to HPC from Windows.
#'
#' @export
#' @param command TO BE ADDED
#' @param username TO BE ADDED
#' @param password TO BE ADDED
#' @param host Host name for the remote server (default: lyra.qut.edu.au)
#' @param port Port number to be used for SSH and SCP to the host (defualt: 22)
submitCommandToLyra <- function(command,username,password,host="lyra.qut.edu.au",port=22) {
  if (.Platform$OS.type == "windows") {
    submitCommandToLyra.Windows(command,username,password,host,port)
  } else if (.Platform$OS.type == "unix") {
    submitCommandToLyra.Unix(command,username,password,host,port)
  } else {
    stop("Your platform is not supported")
  }
}

submitCommandToLyra.Windows <- function(command,username,password,host="lyra.qut.edu.au",port=22) {
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

