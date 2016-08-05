#' scpToLyra
#'
#' @export
scpToLyra <- function(source,dest,username,password,host="lyra.qut.edu.au",port=22) {
  if (.Platform$OS.type == "windows") {
    std.return<-scpToLyra.Windows(source,username,password,host,port)
  } else if (.Platform$OS.type == "unix") {
    std.return<-scpToLyra.Unix(source,dest,username,password,host,port)
  } else {
    stop("Your platform is not supported")
  }
  return(std.return)
}

scpToLyra.Windows <- function(command,username,password,host="lyra.qut.edu.au",port=22) {
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

scpToLyra.Unix <- function(source,dest,username,password,host,port) {
  # Parameters for ssh.exp
  cmd.scpCmd <- "; ./scp.exp"
  cmd.cd <- paste("cd", addressToScripts("sh"))

  # Concatenate commands for shell
  cmd <- paste(cmd.cd, cmd.scpCmd, paste(host,dest,sep=":"), username, password, source)

  # Send command to shell
  some_String <- system(cmd,intern=TRUE,ignore.stderr=TRUE)

  return(some_String)
}
