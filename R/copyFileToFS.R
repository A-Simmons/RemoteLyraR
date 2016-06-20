copyFileToFS_PSCP <- function(fileSource,target,username,password,host="lyra.qut.edu.au",port=22,pathToPSCP='.') {
  if (.Platform$OS.type == "windows") {
    copyFileToFS_PSCP.Windows(fileSource,target,username,password,host="lyra.qut.edu.au",port=22,pathToPSCP='.')
  } else if (.Platform$OS.type == "unix") {
    copyFileToFS_PSCP.unix(fileSource,target,username,password,host="lyra.qut.edu.au",port=22,pathToPSCP='.')
  } else {
    stop("Your platform is not supported")
  }
}

copyFileToFS_PSCP.Windows <- function(fileSource,target,username,password,host="lyra.qut.edu.au",port=22,pathToPSCP='.') {
  # Parameters for plink
  cmd.plinkCmd <- "PSCP"
  cmd.username <- paste('-l',username)
  cmd.password <- paste('-pw',password)
  cmd.port <- paste('-P',port)
  cmd.host <- host
  cmd.source <- fileSource
  cmd.target <- target
  cmd.host.target <- paste(cmd.host, cmd.target, sep=":")
  cmd.cd <- ""

  # If PSCP link is not in current directory, add the cd command to shell script
  if (pathToPSCP != ".") {
    cmd.cd <- paste("cd", pathToPSCP,'&')
  }

  # Concatenate commands for shell
  cmd <- paste(cmd.cd, cmd.plinkCmd, cmd.username, cmd.password, cmd.port, cmd.source, cmd.host.target)

  # Send command to shell
  shell(cmd)
}

copyFileToFS_PSCP.unix <- function(fileSource,target,username,password,host="lyra.qut.edu.au",port=22,pathToPSCP='.') {

}


copyFileToFS_PSCP.Unix <- function(fileSource,target,username,password,host="lyra.qut.edu.au",port=22) {

}
