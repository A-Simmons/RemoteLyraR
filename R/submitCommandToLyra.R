submitCommandToLyra <- function(command,username,password,host="lyra.qut.edu.au",port=22) {
  if (.Platform$OS.type == "windows") {
    submitCommandToLyra.Windows(command,username,password,host,port)
  } else if (.Platform$OS.type == "unix") {
    .submitCommandToLyra.Unix(command,username,password,host,port)
  } else {
    error("Your platform is not supported")
  }


}
