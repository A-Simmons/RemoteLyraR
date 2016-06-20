command="ls -l"
username="n8352747"
password="7467136573dD4"
host="lyra.qut.edu.au"
port="22"
submitCommandFileToLyra.Windows(command,username,password,host,port)


credentials <- c("n8352747", "7467136573dD4")
remote_folder <- "bin"
script_file <- "scp.exp"
submitRemote(credentials,remote_folder,script_file)
