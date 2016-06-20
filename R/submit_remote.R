submitRemote <- function(credentials, remote_folder, script_file, data, host="lyra.qut.edu.au", port=22, submission_file=2) {

  username = credentials[1]; password = credentials[2];

  # Check that server can be accessed
  checkConnection(username,password)

  # Check that remote folder exists
  checkFolderExists(remote_folder,username,password,host=host,port=port)


  # Check that script file exists in remote folder
  checkScriptFileExists(remote_folder,script_file,username,password,host,port)
}




