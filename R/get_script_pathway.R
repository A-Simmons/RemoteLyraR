addressToScripts <- function(type) {
  # Finds the path to non-R scripts in this package.
  #
  # Args:
  #   type: The type of scripts to be found.
  #
  # Returns:
  #   The path to the script type.

  if (type == "sh") {
    dir <- system.file(package="RemoteLyraR", "bin")
    if (.Platform$OS.type == "unix") {
      system(paste("cd ", dir,"; chmod 755 *", sep=""))
    }
  } else if (type == "exe") {
    dir <- system.file(package="RemoteLyraR", "executables", "win32")
  }
  return(dir)
}
