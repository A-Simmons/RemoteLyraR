addressToScripts <- function(type) {
  if (type == "sh") {
    dir <- system.file(package="RemoteLyraR", "bin")
  } else if (type == "exe") {
    dir <- system.file(package="RemoteLyraR", "executables", "win32")
  }
}
