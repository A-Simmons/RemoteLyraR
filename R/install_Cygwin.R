installCygwin.windows<-function() {
  if (requireNamespace("installr", quietly = TRUE)) {
    installr::install.cygwin()
  }
}
