installCygwin.windows<-function() {
  devtools::use_package("installr", "Suggests")
  if (requireNamespace("installer", quietly = TRUE)) {
    installer::install.cygwin()()
  }
}
