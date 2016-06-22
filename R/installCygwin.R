installCygwin.windows<-function() {
  devtools::use_package("installr", "Suggests")
  success<-install.Cygwin(bit = 32, installed_option="-P dos2unix,expect,sshOpen")
}
