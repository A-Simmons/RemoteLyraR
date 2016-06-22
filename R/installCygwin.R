installCygwin.windows<-function() {
  requireNamespace("installr")
  success<-install.Cygwin(bit = 32, installed_option="-P dos2unix,expect,sshOpen")
}
