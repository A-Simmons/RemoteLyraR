initDataFrame <- function()
{
  JOBNAME<-c("TEST_JOB")
  MEMORY<-c("100mb")
  WALLTIME<-c("00:10:00")
  NCPUS<-c("1")
  DONOTRUNJOB<-c(TRUE)
  Example_Parameter<-c("Hello, World")
  df <- base::data.frame(JOBNAME, MEMORY, WALLTIME, NCPUS, DONOTRUNJOB, Example_Parameter)
  return(df)
}
