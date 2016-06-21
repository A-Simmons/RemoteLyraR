#' Submit Job to HPC (Remotely Stored Project)
#'
#' @export
#' @param credentials Vector of username and password (<Username>, <Password>). REQUIRED
#' @param remote.folder Root directory for the project stored on the remote file server.REQUIRED
#' @param script.file R file to be called. REQUIRED
#' @param data ADD SOON. REQUIRED
#' @param submission.file ADD SOON
#' @param quiet Turns on quiet mode, disabling all messages except warnings and errors (default: FALSE)
#' @param host Host name for the remote server (default: lyra.qut.edu.au)
#' @param port Port number to be used for SSH and SCP to the host (defualt: 22)
#' @ignore.warning Suppress warning messages (default: FALSE)
#'
#' @description
#' Submit Job to HPC where project files are already stored on the file server.
#'
#' @details
#' Keep in mind that four parameters, credentials, remote.folder, script.file, and data are required.The
#' remote fileserver must contain the directory \code{remote.folder} and \code{script.file} must be found
#' in the top level of \code{remote.folder}.
#'
#' Before attempting to submit the job the \code{credentials} and existence of \code{remote.folder} and
#' \code{script.file} are checked. An error is given if any fail.
#'
#' \code{data} must be either a data.frame or a data.table which have the column names:
#' \itemize{
#'  \item{"JOBNAME"}{ Name of the job to be submitted to HPC. If \code{REPEAT} != 1 the job name will
#'  be affixed an identifier. While not necessary it is recommended each job name in the submission stack
#'  is unqiue (A warning will be produced if a conflixt exists). Only letters, numbers and underscores can
#'  be used, job name must begin with a letter (Violation to this will produce an error). \emph{REQUIRED}.}
#'  \item{"MEMORY"}{ The memory required for the job. Example: "100mb" or "2gb" requests 100 megabytes or
#'  2 gigabytes respectively. \emph{REQUIRED}.}
#'  \item{"WALLTIME"}{ The time requested for the job on the HPC in the form "<hours>:<mins>:<secs>".
#'  Example: "1:30:00" will request 1hour and 30minutes. A single number will be converted to hours and
#'  minutes, for example "1.5" will converted to "1:30:00". \emph{REQUIRED}.}
#'  \item{"NCPUS"}{ Number of CPUs to be requested from the node for parallelisation. \emph{NOT REQUIRED, DEFAULT: 1}.}
#'  \item{"DONOTRUNJOB"}{ If \code{TRUE} the job is not submitted. Useful for submitting larger structures
#'  of parameters without partitioning them into smaller chunks. \emph{NOT REQUIRED, DEFAULT: FALSE}.}
#'  \item{"REPEAT"}{ Resubmit a particular job \code{REPEAT} times. Useful when submitting a simulation that includes
#'  random elements. Owing to how the Random Number Generator seeds, each job could initialise with the same seed.
#'  Due to this a random integer is provided to each submission to mitigate this issue. \emph{NOT REQUIRED, DEFAULT: 1}.}
#' }
#'
#' In addition to these columns are any arguments to be passed to the user script in the form \code{<col_name>=<value>}.
#'
#' @return A structure (dataframe or datatable, matches the structure of \code{data}) which contains the details of every
#' submission. Each row of structure contains all the job details (job name, memory, wall time, ncpus, jobid) as well as
#' the arguments submitted (both user defined and provided).
#'
#' The structure is also saved as a .rds file as well as parsed into a DSV file (space-delimited). These files are saved
#' to the \code{remote.folder}.
#' @import stats
#' @import data.table

submitRemote <- function(credentials, remote.folder, script.file, data, submission.file, quiet=FALSE, host="lyra.qut.edu.au", port=22, ignore.warning=FALSE) {
  requireNamespace("stats")
  requireNamespace("data.table")
  username = credentials[1]; password = credentials[2];
  ### ARUGMENT CHECK ###
  # Check that server can be accessed
  checkConnection(username,password)

  # Check that remote folder exists
  checkFolderExists(remote.folder,username,password,host=host,port=port)

  # Check that script file exists in remote folder
  checkScriptFileExists(remote.folder,script.file,username,password,host=host,port=port)

  # Check that submission.file is in local directory
  ## ACTUALLY WRITE THIS

  # Check DATA structure
  err_warn<-checkData(data)

  ### Transform data structure
  submissionDF<-createSubmissionDataframe(data)

  ### Add submission string column
  submissionDF<-createSubmissionString(submissionDF)
}

### Create argument string
createSubmissionString<-function(df,scriptFile) {
  df$submissionString<-NA
  for (ii in 1:nrow(df)) {
    df$submissionString[ii]<-paste("-v scriptFile=",scriptFile,",",df$argument_string[ii],sep="")
  }
  return(df)
}

### Create Submission dataframe
createSubmissionDataframe<-function(data) {

  repeatColExists <- any(colnames(data) %in% "REPEAT")
  ncpusColExists <- any(colnames(data) %in% "NCPUS")
  dnrColExists <- any(colnames(data) %in% "DONOTRUN")
  # Remove all DONOTRUN rows
  if (dnrColExists) data<-data[data$DONOTRUN==TRUE,]
  # Determine number of submissions
  if (repeatColExists) nrows<-sum(data$REPEAT) else nrows<-nrow(data)

  # Create argument dataframe
  reserved.colnames<-c("JOBNAME","MEMORY","WALLTIME","NCPUS","REPEAT","DONOTRUN","seed")
  argument.names<-colnames(data)[!(colnames(data) %in% reserved.colnames)]
  parameter.names<-c("jobname","memory","walltime","ncpus",argument.names,"seed","argument_string")
  argumentDF<-data.frame(matrix(NA,ncol=length(parameter.names),nrow=nrows))
  colnames(argumentDF)<-parameter.names

  # Add RNG Seed column
  argumentDF$seed<-ceiling(runif(nrows, 0, 10^8))

  # Iterate for each submission
  for (ii in 1:nrows) {
    # Define which row in data to pull from
    if (repeatColExists) jobRow<-which(cumsum(data$REPEAT)>=ii)[1] else jobRow<-ii
    # Define number of times to repeat job (Default = 1)
    if (repeatColExists) jobRepeats<-data$REPEAT[jobRow] else jobRepeats<-1
    # Define jobname with unique ID if repeat is on (Default=1)
    if (repeatColExists) repCount<-ii-sum(data$REPEAT[0:(jobRow-1)])
    if (repeatColExists) jobname<-paste(data$JOBNAME[jobRow],"_rep",repCount,sep="") else jobname<-data$JOBNAME[jobRow]
    argumentDF$jobname[ii]<-jobname # Jobname
    # Translate values from data to argumentDF
    argumentDF$memory[ii]<-as.vector(data$MEMORY[jobRow]) # Memory
    argumentDF$walltime[ii]<-as.vector(data$WALLTIME[jobRow]) # Walltime
    if (ncpusColExists) argumentDF$ncpus[ii]<-as.vector(data$NCPUS[jobRow]) else argumentDF$ncpus[ii]<-1 # NCPUS
    argumentDF[ii,argument.names]<-data[jobRow,argument.names] # User specified arguments
    # Construct argument string
    arguments<-argumentDF[ii,c("jobname","seed",argument.names)]
    argumentDF$argument_string[ii]<-argumentString(arguments)
  }
  return(argumentDF)
}

### Create argument string
argumentString<-function(arguments) {
  argumentString<-"argString=--args"
  col.names<-colnames(arguments)
  col.values<-arguments[1,]
  for (ii in 1:ncol(arguments)) {
    argumentString<-paste(argumentString," ",col.names[ii],"=",col.values[ii],sep="")
  }
  return(argumentString)
}

### Check data structure
checkData<-function(data) {
  colnames <- colnames(data)
  jobnameColExists <- any(colnames %in% "JOBNAME")
  memoryColExists <- any(colnames %in% "MEMORY")
  walltimeColExists <- any(colnames %in% "WALLTIME")
  ncpusColExists <- any(colnames %in% "NCPUS")
  dnrColExists <- any(colnames %in% "DONOTRUN")
  repeatColExists <- any(colnames %in% "REPEAT")

  # Determine structure type
  if (is.data.frame(data)) structure<-"data.frame"
  if (is.data.table(data)) structure<-"data.table"

  # Initialise Error and Warning strings
  err_str<-character(0)
  warn_str<-character(0)

  # Check that JOBNAME, MEMORY and WALLTIME exist as columns in the data structure
  if (!((jobnameColExists) && (memoryColExists) && (walltimeColExists))) {
    # Determine which columns are missing
    if (sum(c(!jobnameColExists,!memoryColExists,!walltimeColExists))>1) plural<-"s " else plural<-" "
    if (!jobnameColExists) jobnameError<-"JOBNAME, " else jobnameError<-""
    if (!memoryColExists) memoryError<-"MEMORY, " else memoryError<-""
    if (!walltimeColExists) walltimeError<-"WALLTIME " else walltimeError<-""
    # Send error
    missingColsErr <- paste("\nColumn",plural,jobnameError,memoryError,walltimeError,"not found in ",structure,sep="")
    err_str<-paste(err_str,missingColsErr,sep="\n\n")
  }


  ### Check that each jobname is unique
  dupJobNameWarning<-character(0)
  dupJobnames <- as.vector(data$JOBNAME[duplicated(data$JOBNAME)])
  print(dupJobnames)
  for (name in dupJobnames) {
    dupJobNameWarning<-paste(dupJobNameWarning,"\n Jobname ",name," found in rows ",paste(which(data$JOBNAME %in% name),collapse=" "),sep="")
  }
  if (length(dupJobNameWarning)!=0) dupJobNameWarning<-paste("Duplicated JOBNAMEs found",dupJobNameWarning)


  ### Check formatting of memory is correct
  invalidRowsMemory<-grep("^[0-9]+[m_g]b$",tolower(data$MEMORY),invert=TRUE)
  memoryError<-createErrorString(invalidRowsMemory,data$MEMORY,"MEMORY")
  if (length(memoryError) != 0) err_str<-paste(err_str,memoryError,sep="\n\n")


  ### Check formatting of walltime is correct
  invalidRowsWalltime<-grep("^[0-9]{2}\\:[0-9]{2}\\:[0-9]{2}$|^([0-9]*\\.[0-9]+|[0-9]+)$",tolower(data$WALLTIME),invert=TRUE)
  walltimeError<-createErrorString(invalidRowsWalltime,data$WALLTIME,"WALLTIME")
  if (length(walltimeError) != 0) err_str<-paste(err_str,walltimeError,sep="\n\n")


  ### Check formatting of NCPUS is correct
  if (ncpusColExists) {
    invalidRowsNCPUS<-grep("^[0-9]*$",tolower(data$NCPUS),invert=TRUE) # Invalid in floating point form
    ncpusError<-createErrorString(invalidRowsNCPUS,data$NCPUS,"NCPUS")
    if (length(ncpusError) != 0) err_str<-paste(err_str,ncpusError,sep="\n\n")
  }


  ### Check formatting of DONOTRUN is correct
  if (dnrColExists) {
    invalidRowsDNR<-grep("^[TRUE_FALSE]+$",tolower(data$DONOTRUNJOB),invert=TRUE) # Invalid in floating point form
    dnrError<-createErrorString(invalidRowsDNR,data$DONOTRUNJOB,"DONOTRUN")
    if (length(dnrError) != 0) err_str<-paste(err_str,dnrError,sep="\n\n")
  }


  ### Check formatting of REPEAT is correct
  if (dnrColExists) {
    invalidRowsREPEAT<-grep("^[TRUE_FALSE]+$",tolower(data$REPEAT),invert=TRUE) # Invalid in floating point form
    repeatError<-createErrorString(invalidRowsREPEAT,data$REPEAT,"REPEAT")
    if (length(repeatError) != 0) err_str<-paste(err_str,repeatError,sep="\n\n")
  }


  ### Consolidate Errors and Warnings
  warn_str<-paste(dupJobNameWarning,sep="\n")
  err_warn <- c(warn_str,err_str)
  return(err_warn)
}

### Create error string from invalid rows
createErrorString<-function(rows,data.vector,colName) {
  string<-character(0)
  for (row in rows) {
    string<-paste(string,"\nRow ",row,": ",data.vector[row],sep="")
  }
  if (length(string)!=0) string<-paste("Invalid",colName,"fields found",string)
}

### Check that server can be accessed
checkConnection <- function(username,password,host="lyra.qut.edu.au",port=22) {
  if (.Platform$OS.type == "windows") {
    parsed_String <- submitCommandToLyra.Windows("exit",username,password,host,port)
  } else if (.Platform$OS.type == "unix") {
    parsed_String <- submitCommandToLyra.Unix("exit",username,password,host,port)
  } else {
    stop("Your platform is not supported")
  }

  if ((length(grep('debug1: Authentication succeeded',parsed_String,value=TRUE)) == 0 ) && (length(grep('Access granted',parsed_String,value=TRUE)) == 0 )) {
    # Connection could not be made
    stop("Connection could not be created using the provided credentials.")
  } else {
    print("Connection with LYRA established.")
    print("Credentials verified.")
  }
}

### Check that server can be accessed
checkFolderExists <- function(directory,username,password,host="lyra.qut.edu.au",port=22) {
  command=paste("[[ -d /",directory," ]] && echo FOLDER_FOUND || echo FOLDER_NOT_FOUND",sep="")


  if (.Platform$OS.type == "windows") {
    parsed_String <- submitCommandToLyra.Windows(command,username,password,host,port)
    count <- 0
  } else if (.Platform$OS.type == "unix") {
    parsed_String <- submitCommandToLyra.Unix(command,username,password,host,port)
    count <- 1
  } else {
    stop("Your platform is not supported")
  }
  if (length(grep('FOLDER_NOT_FOUND',parsed_String,value=TRUE)) > count ) {
    stop(paste("The directory: /",directory," could not be found.",sep=""))
  } else if (length(grep('FOLDER_FOUND',parsed_String,value=TRUE)) > count ) {
    print("Folder Located.")
  }
}

### Check that server can be accessed
checkScriptFileExists <- function(directory,file,username,password,host="lyra.qut.edu.au",port=22) {
  command=paste("[[ -f ./",directory,"/",file, " ]] && echo FILE_FOUND || echo FILE_NOT_FOUND",sep="")

  if (.Platform$OS.type == "windows") {
    parsed_String <- submitCommandToLyra.Windows(command,username,password,host,port)
    count <- 0
  } else if (.Platform$OS.type == "unix") {
    parsed_String <- submitCommandToLyra.Unix(command,username,password,host,port)
    count <- 1
  } else {
    stop("Your platform is not supported")
  }


  if (length(grep('FILE_NOT_FOUND',parsed_String,value=TRUE)) > count ) {
    stop(paste("The script file: /",file," could not be found.",sep=""))
  } else if (length(grep('FILE_FOUND',parsed_String,value=TRUE)) > count ) {
    print("Script file Located.")
  }
}
