#' Submit Job to HPC (Remotely Stored Project)
#'
#' @export
#' @param credentials Vector of username and password (<Username>, <Password>). REQUIRED
#' @param remote.folder Root directory for the project stored on the remote file server.REQUIRED
#' @param script.file R file to be called. REQUIRED
#' @param submission.file ADD SOON
#' @param data ADD SOON. REQUIRED
#' @param quiet Turns on quiet mode, disabling all messages except warnings and errors (default: FALSE)
#' @param host Host name for the remote server (default: lyra.qut.edu.au)
#' @param port Port number to be used for SSH and SCP to the host (defualt: 22)
#' @param ignore.warning Suppress warning messages (default: FALSE)
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
submitRemote <- function(credentials, remote.folder, script.file, submission.file, data, quiet=FALSE, host="lyra.qut.edu.au", port=22, ignore.warning=FALSE) {
  # Main function for submitting a batch of jobs to the HPC where the R scripts are stored on the HPC file server.
  #
  # Args:
  #   credentials: Vector of the username and password c("<username>", "<password>").
  #   remote.folder: PATH to the folder storing the script files on the remote device.
  #   script.file: Name of the script file to be called on the remote device.
  #   submission.file: Name of the PBS submission file.
  #   data: Data frame of the data to be segmented and submitted. Includes Job details sauch as name, runtime, memory and ncpus
  #   quiet:
  #   host:
  #   port:
  #   ignore.warning:
  #
  # Returns:
  #
  # TODO:
  #   Add logging

    requireNamespace("stats")
    requireNamespace("data.table")
    username = credentials[1]
    password = credentials[2]

    ### ARUGMENT CHECK ###
    # Check that server can be accessed
    flag <- checkConnection(username, password)
    if (flag) {
      stop("Connection refused. Check credentials.")
    }

    # Check that remote folder exists
    flag <- checkRemoteFolderFileExists(directory=remote.folder, username=username, password=password, host=host, port=port)
    if (flag) {
      stop("Could not find remote folder.")
    }

    # Check that script file exists in remote folder
    flag <- checkRemoteFolderFileExists(directory=remote.folder, file=script.file, username=username, password=password, host=host, port=port)
    if (flag) {
      stop(paste("Could not find script file", script.file))
    }

    # Check that submission.file is in remote directory
    flag <- checkRemoteFolderFileExists(directory=remote.folder, file=submission.file, username=username, password=password, host=host, port=port)
    if (flag) {
      stop(paste("Could not find script file", submission.file))
    }

    # Check DATA structure
    #err_warn<-checkData(data)

    ### Transform data structure
    submissionDF <- createSubmissionDataframe(data)

    ### Add submission string column
    submissionDF <- createSubmissionString(submissionDF, script.file)

    ## Send submission to LYRA
    sendSubmission(submissionDF, remote.folder, submission.file, credentials, host, port)
  }

### Initiate Submission Log File
createLogFIle <-  function() {
  # Sets up the log file in the remoteLyraR environment
  #
  # Args:
  #
  # Returns:
  #
    file.name <- paste("Batch_", gsub(" ", "_", Sys.time(), fixed = TRUE), ".log", sep = "")
    file.name <- gsub("-", "_", file.name, fixed = TRUE)
    file.name <- gsub(":", "_", file.name, fixed = TRUE)

    assign("log.filename", file.name, envir = remoteLyraR.Env)
    assign("log", "", envir = remoteLyraR.Env)
  }

### Send Job Batch
sendSubmission <- function(df, remote.folder, submission.file, credentials, host, port) {
  # Sends the actual submission requests to HPC queue.
  #
  # Args:
  #   df: Data frame containing all the job details and data.
  #   remote.folder: PATH to folder on remote device (HPC file server) that contains the R scripts.
  #   submission.file: name of the PBS submission file all the details are sent to.
  #   credentials: Vector containing Username and Password for the HPC
  #   host:
  #   port:
  #
  # Returns:
  #
    username = credentials[1]
    password = credentials[2]

    nrows <- nrow(df)
    for (ii in 1:nrows) {
      submissionCmd <- paste("cd ", remote.folder, "; qsub ", df$submissionString[ii], " -N ", df$jobname[ii], " -l walltime=", df$walltime[ii], " -l ncpus=", df$ncpus[ii], " -l mem=", df$memory[ii], " ", submission.file, sep = "")
      std.out <- submitCommandToLyra(submissionCmd, username, password)
      print(paste("Job", df$jobname[ii], "submitted successfully with Job ID", sub(".pbs\\r", "\\1", std.out[82]), sep = " "))
    }
  }

### Create argument string
createSubmissionString <- function(df, scriptFile) {
  # Creates the string command for submission.
  #
  # Args:
  #   df: Dataframe for each job with job details and data.
  #   scriptFile: name of the script file.
  #
  # Returns:
  #   Returns an appended dataframe with a new column with the command line for each job.
  #
  # TODO:
  #   Look into updating this using dplyr (This entire function could be one line of code)

  df$submissionString <- NA
  for (ii in 1:nrow(df)) {
    df$submissionString[ii] <- paste("-v scriptFile=\"", scriptFile, ",", df$argument_string[ii], "\"", sep = "")
  }
  return(df)
}

### Create Submission dataframe
createSubmissionDataframe <- function(data) {
  # Checks consistency of data and creates a job-centred dataframe for submission.
  #
  # Args:
  #   data: Dataframe of each set of data. Each row contains a potentially unique set of data as well as run statistics (such as walltime, memory, etc).
  #
  # Returns:
  #   A dataframe with the necessary entries for each job submission.

  repeatColExists <- any(colnames(data) %in% "REPEAT")
  ncpusColExists <- any(colnames(data) %in% "NCPUS")
  dnrColExists <- any(colnames(data) %in% "DONOTRUN")
  # Remove all DONOTRUN rows
  if (dnrColExists)
    data <- data[data$DONOTRUN == TRUE,]
  # Determine number of submissions
  if (repeatColExists)
    nrows <- sum(data$REPEAT)
  else
    nrows <- nrow(data)

  # Create argument dataframe
  reserved.colnames <- c("JOBNAME", "MEMORY", "WALLTIME", "NCPUS", "REPEAT", "DONOTRUN", "seed")
  argument.names <- colnames(data)[!(colnames(data) %in% reserved.colnames)]
  parameter.names <- c("jobname", "memory", "walltime", "ncpus", argument.names, "seed", "argument_string")
  argumentDF <- data.frame(matrix(NA, ncol = length(parameter.names), nrow = nrows, dimnames = list(NULL, parameter.names)))

  # Add RNG Seed column
  argumentDF$seed <- ceiling(runif(nrows, 0, 10^8))

  # Iterate for each submission
  for (ii in 1:nrows) {
    # Define which row in data to pull from
    if (repeatColExists)
      jobRow <- which(cumsum(data$REPEAT) >= ii)[1]
    else
      jobRow <- ii
    # Define number of times to repeat job (Default = 1)
    if (repeatColExists)
      jobRepeats <- data$REPEAT[jobRow]
    else
      jobRepeats <- 1
    # Define jobname with unique ID if repeat is on (Default=1)
    if (repeatColExists)
      repCount <- ii - sum(data$REPEAT[0:(jobRow - 1)])
    if (repeatColExists)
      jobname <- paste(data$JOBNAME[jobRow], "_rep", repCount, sep = "")
    else
      jobname <- data$JOBNAME[jobRow]
    argumentDF$jobname[ii] <- as.vector(jobname) # Jobname
    # Translate values from data to argumentDF
    argumentDF$memory[ii] <- as.vector(data$MEMORY[jobRow]) # Memory
    argumentDF$walltime[ii] <- as.vector(data$WALLTIME[jobRow]) # Walltime
    if (ncpusColExists)
      argumentDF$ncpus[ii] <- as.vector(data$NCPUS[jobRow])
    else
      argumentDF$ncpus[ii] <- 1 # NCPUS
    argumentDF[ii, argument.names] <- as.vector(t(data[jobRow, argument.names])[, 1]) # User specified arguments

    # Construct argument string
    arguments <- argumentDF[ii, c("jobname", "seed", argument.names)]
    argumentDF$argument_string[ii] <- argumentString(arguments)
  }
  return(argumentDF)
}

### Create argument string
argumentString <- function(arguments) {
  # Construct the argument string for submission
  #
  # Args:
  #   arguments: Named vector of arguments to be passed to the R script.
  #
  # Returns:
  #   String of arguments in the form "argString=--args <arg Name>=<arg Value> ..."
  #
  # TODO:
  #   This could be summarised using paste(col.names, "=", col.values, sep="", collapse=" ")

  argumentString <- "argString=--args"
  col.names <- colnames(arguments)
  col.values <- arguments[1, ]
  for (ii in 1:ncol(arguments)) {
    argumentString <- paste(argumentString, " ", col.names[ii], "=", col.values[ii], sep = "")
  }
  return(argumentString)
}

### Check data structure
checkData <- function(data) {
  # Checks the consistency of the data to ensure it matches what the HPC queue expects.
  #
  # Args:
  #   data: Dataframe of each set of data. Each row contains a potentially unique set of data as well as run statistics (such as walltime, memory, etc
  #
  # Returns:
  #   Returns errors and warnings.
  #
  # TODO:
  #   Change the error and warning handling to use the logging methods.

  colnames <- colnames(data)
  jobnameColExists <- any(colnames %in% "JOBNAME")
  memoryColExists <- any(colnames %in% "MEMORY")
  walltimeColExists <- any(colnames %in% "WALLTIME")
  ncpusColExists <- any(colnames %in% "NCPUS")
  dnrColExists <- any(colnames %in% "DONOTRUN")
  repeatColExists <- any(colnames %in% "REPEAT")

  # Determine structure type
  if (is.data.frame(data))
    structure <- "data.frame"
  if (is.data.table(data))
    structure <- "data.table"

  # Initialise Error and Warning strings
  err_str <- character(0)
  warn_str <- character(0)

  # Check that JOBNAME, MEMORY and WALLTIME exist as columns in the data structure
  if (!((jobnameColExists) &&
        (memoryColExists) && (walltimeColExists))) {
    # Determine which columns are missing
    if (sum(c(!jobnameColExists, !memoryColExists, !walltimeColExists)) >
        1)
      plural <- "s "
    else
      plural <- " "
    if (!jobnameColExists)
      jobnameError <- "JOBNAME, "
    else
      jobnameError <- ""
    if (!memoryColExists)
      memoryError <- "MEMORY, "
    else
      memoryError <- ""
    if (!walltimeColExists)
      walltimeError <- "WALLTIME "
    else
      walltimeError <- ""
    # Send error
    missingColsErr <-paste("\nColumn", plural, jobnameError, memoryError, walltimeError, "not found in ", structure, sep = "")
    err_str <- paste(err_str, missingColsErr, sep = "\n\n")
  }

  ### Check that each jobname is unique
  dupJobNameWarning <- character(0)
  dupJobnames <- as.vector(data$JOBNAME[duplicated(data$JOBNAME)])
  print(dupJobnames)
  for (name in dupJobnames) {
    dupJobNameWarning <- paste(dupJobNameWarning, "\n Jobname ", name, " found in rows ", paste(which(data$JOBNAME %in% name), collapse = " "), sep = "")
  }
  if (length(dupJobNameWarning) != 0)
    dupJobNameWarning <- paste("Duplicated JOBNAMEs found", dupJobNameWarning)

  ### Check formatting of memory is correct
  invalidRowsMemory <- grep("^[0-9]+[m_g]b$", tolower(data$MEMORY), invert = TRUE)
  memoryError <- createErrorString(invalidRowsMemory, data$MEMORY, "MEMORY")
  if (length(memoryError) != 0)
    err_str <- paste(err_str, memoryError, sep = "\n\n")

  ### Check formatting of walltime is correct
  invalidRowsWalltime <- grep("^[0-9]{2}\\:[0-9]{2}\\:[0-9]{2}$|^([0-9]*\\.[0-9]+|[0-9]+)$", tolower(data$WALLTIME), invert = TRUE)
  walltimeError <- createErrorString(invalidRowsWalltime, data$WALLTIME, "WALLTIME")
  if (length(walltimeError) != 0)
    err_str <- paste(err_str, walltimeError, sep = "\n\n")

  ### Check formatting of NCPUS is correct
  if (ncpusColExists) {
    invalidRowsNCPUS <- grep("^[0-9]*$", tolower(data$NCPUS), invert = TRUE) # Invalid in floating point form
    ncpusError <- createErrorString(invalidRowsNCPUS, data$NCPUS, "NCPUS")
    if (length(ncpusError) != 0)
      err_str <- paste(err_str, ncpusError, sep = "\n\n")
  }

  ### Check formatting of DONOTRUN is correct
  if (dnrColExists) {
    invalidRowsDNR <- grep("^[TRUE_FALSE]+$", tolower(data$DONOTRUNJOB), invert = TRUE) # Invalid in floating point form
    dnrError <- createErrorString(invalidRowsDNR, data$DONOTRUNJOB, "DONOTRUN")
    if (length(dnrError) != 0)
      err_str <- paste(err_str, dnrError, sep = "\n\n")
  }

  ### Check formatting of REPEAT is correct
  if (dnrColExists) {
    invalidRowsREPEAT <- grep("^[TRUE_FALSE]+$", tolower(data$REPEAT), invert = TRUE) # Invalid in floating point form
    repeatError <- createErrorString(invalidRowsREPEAT, data$REPEAT, "REPEAT")
    if (length(repeatError) != 0)
      err_str <- paste(err_str, repeatError, sep = "\n\n")
  }

  ### Consolidate Errors and Warnings
  warn_str <- paste(dupJobNameWarning, sep = "\n")
  err_warn <- c(warn_str, err_str)
  return(err_warn)
}

### Create error string from invalid rows
createErrorString <- function(rows,data.vector,colName) {
  #
  #
  # Args:
  #   rows:
  #   data.vector:
  #   colName:
  #
  # Returns:
  #

  string <- character(0)
  for (row in rows) {
    string <- paste(string,"\nRow ",row,": ",data.vector[row],sep = "")
  }
  if (length(string) != 0)
    string <- paste("Invalid",colName,"fields found",string)

  return(string)
}
