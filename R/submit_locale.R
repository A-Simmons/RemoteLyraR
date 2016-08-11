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
#'  be affixed an identifier. Whilith a letter (Violation to this will produce an error). \emph{REQUIRED}.}
#'  \item{"MEMORY"}{ The memory required for the job. Example: "100mb" or "2gb" requests 100 megabytes or
#'  2 gigabytes respectively. \emph{REQUIRED}.}
#'  \item{"WALLTIME"}{ The time requested for the job on the HPC in the form "<hours>:<mins>:<secs>".
#'  Example: "1:30:00" will request 1hour and 30minutes. A single number will be converted to hours and
#'  minutes, for example "1.5" wilith a letter (Violation to this will produce an error). \emph{REQUIRED}.}
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
submitLocal <- function(credentials, local.folder, script.file, submission.file, data, remote.folder, quiet=FALSE, host="lyra.qut.edu.au", port=22, ignore.warning=FALSE) {

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

  # Check Local fodler exists

  # Check that script file exists in LOCAL folder
  flag <- checkRemoteFolderFileExists(directory=remote.folder, file=script.file, username=username, password=password, host=host, port=port)
  if (flag) {
    stop(paste("Could not find script file", script.file))
  }

  # Check that submission.file is in LOCAL directory
  flag <- checkRemoteFolderFileExists(directory=remote.folder, file=submission.file, username=username, password=password, host=host, port=port)
  if (flag) {
    stop(paste("Could not find script file", submission.file))
  }

  # Check that remote folder exists
  flag <- checkRemoteFolderFileExists(directory=remote.folder, username=username, password=password, host=host, port=port)
  if (flag) {
    stop("Could not find remote folder.")
  } else { # If not, create remote folder

  }

  ## All data and files are now in a remote State, submit to Lyra using submiteRemote.

}


#' Submit Job to HPC (Remotely Stored Project)
#'
#' @export
#' @param directory The Path to the folder to search recursively
tree <- function(directory) {
  mem_index <- "V5"
  name_index <- "V9"

  dt <- data.frame()
  dt <- tree.recursive(dt, directory)

  dt <- dt[, c(mem_index,name_index,"directory")]
  print(dt)
  dt <- tree.sort(dt)
  tree.build.graphic(dt)
  return(dt)
}

tree.sort <-function(dt) {
  dt$full <- paste(dt$directory,dt$V9,sep="/")
  dt <- dt[with(dt, order(full)), ]
  return(dt)
}

tree.build.graphic <- function(dt) {
  requireNamespace("stringr")
  T_char <- "\U251C"
  T_vline <- "\U2502"
  T_hline <- "\U2500"
  C_char <- "\U2514"

  # Add depth count to dt
  dt$depth <- str_count(dt$directory,"/")
  dt$depth <- dt$depth - min(str_count(dt$directory,"/"))
  dt$printed <- FALSE
  # Various options needed
  max.depth <- max(dt$depth) # How deep the folders go
  mem.nchar <- c(rep(NA,max.depth)) # Number of characters used to represent memory at each depth
  for (depth in 1:max.depth) {
    mem.nchar[depth] <- max(nchar(dt[dt$depth==1,'V5']))
  }

  for (index in 1:nrow(dt)) {
    string <- ""
    parent.dir <- dt[index,'directory']
    if (dt[index,'depth']>0)
    for (i in 1:dt[index,'depth']) {
      parent.dir <- str_split(parent.dir,'/')
      parent.dir <- paste(parent.dir[[1]][-length(parent.dir[[1]])],collapse="/")

      # See if this directory exists in any lower elements
      num.with.parent = length(grep(parent.dir,dt[index:nrow(dt),'directory']))
      num.with.direc = length(grep(dt[index,'directory'],dt[index:nrow(dt),'directory']))
      if (num.with.parent > num.with.direc) {
            string <- paste(T_vline," "," "," ",string,sep="")
      } else {
          string <- paste(" "," "," "," ",string,sep="")
      }

    }

    if (dt[index,'V9']==tail( dt[ dt[,'directory']==dt[index,'directory'],'V9'], n=1 )) {
      S_char = C_char
    } else {
      S_char = T_char
    }
    string <- paste(string,S_char,T_hline,T_hline," ",sep="")
    string <- paste(string,"[",dt$V5[index],"]   ",dt$V9[index],sep="")
    print(string)
  }
}

tree.recursive <- function(dt,directory) {

  command <- paste("ls -lh",directory)
  ls <- system(command, intern = TRUE)

  #Get number of files
  files <- gsub("total ","",ls[1])

  if (files > 0) {
    #print('----COMAND AND LS-----')
    #print(command)
    #print(ls)
    # Get rid of excess whitespace
    ls <- gsub("[[:space:]]+"," ",ls)
    # Read into table
    dt.new <- read.table(text=ls[-1],sep=" ",colClasses="character")
    dt.new$directory <- directory

    folders.index <- grep("^d.*",ls[-1])

    for (index in folders.index) {
      directory.new <- paste(directory,dt.new$V9[index],sep="/")
      dt.new <- tree.recursive(dt.new,directory.new)
    }

    dt <- rbind(dt,dt.new)
  }
  print(directory)
  print(dt)
  return(dt)

}
