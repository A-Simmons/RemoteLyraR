#' Submit Job to HPC (Remotely Stored Project)
#'
#' @export
#' @param credentials Vector of username and password (<Username>, <Password>). REQUIRED
#' @param remote.folder Root directory for the project stored on the remote file server.REQUIRED
#' @param script.file R file to be called. REQUIRED
#' @param data ADD SOON. REQUIRED
#' @param quiet Turns on quiet mode, disabling all messages except warnings and errors (default: FALSE)
#' @param host Host name for the remote server (default: lyra.qut.edu.au)
#' @param port Port number to be used for SSH and SCP to the host (defualt: 22)
#' @param submission.file ADD SOON
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

submitRemote <- function(credentials, remote.folder, script.file, data, quiet=FALSE, host="lyra.qut.edu.au", port=22, submission.file) {

  username = credentials[1]; password = credentials[2];

  # Check that server can be accessed
  checkConnection(username,password)

  # Check that remote folder exists
  checkFolderExists(remote.folder,username,password,host=host,port=port)

  # Check that script file exists in remote folder
  checkScriptFileExists(remote.folder,script_file,username,password,host,port)

}




