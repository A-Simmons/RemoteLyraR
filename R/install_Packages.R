#' Get package source files and install to a personal library on the HPC file
#' server
#'
#' @export
#' @param credentials Vector of username and password (<Username>, <Password>).
#' REQUIRED
#' @param packages.toinstall Vector of packages to be installed. REQUIRED
#' @param local.dest Path to store the source files locally before moving to the
#' HPC file server.
#' @param remote.dest Path on the remote device for the Personal Library to install too.
#' @param host Address for the host device. (Default="lyra.qut.edu.au", QUT's HPC).
#' @param port Port to use for SSH. (Default = 22).
#'
#' @description
#' Automates a recursive search for package source files and their dependencies.
#' Using dependency information a specific install order is  prepared to
#' compensate for dependency trees. It will also attempt to identify when a
#' particular module is needed for installation. Once prepared the files are
#' copied to the HPC file server and installed to a personal library for future
#' use.
#'
#' @details
#'
#' @return
#'
getDependencies <-  function(credentials, packages.toinstall, local.dest, remote.dest, host = "lyra.qut.edu.au", port = 22) {
    # Main function for installing packages on the remote device.
    #
    # Args:
    #   credentials: Vector of the username and password c("<username>", "<password>").
    #   packages.toinstall: Vector of strings of the package names that need to be installed.
    #   local.dest: Path on the local device to store the package's source before copying to the remote device.
    #   remote.dest: Path on the remote device for the Personal Library to install too.
    #   host: Address for the host device. (Default="lyra.qut.edu.au", QUT's HPC).
    #   port: Port to use for SSH. (Default = 22).
    #
    # Returns:
    #
    # TODO:
    #   - Allow acceptance of dataframe for packages.toinstall to include specific version of package.
    #   - But in some flags to return.
    #   - Add option for quiet mode
    #   - Add option to force some operations (such as overwritting and remote folder creation)

    username = credentials[1]
    password = credentials[2]

    ### Initial Checks
    # Check Credentials
    flag <- checkConnection(username, password)
    if (flag) {
      stop("Connection refused. Check credentials.")
    }

    # Check Local Folder Pathing
    if (missing(local.dest)) {
      local.dest <- checkLocalDestination()
    } else {
      local.dest <- checkLocalDestination(local.dest)
    }

    # Check Remote Folder Pathing
    flag <- checkRemoteFolderFileExists(directory=remote.dest,username=username, password=password, host=host, port=port)
    if (flag) {
      warning(paste("Could not find remote directory", remote.dest))
    }

    # Check Remote Folder source Pathing
    flag <- checkRemoteFolderFileExists(directory=paste(remote.dest, "source", sep=""), username=username, password=password, host=host, port=port)
    if (flag) {
      warning(paste("Could not find remote directory for source files", paste(remote.dest, "source", sep="")))
    }

    # Make list of dependencies
    pack.df <- data.frame(package = packages.toinstall, rank = 0)
    for (package in packages.toinstall) {
      pack.df <- addPackagesToInstallList(package, pack.df, 1)
    }

    # Clean up and Download files
    pack.df <- dfCleanUp(pack.df, local.dest)

    # Print Summary Messages
    install.strings <- packageSummary(pack.df, remote.dest, quiet)

    # Copy files to remote.dest on HPC-FS
    source.files <- paste(local.dest, "*", sep="")
    scpToLyra(source.files,paste(remote.dest, "source", sep=""), username, password, host, port)

    # Run Install Scripts
    command <- paste(install.strings[1], install.strings[2], install.strings[3], sep=" && ")
    submitCommandToLyra(command, username, password, host, port)
  }

packageSummary <- function(pack.df, remote.dest, library.dest, quiet = TRUE) {
  # Generates commands needed to install the packages to be installed on the remote device.
  #
  # Args:
  #   pack.df: A dataframe that contians the package information to be installed.
  #   remote.dest: Path to source files on the remote device.
  #   library.dest: Path to the Remote Library on  the remote device.
  #   quiet: Quiet mode to not print install instructions
  #
  # Returns:
  #   Vector of strings needed to install the packages on the remote device.
  #     1. String to load required modules, including R
  #     2. String to change directory to the package source files
  #     3. String to install the packages

  ### Create Strings
  module.load.string <- getModulesToLoad(pack.df$package)  # Make module load string
  cd.string <- paste("cd", paste(remote.dest, "source", sep=""))  # CD to source location
  # Make Install Script String
  install.string <- paste("R CMD INSTALL --library=", remote.dest, " ", sep = "")
  for (file in pack.df$fileName) {
    install.string <- paste(install.string, file)
  }

  ### Print Summaries
  if (quiet == FALSE) {
    # Downloaded Packages
    cat(sprintf("\nList of downloaded packages:\n"))
    print(pack.df)
    # Module Load String
    cat(sprintf(
        "\nLoad the required modules with this line. Note, this line ...
        will be needed in your .pbs or .sub file to ensure all modules are loaded ...
        for code execution, not just installation!\n\n%s", module.load.string))
    # Install Script
    cat(sprintf(
        "\nInstall string. CD to the location you store the source ...
        files and add the location of your personal library.\n\n%s", install.string))
  }

  return(c(module.load.string, cd.string, install.string))
  }

dfCleanUp <- function(pack.df, local.dest) {
  # Cleans up the package dataframe. Removing packages already installed on Lyra and ordering by descending rank. Finally, it downloads the source files
  #
  # Args:
  #   pack.df: The package dataframe
  #   local.dest: Local destination to download the source files.
  #
  # Returns:
  #   The package dataframe with a new column of the file names of the source files.

  # Remove any already on Lyra
  pack.df <- removeInstalledPackages(pack.df)
  # Order descending by rank
  pack.df <- pack.df[with(pack.df, order(-rank)), ]
  # Download Files and append a filename column
  pack.df$fileName <- NA
  pack.df <- downloadSourceFile(pack.df, dest = local.dest)
  return(pack.df)
}

checkLocalDestination <- function(dest) {
  # Checks the consistency of the local path to source files. If non are submitted a default is created. Checks if the location exists, if not it creates the directory.
  #
  # Args:
  #   dest: The path to the folder
  #
  # Returns:
  #   Returns the path to the folder

  # Set defaults for dest and check consistency
  if (missing(dest)) {
    if (.Platform$OS.type == "windows") {
      dest = "C:/R_Library_Source_Files/"
    } else {
      dest = "~/R_Library_Source_Files/"
    }
  } else {
    # Ensure that the last character is /
    if (grep("[^/]{1}$", dest)) {
      dest <- paste(dest, "/", sep = "")
    }
  }

  # Check if folder exists, create if not (The end / gives a FALSE so it is removed)
  if (!file.exists(gsub("/{1}$","",dest))) {
    dir.create(dest)
  }
  return(dest)
}

addPackagesToInstallList <- function(package, pack.df, rank) {
  # Recursive function to add packages to the install list. Pocess works as such:
  #   1. Determine which packages are in the Import or Depends list from the CRAN webpage and are not already in pack.df
  #   2. Give the new packages a rank to designate the need to be earlier in the install order.
  #   3. Call this function on all new packages that added to the list.
  #
  # Args:
  #   package: The name of the package to search for dependencies
  #   pack.df: Data.frame of packages which are to be installed
  #   rank: Install order rank. Higher values indicate a lower position in the dependency tree (as such, need to be installed earlier)
  #
  # Returns:
  #   The appended package data.frame, pack.df
  #
  # TODO:
  #   Currently, if a package already exists in the list nothing happens. Instead, it's rank should be updated to match the fact it may need to earlier in the install order for another package.

  # Get list of missing dependencies
  Dependencies <- dependenciesMissing(package, pack.df)

  if (length(Dependencies) > 0) {
    Dependencies <-
      data.frame(package = Dependencies[!(Dependencies %in% pack.df$package)],rank =
                   rank)
    # Append list of missing dependencies
    pack.df <- rbind(pack.df, Dependencies)
    # Search these dependencies for their own missing dependencies
    for (packageD in Dependencies$package) {
      pack.df <- addPackagesToInstallList(packageD, pack.df, rank + 1)
    }

  }
  return(pack.df)
}

downloadSourceFile <-  function(pack.df, dest, repository="https://cran.r-project.org/web/packages/", repository.dl="https://cran.r-project.org/src/contrib/") {
  # Downloads source files from packages listed in pack.df
  #
  # Args:
  #   pack.df: Data.frame of packages which are to be installed
  #   dest: Local PATH to store source files in
  #   repository: URL to pull the package index page
  #   repository.dl: URL to download the packages from
  #
  # Returns:
  #   The updated package data.frame, pack.df

    for (count in (1:nrow(pack.df))) {
      package <- pack.df[count, "package"]
      IsException <- exceptionList(package, dest) # Check if an exception for a certain package exists
      if (IsException != FALSE) {
        pack.df[count, "fileName"] <- IsException
      } else {
        ### GET FILE NAME
        thepage = readLines(paste(repository, package, '/index.html', sep = "")) # Read HTML for package index page
        index <- grep("Package&nbsp;source:", thepage) + 1
        filename <- gsub(" <.*$", "", gsub("^.*\"> ", "", thepage[index])) # Get filename of file
        pack.df[count, "fileName"] <- filename
        ### DOWNLOAD FILE
        download.file(paste(repository.dl, filename, sep = ""), dest = paste(dest, filename, sep = ""))
      }
    }
    return(pack.df)
  }

getDependsAndImports <-  function(package, type, repository = "https://cran.r-project.org/web/packages/") {
   # Get the Depends or Imports packages from the Index page of a package.
   #
   # Args:
   #   package: The name of the package to check for dependencies
   #   type: 'Depends' or 'Imports' to determine dependency type to gather
   #   repository: URL to pull the package index page
   #
   # Returns:
   #   Vector of package names which are of type <type> dependency to <package>

    # Retrieve HTML from pakcage index page
    thepage = readLines(paste(repository, package, '/index.html', sep = ""))
    ### Check page has desired content
    if (length(grep(paste("<h2>", package, sep=""), thepage)) == 0)
      stop(paste("Package ", package," not found on repository with URL: ", repository,package, '/index.html', sep=""))

    ### GET Imports/Depends
    index <- grep(type, thepage) + 1
    if (length(index) > 0) {
      package.imports <- gsub("^.*>", "", strsplit(thepage[index], "</a")[[1]])
      package.imports <- package.imports[nchar(package.imports) > 0]
    } else {
      package.imports = c()
    }
    return(package.imports)
  }

dependenciesMissing <- function(package, pack.df) {
  # Creates a vector of dependencies that are needed by a package and missing from pack.df
  #
  # Args:
  #   package: The name of the package to check for dependencies
  #   pack.df: The data.frame of packages which are to be installed
  #
  # Returns:
  #   Vector of package names

  Imports <- getDependsAndImports(package, "Imports")
  Depends <- getDependsAndImports(package, "Depends")
  Dependencies <- c(Imports, Depends)
  Dependencies <- Dependencies[!(Dependencies %in% pack.df$package)]
  return(Dependencies)
}

exceptionList <- function(package,dest) {
  # Determines packages that are in an exception list and performs an array of functions such as downloading from specific sources, unzipping, etc
  #
  # Args:
  #   package: The name of the package to check for dependencies
  #   dest: Local PATH to install the source files
  #
  # Returns:
  #   Filename of the package source file/folder

  # Load Exception List
  exception.df <- read.csv(   "https://raw.githubusercontent.com/A-Simmons/LyraR_Package_Install/master/LyraR_Package_Exception_List.csv", header = TRUE, stringsAsFactors = FALSE)

  if (package %in% exception.df$packages) {
    package.details <- exception.df[exception.df$packages == package, ]
    fileName <- package.details$fileName

    download.file(as.character(package.details$URL), dest = paste(dest, fileName, sep = ""))
    # If file needs unzipping
    if (package.details$unzip == TRUE) {
      print(paste("Unzipping", fileName))
      unzip(paste(dest, package.details$fileName, sep = ""), exdir = gsub("/{1}$", "", dest))
      print(paste("Cleaning up", fileName))
      file.remove(paste(dest, package.details$fileName, sep = ""))
      fileName <- gsub("\\..*$", "", fileName)
    }

    return(as.character(fileName))
  } else {
    return(FALSE)
  }
}

getModulesToLoad <- function(packages) {
  # Determines if a package needs a specific module on the HPC to be loaded.
  #
  # Args:
  #   packages: List of package names
  #
  # Returns:
  #   String command to load the required modules, including R itself

  exception.df <- read.csv(      "https://raw.githubusercontent.com/A-Simmons/LyraR_Package_Install/master/LyraR_Package_Exception_List.csv",header = TRUE, stringsAsFactors = FALSE)
  string <- "module load R/3.2.4_gcc"
  for (package in packages) {
    string <- paste(string, exception.df[exception.df$packages %in% package, "module"])
  }
  return(gsub(" +", " ", string))
}

removeInstalledPackages <- function(pack.df) {
  # Determines if packages in the install list are already on the HPC and removes them from the list.
  #
  # Args:
  #   pack.df: Data.frame of the packages to be installed
  #
  # Returns:
  #   Updated data.frame of packages to be installed

  installed.packages <- read.csv(      "https://raw.githubusercontent.com/A-Simmons/LyraR_Package_Install/master/Lyra_Installed_Packages.csv", header = TRUE, stringsAsFactors = FALSE)
  packages <- pack.df$package
  pack.df <- pack.df[!(packages %in% installed.packages$Package), ]
  return(pack.df)
}
