#' Get package source files and install to a personal library on the HPC file
#' server
#'
#' @export
#' @param credentials Vector of username and password (<Username>, <Password>).
#' REQUIRED
#' @param packages.install Vector of packages to be installed. REQUIRED
#' @param remote.library Path to personal library on the HPC file server.
#' @param local.dest Path to store the source files locally before moving to the
#' HPC file server.
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
getDependencies <-
  function(credentials,packages.install,local.dest,remote.dest,host = "lyra.qut.edu.au", port = 22) {
    username = credentials[1]; password = credentials[2];

    ### Initial Checks
    # Check Credentials
    flag <- checkConnection(username,password)
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
    flag <- checkRemoteFolderFileExists(directory=remote.dest,username=username,password=password,host=host,port=port)
    if (flag) {
      warning(paste("Could not find remote directory",remote.dest))
    }

    # Check Remote Folder source Pathing
    flag <- checkRemoteFolderFileExists(directory=paste(remote.dest,"source",sep=""),username=username,password=password,host=host,port=port)
    if (flag) {
      warning(paste("Could not find remote directory for source files",paste(remote.dest,"source",sep="")))
    }

    # Make list of dependencies
    pack.df <- data.frame(package = packages.toinstall,rank = 0)
    for (package in packages.toinstall) {
      pack.df <- addPackagesToInstallList(package,pack.df,1)
    }

    # Clean up and Download files
    pack.df <- dfCleanUp(pack.df,local.dest)

    # Print Summary Messages
    install.strings <- packageSummary(pack.df,remote.dest,quiet)

    # Copy files to remote.dest on HPC-FS
    source.files <- paste(local.dest,"*",sep="")
    scpToLyra(source.files,paste(remote.dest,"source",sep=""),username,password,host,port)

    # Run Install Scripts
    command <- paste(install.strings[1],install.strings[2],sep=" && ")
    submitCommandToLyra(command,username,password,host,port)
  }

packageSummary <- function(pack.df,remote.dest,library.dest,quiet = TRUE) {
  ### Create Strings
  # Make module load string
  module.load.string <- getModulesToLoad(pack.df$package)
  # CD to source location
  cd.string <- paste("cd",paste(remote.dest,"source",sep=""))
  # Make Install Script String
  install.string <-
    paste("R CMD INSTALL --library=",remote.dest," ",sep = "")
  for (file in pack.df$fileName) {
    install.string <- paste(install.string,file)
  }

  ### Print Summaries
  if (quiet == FALSE) {
    # Downloaded Packages
    cat(sprintf("\nList of downloaded packages:\n"))
    print(pack.df)
    # Module Load String
    cat(
      sprintf(
        "\nLoad the required modules with this line. Note, this line ...
        will be needed in your .pbs or .sub file to ensure all modules are loaded ...
        for code execution, not just installation!\n\n%s",module.load.string
      )
      )
    # Install Script
    cat(
      sprintf(
        "\nInstall string. CD to the location you store the source ...
        files and add the location of your personal library.\n\n%s",install.string
      )
      )
  }

  return(c(module.load.string,cd.string,install.string))
  }

dfCleanUp <- function(pack.df,local.dest) {
  # Remove any already on Lyra
  pack.df <- removeInstalledPackages(pack.df)
  # Order descending by rank
  pack.df <- pack.df[with(pack.df, order(-rank)),]
  # Download Files and append a filename column
  pack.df$fileName <- NA
  pack.df <- downloadSourceFile(pack.df,dest = local.dest)
  return(pack.df)
}

checkLocalDestination <- function(dest) {
  # Set defaults for dest and check consistency
  if (missing(dest)) {
    if (.Platform$OS.type == "windows") {
      dest = "C:/R_Library_Source_Files/"
    } else {
      dest = "~/R_Library_Source_Files/"
    }
  } else {
    # Ensure that the last character is /
    if (grep("[^/]{1}$",dest)) {
      dest <- paste(dest,"/",sep = "")
    }
  }

  # Check if folder exists, create if not (The end / gives a FALSE so it is removed)
  if (!file.exists(gsub(".{1}$","",dest))) {
    dir.create(dest)
  }

  return(dest)
}

addPackagesToInstallList <- function(package,pack.df,rank) {
  # Get list of missing dependencies
  Dependencies <- dependenciesMissing(package,pack.df)

  if (length(Dependencies) > 0) {
    Dependencies <-
      data.frame(package = Dependencies[!(Dependencies %in% pack.df$package)],rank =
                   rank)
    # Append list of missing dependencies
    pack.df <- rbind(pack.df,Dependencies)
    # Search these dependencies for their own missing dependencies
    for (packageD in Dependencies$package) {
      pack.df <- addPackagesToInstallList(packageD,pack.df,rank + 1)
    }

  }
  return(pack.df)
}

downloadSourceFile <-
  function(pack.df,dest,repository = "https://cran.r-project.org/web/packages/",repository.dl =
             "https://cran.r-project.org/src/contrib/") {
    for (count in (1:nrow(pack.df))) {
      package <- pack.df[count,"package"]
      IsException <- exceptionList(package,dest)
      if (IsException != FALSE) {
        pack.df[count,"fileName"] <- IsException
      } else {
        ### GET FILE NAME
        thepage = readLines(paste(repository,package,'/index.html',sep = ""))
        index <- grep("Package&nbsp;source:",thepage) + 1
        filename <- gsub(" <.*$","",gsub("^.*\"> ","",thepage[index]))
        pack.df[count,"fileName"] <- filename
        ### DOWNLOAD FILE
        download.file(paste(repository.dl,filename,sep = ""),dest = paste(dest,filename,sep =
                                                                            ""))
      }
    }
    return(pack.df)
  }

getDependsAndImports <-
  function(package,type,repository = "https://cran.r-project.org/web/packages/",repository.dl =
             "https://cran.r-project.org/src/contrib/") {
    thepage = readLines(paste(repository,package,'/index.html',sep = ""))

    ### GET Imports/Depends
    index <- grep(type,thepage) + 1
    if (length(index) > 0) {
      package.imports <- gsub("^.*>","",strsplit(thepage[index],"</a")[[1]])
      package.imports <- package.imports[nchar(package.imports) > 0]
    } else {
      package.imports = c()
    }
    return(package.imports)
  }

dependenciesMissing <- function(package,pack.df) {
  Imports <- getDependsAndImports(package,"Imports")
  Depends <- getDependsAndImports(package,"Depends")
  Dependencies <- c(Imports,Depends)
  Dependencies <- Dependencies[!(Dependencies %in% pack.df$package)]
}

exceptionList <- function(package,dest) {
  # Load Exception List
  exception.df <-
    read.csv(
      "https://raw.githubusercontent.com/A-Simmons/LyraR_Package_Install/master/LyraR_Package_Exception_List.csv",header =
        TRUE,stringsAsFactors = FALSE
    )

  if (package %in% exception.df$packages) {
    package.details <- exception.df[exception.df$packages == package,]
    fileName <- package.details$fileName

    download.file(as.character(package.details$URL),dest = paste(dest,fileName,sep =
                                                                   ""))
    # If file needs unzipping
    if (package.details$unzip == TRUE) {
      print(paste("Unzipping",fileName))
      unzip(paste(dest,package.details$fileName,sep = ""),exdir = gsub("/{1}$","",dest))
      print(paste("Cleaning up",fileName))
      file.remove(paste(dest,package.details$fileName,sep = ""))
      fileName <- gsub("\\..*$","",fileName)
    }

    return(as.character(fileName))
  } else {
    return(FALSE)
  }
}

getModulesToLoad <- function(packages) {
  exception.df <-
    read.csv(
      "https://raw.githubusercontent.com/A-Simmons/LyraR_Package_Install/master/LyraR_Package_Exception_List.csv",header =
        TRUE,stringsAsFactors = FALSE
    )
  string <- "module load R/3.2.4_gcc"
  for (package in packages) {
    string <-
      paste(string,exception.df[exception.df$packages %in% package,"module"])
  }
  return(gsub(" +"," ",string))
}

removeInstalledPackages <- function(pack.df) {
  installed.packages <-
    read.csv(
      "https://raw.githubusercontent.com/A-Simmons/LyraR_Package_Install/master/Lyra_Installed_Packages.csv",header =
        TRUE,stringsAsFactors = FALSE
    )
  packages <- pack.df$package
  pack.df <- pack.df[!(packages %in% installed.packages$Package),]
  return(pack.df)
}
