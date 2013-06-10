#' An easy way for students to load data sets, source r files, and load r workspaces
#'
#' Description of \code{fetch}
#'
#' @param name a character string naming a data set.
#' This will often end in \code{.csv} for reading in a data set.
#' If it ends in \code{.r} or \code{.R}, the file will be "sourced".
#' An extension is not required, however.
#' If no name is specified the user file.choose() interface will be opened for the
#' user to choose his/her file.
#' @param onlineDir a character string specifying a website. 
#' \code{fetch} will search for the file on this website.
#'
#' @details
#' details go here
#'
#' @return a data frame.
#'
#' @export
#' @examples
#' \dontrun{dome <- fetchData("Dome.csv")}
#' \dontrun{carbon <- fetchData("CO2")}
#' \dontrun{fetchData(show=TRUE)}
#' \dontrun{fetchData(add.to.path="http://www.macalester.edu/~kaplan/ISM/datasets/")}
#' \dontrun{fetchData(drop.from.path="http://www.macalester.edu/~kaplan/ISM/datasets/") }
#' \dontrun{dome <- fetchData("Dome.csv", cache=TRUE)}
#' @keywords util
#' 

fetch <- function(name = NULL,onlineDir = "http://www.mosaic-web.org/go/datasets/"){
  if(is.null(name)){
    fname = file.choose()
    base <- basename(fname)
    last <- strsplit(base,'.',fixed=TRUE)[[1]]
    ext <- tolower(last[length(last)])
    ##Find file
    switch(ext,
           "csv" = {res <- .read.csv(fname); return(res)},
           "r" = {.source.file(fname); message("File successfully sourced")},
           "rda" = {.loadWorkspace(fname); message("Workspace successfully loaded")},
           "rdata" = {.loadWorkspace(fname); message("Workspace successfully loaded")},
           "rmd" = {res <- .getText(fname); return(res)},
{stop("File extension not compatible. Please use one of the following file extensions: \n .csv, .r, .rda, .rdata, .rmd")})
  }
  first <- strsplit(name,'/',fixed=TRUE)[[1]][1]
  base <- basename(name)
  packages <- c("mosaic","Fetch")
  ##look in packages
  if (first %in% packages) res <- fetchDat(base,first)
  else res <- fetchDat(name)
  if(!is.null(res)) return(res)
  last <- strsplit(base,'.',fixed=TRUE)[[1]]
  l <- length(last)
  ext <- tolower(last[l])
  
  ##look online
  if(l == 1) res <- .fetchOnline(name,onlineDir)
  else res <- .fetchOnline(name,onlineDir,ext)
  if(!is.null(res)){
    if(!is.logical(res)) return(res)
    else return(invisible(res))
  }
  
  ##look using .remoteFetch
  oDir <- .remoteFetch(name)
  if (!is.null(oDir)){
    if(l == 1) res <- .fetchOnline(base,oDir)
    else res <- .fetchOnline(base,oDir,ext)
    if(!is.null(res)){
      if(!is.logical(res)) return(res)
      else return(invisible(res))
    }
  }
  
  ##look in cwd
  switch(ext,
         "csv" = {res <- .read.csv(name)},
         "r" = {res <- .source.file(path); if(!is.null(res)) message("File successfully sourced")},
         "rda" = {res <- .loadWorkspace(path); if(!is.null(res)) message("Workspace successfully loaded")},
         "rdata" = {res <- .loadWorkspace(path); if(!is.null(res)) message("Workspace successfully loaded")},
         "rmd" = {res <- .getText(path)},
         "html" = {res <- .getText(path)},
{stop("File extension not compatible. Please use one of the following file extensions: \n .csv, .r, .rda, .rdata, .rmd, .html")})
  if(!is.null(res)){
    if(!is.logical(res)) return(res)
    else return(invisible(res))
  }
  else stop("No file found.")
}

fetchDat <- function(name,pkg = NULL){
  if (is.null(pkg)){
    ##look in mosaic package
    newEnv = new.env()
    try( suppressWarnings(data(list = name,package="mosaic", envir = newEnv)), silent=TRUE )
    if(length(ls(envir=newEnv)) > 0){
      data <- get(name,envir=newEnv)
      return(data)
    } 
    ##look in Fetch package
    try( suppressWarnings(data(list = name,package="Fetch", envir = newEnv)), silent=TRUE )
    if(length(ls(envir=newEnv)) > 0){
      data <- get(name,envir=newEnv)
      return(data)
    }
  }
  else{
    ##look in specified package
    newEnv = new.env()
    res <- try( suppressWarnings(data(list = name,package=pkg, envir = newEnv)), silent=TRUE )
    if(length(ls(envir=newEnv)) > 0){
      data <- get(name,envir=newEnv)
      return(data)
    }
  }
  ##try removing file extension, if one exists
  names <- strsplit(name,'.',fixed=TRUE)[[1]]
  l = length(names)
  if (l > 1){
    return(fetchDat(paste(names[-l],collapse = ""),pkg))
  }
  return(NULL)
}

.fetchOnline <- function(name,onlineDir,ext=NULL){
  if (!is.null(ext)){
    path <- paste(onlineDir,name,sep="")
    switch(ext,
           "csv" = {res <- .read.csv(path); return(res)},
           "r" = {res <- .source.file(path); if(!is.null(res)) {message("File successfully sourced"); return(res)}},
           "rda" = {res <- .loadWorkspace(path); if(!is.null(res)) {message("Workspace successfully loaded"); return(res)}},
           "rdata" = {res <- .loadWorkspace(path); if(!is.null(res)) {message("Workspace successfully loaded"); return(res)}},
           "rmd" = {res <- .getText(path); return(res)},
           "html" = {res <- .getText(path); return(res)},
{stop("File extension not compatible. Please use one of the following file extensions: \n .csv, .r, .rda, .rdata, .rmd, .html")})
  }
  else{
    ##look for .csv file
    path <- paste(onlineDir,name,'.csv',sep="")
    res <- .read.csv(path)
    if(!is.null(res)) return(res)
    ##look for .r file
    path <- paste(onlineDir,name,'.r',sep="")
    res <- .source.file(path)
    if(!is.null(res)){
      message("File successfully sourced")
      return(TRUE)
    }
    ##look for .rda file
    path <- paste(onlineDir,name,'.rda',sep="")
    res <- .loadWorkspace(path)
    if(!is.null(res)){
      message("Workspace successfully loaded")
      return(TRUE)
    }
    ##look for .rdata file
    path <- paste(onlineDir,name,'.rdata',sep="")
    res <- .loadWorkspace(path)
    if(!is.null(res)){
      message("Workspace successfully loaded")
      return(TRUE)
    }
    ##look for .CSV file
    path <- paste(onlineDir,name,'.CSV',sep="")
    res <- .read.csv(path)
    if(!is.null(res)) return(res)
    ##look for .R file
    path <- paste(onlineDir,name,'.R',sep="")
    res <- .source.file(path)
    if(!is.null(res)){
      message("File successfully sourced")
      return(TRUE)
    }
    ##look for .RDA file
    path <- paste(onlineDir,name,'.RDA',sep="")
    res <- .loadWorkspace(path)
    if(!is.null(res)){
      message("Workspace successfully loaded")
      return(TRUE)
    }
    ##look for .Rdata file
    path <- paste(onlineDir,name,'.Rdata',sep="")
    res <- .loadWorkspace(path)
    if(!is.null(res)){
      message("Workspace successfully loaded")
      return(TRUE)
    }
    ##look for .Rmd file
    path <- paste(onlineDir,name,'.Rmd',sep="")
    res <- .getText(path)
    if(!is.null(res)) return(res)
    ##look for .rmd file
    path <- paste(onlineDir,name,'.rmd',sep="")
    res <- .getText(path)
    if(!is.null(res)) return(res)
    ##look for .RMD file
    path <- paste(onlineDir,name,'.RMD',sep="")
    res <- .getText(path)
    if(!is.null(res)) return(res)
    ##look for .html file
    path <- paste(onlineDir,name,'.html',sep="")
    res <- .getText(path)
    if(!is.null(res)) return(res)
    ##no file found
    return(NULL)
  }
}

.remoteFetch <- function(fname){
  rootName <- 'http://www.mosaic-web.org/go/Redirects/mosaic-fetch-redirect.csv'
  components <- strsplit(fname,'/',fixed=TRUE)
  who <- components[[1]][1] # which redirect site
  restOfName <- components[[1]][-1] # address within that site
  redirects <- read.csv(rootName,stringsAsFactors=FALSE)
  sites <- subset(redirects, Site==who)
  #if (nrow(sites)==0) stop("No redirect site set up.")
  if(nrow(sites) > 0){
    redirectName <- sites$Directory[1]
    #    fileName <- paste(redirectName,
    #                    paste(restOfName,collapse='/'),
    #                    sep="")
    #    return(fileName)
    return(redirectName)
    
  }
  else return(NULL)
}

.read.csv <- function(name) {
  if (suppressWarnings((require(RCurl,quietly=TRUE)))){
    if (url.exists(name)){
      s <- getURLContent(name)
      name <- textConnection(s)
    }
  }
  res <- try( suppressWarnings(read.csv( name )), silent=TRUE )
  if( is.null(res) | class(res)=="try-error" ) return(NULL)
  else return(res)
}

.source.file <- function(name){
  if (suppressWarnings((require(RCurl,quietly=TRUE)))){
    if (url.exists(name)){
      s <- getURL(name)
      name <- textConnection(s)
    } 
  }
  res <- try( suppressWarnings(source( name )), silent=TRUE )
  if( is.null(res) | class(res)=="try-error" ) return(NULL)
  else return(TRUE)
}

.loadWorkspace <- function(name){
  if (suppressWarnings((require(RCurl,quietly=TRUE)))){
    if (url.exists(name)){
      s <- getBinaryURL(name)
      name <- rawConnection(s)
    }
  }
  else name <- url(name)
  res <- try( suppressWarnings(load( name , envir = .GlobalEnv)), silent=TRUE ) 
  if( is.null(res) | class(res)=="try-error" ) return(NULL)
  else return(TRUE)
}

####  Change
.getText <- function(name){
  if (suppressWarnings((require(RCurl,quietly=TRUE)))){
    if (url.exists(name)){
      s <- getBinaryURL(name)
      name <- rawConnection(s)
    }
  }
  else name <- url(name)
  res <- try( suppressWarnings(readChar( name , 10^6)), silent=TRUE )
  if( is.null(res) | class(res)=="try-error" ) return(NULL)
  else return(res)
}