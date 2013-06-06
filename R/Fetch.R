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
  packages <- c("mosaic","Fetch")
  
  ##look in packages
  if (first %in% packages) res <- fetchDat(name,first)
  else res <- fetchDat(name)
  if(!is.null(res)) return(res)
  
  base <- basename(name)
  last <- strsplit(base,'.',fixed=TRUE)[[1]]
  l <- length(last)
  ext <- tolower(last[l])
  
  ##look online
  if(l == 1) res <- fetchOnline(name,onlineDir)
  else res <- fetchOnline(name,onlineDir,ext)
  if(!is.null(res)){
    if(!is.logical(res)) return(res)
    else invisible(res)
  }
  
  ##look using remoteFetch
  oDir <- remoteFetch(name)
  if (!is.null(oDir)){
    if(l == 1) res <- fetchOnline(base,oDir)
    else res <- fetchOnline(base,oDir,ext)
    if(!is.null(res)){
      if(!is.logical(res)) return(res)
      else invisible(res)
    }
  }
  
  ##look in cwd
  switch(ext,
         "csv" = {res <- .read.csv(name)},
         "r" = {.source.file(path); if(!is.null(res)) message("File successfully sourced")},
         "rda" = {.loadWorkspace(url(path)); if(!is.null(res)) message("Workspace successfully loaded")},
         "rdata" = {.loadWorkspace(url(path)); if(!is.null(res)) message("Workspace successfully loaded")},
         "rmd" = {res <- .getText(url(path))},
         "html" = {res <- .getText(url(path))},
{stop("File extension not compatible. Please use one of the following file extensions: \n .csv, .r, .rda, .rdata, .rmd, .html")})
  if(!is.null(res)){
    if(!is.logical(res)) return(res)
    else invisible(res)
  }
  else stop("No file found.")
}

fetchDat <- function(name,pkg = NULL){
  if (is.null(pkg)){
    ##look in mosaic package
    newEnv = new.env()
    res <- try( suppressWarnings(data(name,package="mosaic", envir = newEnv)), silent=TRUE )
    if(length(ls(envir=newEnv)) > 0){
      data <- get(name,envir=newEnv)
      return(data)
    } 
    ##look in Fetch package
    res <- try( suppressWarnings(data(name,package="Fetch", envir = newEnv)), silent=TRUE )
    if(length(ls(envir=newEnv)) > 0){
      data <- get(name,envir=newEnv)
      return(data)
    }
  }
  else{
    ##look in specified package
    newEnv = new.env()
    res <- try( suppressWarnings(data(name,package=pkg, envir = newEnv)), silent=TRUE )
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

fetchOnline <- function(name,onlineDir,ext=NULL){
  if (!is.null(ext)){
    path <- paste(onlineDir,name,sep="")
    switch(ext,
           "csv" = {res <- .read.csv(path); return(res)},
           "r" = {.source.file(path); if(!is.null(res)) {message("File successfully sourced"); return(res)}},
           "rda" = {.loadWorkspace(url(path)); if(!is.null(res)) {message("Workspace successfully loaded"); return(res)}},
           "rdata" = {.loadWorkspace(url(path)); if(!is.null(res)) {message("Workspace successfully loaded"); return(res)}},
           "rmd" = {res <- .getText(url(path)); return(res)},
           "html" = {res <- .getText(url(path)); return(res)},
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
    res <- .loadWorkspace(url(path))
    if(!is.null(res)){
      message("Workspace successfully loaded")
      return(TRUE)
    }
    ##look for .rdata file
    path <- paste(onlineDir,name,'.rdata',sep="")
    res <- .loadWorkspace(url(path))
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
    res <- .loadWorkspace(url(path))
    if(!is.null(res)){
      message("Workspace successfully loaded")
      return(TRUE)
    }
    ##look for .Rdata file
    path <- paste(onlineDir,name,'.Rdata',sep="")
    res <- .loadWorkspace(url(path))
    if(!is.null(res)){
      message("Workspace successfully loaded")
      return(TRUE)
    }
    ##look for .Rmd file
    path <- paste(onlineDir,name,'.Rmd',sep="")
    res <- .getText(url(path))
    if(!is.null(res)) return(res)
    ##look for .rmd file
    path <- paste(onlineDir,name,'.rmd',sep="")
    res <- .getText(url(path))
    if(!is.null(res)) return(res)
    ##look for .RMD file
    path <- paste(onlineDir,name,'.RMD',sep="")
    res <- .getText(url(path))
    if(!is.null(res)) return(res)
    ##look for .html file
    path <- paste(onlineDir,name,'.html',sep="")
    res <- .getText(url(path))
    if(!is.null(res)) return(res)
    ##no file found
    return(NULL)
  }
}

remoteFetch <- function(fname){
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
    if (url.exists(name)) name <- getURLContent(name)
  }
  res <- try( suppressWarnings(source( name )), silent=TRUE )
  if( is.null(res) | class(res)=="try-error" ) return(NULL)
  else return(TRUE)
}

.loadWorkspace <- function(name){
  if (suppressWarnings((require(RCurl,quietly=TRUE)))){
    if (url.exists(name)){
      s <- getURLContent(name)
      name <- textConnection(s)
    }
  }
  res <- try( suppressWarnings(load( name , envir = .GlobalEnv)), silent=TRUE ) 
  if( is.null(res) | class(res)=="try-error" ) return(NULL)
  else return(TRUE)
}

.getText <- function(name){
  if (suppressWarnings((require(RCurl,quietly=TRUE)))){
    if (url.exists(name)){
      s <- getURLContent(name)
      if (attributes(x)$`Content-Type` == "text/html") name <- getURL(name)
      else name <- textConnection(s)
    }
  }
  res <- try( suppressWarnings(readChar( name , 10^9)), silent=TRUE )
  if( is.null(res) | class(res)=="try-error" ) return(NULL)
  else return(res)
}