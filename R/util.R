
## ************************************************************************
## 
## 
## 
## (c) Xiaobei Zhao
## 
## Thu Jan 09 07:52:27 EST 2014 -0500 (Week 01)
## 
## 
## Reference: 
## 
## 
## ************************************************************************


## ------------------------------------------------------------------------
## print and log
## ------------------------------------------------------------------------

##' Print the name and the content of an R object
##'
##' 
##' @title Print the name and the content of an R object
##' @param x ANY, an R object.
##' @param prefix the prefix to print.
##' @param envir the \code{environment} to use.
##' @return NULL
##' @author Xiaobei Zhao
##' @examples
##' ## print an object
##' x1 <- 1:6
##' printme(x1)
##' 
##' ## print with a prefix
##' foo <- function(x,envir=sys.frame(sys.parent(0))){
##'   printme(x,match.call(),envir=envir)
##'   invisible()
##' }
##' foo(1:6)
##' 
##' @seealso \code{\link{logme}}
printme <- function(x=NULL,prefix=NULL,envir=sys.frame(sys.parent(0)))
{
  if(is.call(prefix)){
    prefix <- prefix[[1]]
  }
  if (!missing(x)){
    .xname <- atos(x,envir=envir)
  }
  if(!length(prefix)){
    prefix <- NULL
  } else{
    prefix <- as.character(prefix)
  }
    if (is.null(prefix)){
      cat(sprintf("\n## "))
    } else {
      cat(sprintf("\n## %s | ",prefix))
    }
    if (!missing(x)){
      cat(sprintf("%s",.xname))
    }
  cat(sprintf(" ##\n"))
  if (!missing(x)){
    if (!is.character(x)) {
      print(x)
    } else {
      if (length(x)!=1){
        print(x)        
      } else {
        if (sprintf('"%s"',x)!=.xname){
          print(x)
        }
      }
    }
  }
  invisible()
}


##' Print a message with a time stamp
##' 
##' 
##' @title Print a message with a time stamp
##' @param x ANY, an R object.
##' @return NULL
##' @author Xiaobei Zhao
##' @examples
##' 
##' stampme('Hello World!')
stampme <- function(x){
  printme(x,prefix=format(Sys.time(), "%Y%m%d %H:%M:%S %Z"),envir=sys.frame(sys.parent(0)))
  flush.console()
  invisible()
}


##' Log the name and the content of an R object given levels of logger
##'
##' 
##' @title Log the name and the content of an R object
##' @param x ANY, an R object.
##' @param prefix the prefix to log.
##' @param logger logging level, one of: NULL, 'INFO', 'DEBUG', 'WARNING', 'ERROR', 'CRITICAL'
##' @param envir the \code{environment} to use.
##' @return NULL 
##' @author Xiaobei Zhao
##' @examples
##' ## log an object
##' x1 <- 1:6
##' logme(x1)
##'
##' ## log according to logger levels
##' bar <- function(x,envir=sys.frame(sys.parent(0))){
##'   for (.logger in get.loglevel()) {
##'     if (is.null(.logger)) .prefix <- 'NULL' else .prefix <- .logger
##'     logme(x,prefix=.prefix,logger=.logger,envir=envir)
##'   }
##' }
##' options(logger='DEBUG')
##' bar(1:6) # print logs of level NULL, INFO and DEBUG
##' options(logger='ERROR')
##' bar(1:6) # print logs of level NULL, INFO, DEBUG, WARNING and ERROR
##' 
##' @seealso \code{\link{printme}}
logme <- function(x=NULL,prefix=NULL,logger=NULL,envir=sys.frame(sys.parent(0)))
{
  logger <- as.loglevel(logger)
  options(logger=as.loglevel(options()$logger))
  
  if (as.numeric(logger) <= as.numeric(options()$logger)) {
    printme(x,prefix,envir=envir)
  }
  invisible()
}


##' Log a `save`
##'
##' 
##' @title Log a `save`
##' @param x ANY, an R object.
##' @param logger see \code{\link{logme}}
##' @param envir the \code{environment} to use.
##' @return NULL
##' @author Xiaobei Zhao
##' @examples
##' inFpath <- "mydir/mypath"
##' logsave(inFpath)
##' 
logsave <- function(x,logger=NULL,envir=sys.frame(sys.parent(0))){
  logger <- as.loglevel(logger)
  options(logger=as.loglevel(options()$logger))
  
  if (as.numeric(logger) <= as.numeric(options()$logger)) {
    cat(sprintf("\nFile saved: \n%s=\"%s\"\n",atos(x,envir=envir),x))
  }
  invisible()
}


## ------------------------------------------------------------------------
## i/o
## ------------------------------------------------------------------------

##' A wrapper of write.table with customized parameters and parsing
##'
##' 
##' @title A wrapper of write.table
##' @param outFpath file, see \code{write.table}
##' @param x see \code{write.table}
##' @param append see \code{write.table}
##' @param sep see \code{write.table}
##' @param quote see \code{write.table}
##' @param row.names see \code{write.table}
##' @param col.names see \code{write.table}
##' @param logger see \code{\link{logme}}
##' @param ... further arguments passed to `write.table`. See \code{write.table}
##' @return NULL
##' @author Xiaobei Zhao
write.data.table <- function(
  outFpath="",x,append=FALSE,sep="\t",quote=FALSE,row.names=FALSE,col.names=!append,logger=NULL,
  ...){
  write.table(file=outFpath,x,append=append,sep=sep,quote=quote,row.names=row.names,col.names=col.names,...)
  logsave(outFpath,logger)
  invisible()
}


## ------------------------------------------------------------------------
## os
## ------------------------------------------------------------------------


##' Is the OS Windows
##'
##' 
##' @title Is the OS Windows
##' @return logical
##' @author Xiaobei Zhao
is.windows <- function(){as.character(Sys.info()['sysname'])=='Windows'}


##' Is the OS Linux
##' 
##' 
##' @title Is the OS Linux
##' @return logical 
##' @author Xiaobei Zhao
is.linux <- function(){as.character(Sys.info()['sysname'])=='Linux'}


## ------------------------------------------------------------------------
## file/dir
## ------------------------------------------------------------------------

##' Is it a file
##'
##' 
##' @title Is it a file
##' @param x character, a file name.
##' @return logical
##' @author Xiaobei Zhao
is.file <- function(x){
  ret <- FALSE
  if(length(x)!=1){
    return(FALSE)
  }
  if (file.exists(x)){
    if (! file.info(x)$isdir){
      ret <- TRUE
    }
  }
  return(ret)
}


##' Is it a directory
##'
##' 
##' @title Is it a directory
##' @param x character, a directory name.
##' @return logical
##' @author Xiaobei Zhao
is.dir <- function(x){
  ret <- FALSE
  if (file.exists(x)){
    if (file.info(x)$isdir){
      ret <- TRUE
    }
  }
  return(ret)
}

##' Does the directory exist
##'
##' 
##' @title Does the directory exist
##' @param x character, a directory name.
##' @return logical
##' @author Xiaobei Zhao
dir.exists <- function(x){
  file.exists(x) & is.dir(x)
}


##' Return a valid mode given digits
##'
##' 
##' @title Return a valid mode given digits
##' @param mode character, the mode of the path, see \code{dir.create}.
##' @param digits numeric, either 3 or 4.
##' @return mode
##' @author Xiaobei Zhao
##' @examples
##' valid.mode("777",4)
##' valid.mode("0777",3)
valid.mode <- function(mode,digits=4){
  if (!digits %in% c(3:4)){
    stop('valid.mode | digits must be either 3 or 4.')
  }
  if (!nchar(mode) %in% c(3:4)){
    stop('valid.mode | nchar(mode) must be either 3 or 4.')
  }
  
  if (digits==4){
    if(nchar(mode)==3){
      mode <- paste("0",mode,sep='')
    }
  }
  if (digits==3){
    if(nchar(mode)==4){
      mode <- substr(mode,2,4)
    }
  }
  return(mode)
}


##' Make a directory recursively
##'
##' 
##' @title Make a directory recursively
##' @param x character, a directory name.
##' @param mode the mode of the path, see \code{dir.create}
##' @return NULL
##' @author Xiaobei Zhao
##' @examples
##' try(make.dir('~/tmpdir','751'))
make.dir <- function(x,mode){
  if (missing(mode)){
    mode <- '0777'
    if (is.linux()){
      mode <- valid.mode(mode,3)
    } else {
      mode <- valid.mode(mode,4)
    }
  }
  if(length(x)){
    if (is.file(x)){
      stop(sprintf('make.dir | cannot create directory (%s): it is a file. ',x))
    }
    if (!dir.exists(x)){
      if (is.linux()){
        system(sprintf('mkdir -p -m %s %s',mode,x))
      } else {
        dir.create(x,mode=mode,recursive=TRUE) ## NG: mode problem via recursive
      }
    }
  }
  invisible()
}


## ------------------------------------------------------------------------
## data.frame
## ------------------------------------------------------------------------

##' Concatenate data.frame into a string
##'
##' 
##' @title Concatenate data.frame into a string
##' @param x data.frame or matrix
##' @param sep character, a delimiter
##' @param ... further arguments passed to `format`. See \code{format}
##' @return data.frame
##' @author Xiaobei Zhao
dfconcat <- function(x,sep=" ",...){
  .x <- apply(x,2,format,...)
  if(!is.matrix(.x)){
    .x <- matrix(.x,ncol=length(.x))
  }
  ret <- paste(apply(.x,1,paste,sep='',collapse=sep),sep='',collapse='\n')
  return(ret)
}


##' Chunk data.frame into parts
##'
##' 
##' @title Chunk data.frame into parts
##' @param x data.frame or matrix
##' @param n numeric, the number of chunks
##' @return a list of data.frame
##' @author Xiaobei Zhao
##' @examples
##' dfchunk(iris,n=5)
##' dfchunk(iris[1:5,],n=5)
dfchunk <- function(x,n){
  x <- as.data.frame(x)
  v_rows <- vchunk(seq(nrow(x)),n)
  ret <- sapply(v_rows,function(e){x[e,]}, simplify=FALSE)
  return(ret)
}


## ------------------------------------------------------------------------
## vector
## ------------------------------------------------------------------------

##' Chunk a vector into parts given the number of chunks
##' or the max size of a chunk
##'
##' 
##' @title Chunk a vector into parts
##' @param x vector to chunk
##' @param n numeric, the number of chunks
##' @param max.size numeric, the maximal size of a chunk
##' @param aeap logical, as equal as possible. Whether return balanced chunks.
##' @return list
##' @author Xiaobei Zhao
##' @examples
##' vchunk(1:7,7)
##' vchunk(1:19,n=3)
##' vchunk(1:19,max.size=9)
##' vchunk(1:19,max.size=9,aeap=FALSE) # unbalanced chunks
vchunk <- function(x,n=NULL,max.size=NULL,aeap=TRUE){
  'section a vector into groups no larger than max.size
@param aeap as equal as possible'
  if (!is.null(n)){
    max.size <- ceiling(length(x)/n) 
  }
  
  if (aeap){
    n <- ceiling(length(x)/max.size)
    size <- ceiling(length(x)/n)
  } else {
    size <- max.size
  }
  split(x, ceiling(seq_along(x)/size))
}


## ------------------------------------------------------------------------
## string
## ------------------------------------------------------------------------

##' Chunk a string into parts
##'
##' 
##' @title Chunk a string into parts
##' @param x character, a string to chunk.
##' @param size numeric, the size of a chunk.
##' @param brk character to link broken words.
##' @param concat logical, whether to concatenate by a `newline`
##' @return character
##' @author Xiaobei Zhao
##' @examples
##' x <- 'The quick brown fox jumps over the lazy dog.'
##' cat(schunk(x,15),'\n')
##' x <- 'The word, honorificabilitudinita, occurs in Shakespeare\'s
##' play Love\'s Labour\'s Lost, and means "with honorablenesses".'
##' cat(schunk(x,30),'\n')
##' ## The word, honorificabilitudini-
##' ## ta, occurs in Shakespeare's
##' ## play Love's Labour's Lost, and
##' ##  means "with honorablenesses".
##' 
schunk <- function(x,size,brk='-',concat=TRUE)
{
  tmp <- strsplit(x,sprintf("(?<=.{%s})",size), perl = TRUE)[[1]]
  for (i in seq_along(tmp)){
    if (i != length(tmp)){
      if (length(grep('[a-zA-Z]$',tmp[i])) & length(grep('^[a-zA-Z]',tmp[i+1]))){
        tmp[i] <- paste(tmp[i],brk,sep='')
      }
    }
  }
  if (concat){
    tmp <- paste(tmp,sep='',collapse='\n')
  }
  return(tmp)
}



##' Determine if a character string "starts with" specified characters. A modified version of gdata::startsWith.
##'
##' 
##' @title Determine if a character string "starts with" specified characters
##' @param x character, a string.
##' @param char character to match.
##' @param ignore.case logical, whether to ignore cases.
##' @return logical
##' @author Xiaobei Zhao
##' @examples
##' startswith('Hello World','hello',ignore.case=TRUE)
##' 
startswith <- 
  function(x,char,ignore.case=FALSE)
{
  if (ignore.case) {
    x <- toupper(x)
    char <- toupper(char)
  }
  substr(x, start = 1, stop = nchar(char)) == char
}


##' Determine if a character string "ends with" specified characters
##'
##' 
##' @title Determine if a character string "ends with" specified characters
##' @param x character, a string
##' @param char character to match
##' @param ignore.case logical, whether to ignore cases
##' @return logical
##' @author Xiaobei Zhao
##' @examples
##' endswith('Hello World','world',ignore.case=TRUE)
##' 
endswith <- 
  function(x,char,ignore.case = FALSE)
{
  if (ignore.case) {
    x <- toupper(x)
    char <- toupper(char)
  }
  substr(x, start = nchar(x)-nchar(char)+1, stop = nchar(x)) == char
}


##' Split a string at the first `split'
##'
##' 
##' @title Split a string at the first `split'
##' @param x character, a string to split.
##' @param split, see \code{strsplit}
##' @param ..., see \code{strsplit}
##' @return list
##' @author Xiaobei Zhao
##' @examples
##' strsplit.first('inFpath="a=1.b=2.c=TRUE"',split="=")
##' 
strsplit.first <- function(x,split,...){
  tmp.split <- "__TMPTMPTMP__"
  x <- sub(split,tmp.split,x) #sub (not gsub) to only split at the first occurence
  strsplit(x,split=tmp.split,...)
}


##' Strip a string with given characters at the beginning (left end)
##'
##' 
##' @title Strip a string with given characters at the beginning (left end)
##' @param x character, a string.
##' @param char character to trim.
##' @return character
##' @author Xiaobei Zhao
lstrip <- function(x,char=" "){
  ## Strip leading space
  sub(sprintf("^[%s]+",char), "", x)
}

##' Strip a string with given chars at the (right) end
##'
##' 
##' @title Strip a string with given chars at the (right) end
##' @param x character, a string.
##' @param char character to trim.
##' @return character
##' @author Xiaobei Zhao
rstrip <- function(x,char=" "){
  ## Strip trailing space
  sub(sprintf("[%s]+$",char), "", x)
}

##' Strip a string with given chars at both ends
##'
##' 
##' @title Strip a string with given chars at both ends
##' @param x character, a string.
##' @param char character to trim.
##' @return character
##' @author Xiaobei Zhao
strip <- function(x,char=" "){
  gsub(sprintf("(^[%s]+)|([%s]+$)",char,char), "", x)
}



## ------------------------------------------------------------------------
## string formatting
## ------------------------------------------------------------------------


##' String formatting given an environment
##'
##' 
##' @title String formatting given an environment
##' @param x character, a string to format.
##' @param envir the \code{environment} to use. 
##' @return character
##' @seealso \code{\link{sprintf}}
##' @author Xiaobei Zhao
##' @examples
##' a="fox";b="dog";
##' x <- 'The quick brown %(a)s jumps over the lazy %(b)s? 
##' Or the quick brown %(b)s jumps over the lazy %(a)s?'
##'
##' ## format given the global environment
##' lprintf(x)
##' ## [1] "The quick brown fox jumps over the lazy dog?
##' ## Or the quick brown dog jumps over the lazy fox?"
##' 
##' ## format given a local environment
##' myenv <- new.env()
##' local(
##'   {a="coyote";b="dog";},
##'   envir=myenv
##' )
##' lprintf(x,myenv)
##' ## [1] "The quick brown coyote jumps over the lazy dog?
##' ## Or the quick brown dog jumps over the lazy coyote?"
##' 
lprintf <- function(x,envir=sys.frame(sys.parent(1))){
  if(is.numeric(envir) | is.integer(envir)){
    envir <- sys.frame(sys.parent(envir))
  }
  .fmt <- function(x,envir,match,value,conversion='s'){
    replacement <- get(value,envir=envir)
    replacement <- switch(
      conversion,
      's'=as.character(replacement), # only for %s
      stop('lprintf | only "%%s" inplemented')
    )
    if(!length(replacement)){
      stop(sprintf("lprintf | !length(%s)",value))
    }
    if(is.na(replacement)){
      stop(sprintf("lprintf | is.na(%s)",value))
    }
    
    ## logme(match,'lprintf','DEBUG')
    ## logme(value,'lprintf','DEBUG')
    ## logme(replacement,'lprintf','DEBUG')
    tryCatch(gsub(match,replacement,x,fixed=TRUE),error=function(e){
      stop(sprintf("lprintf | Invalid value: %s",value))
    })
  }
  ## match
  ## require(stringr)
  ## stringr::str_match_all
  require(stringr)
  .m <- stringr::str_match_all(x,'\\%\\(([^()]+)\\)([a-z])')[[1]]
  for (i in seq(nrow(.m))){
    x <- .fmt(x,envir,.m[i,1],.m[i,2],.m[i,3])
  }
  x
}


##' Convert an R object to a string
##'
##' 
##' @title Convert an R object to a string
##' @param x an R object.
##' @param envir the \code{environment} to use.
##' @return character
##' @author Xiaobei Zhao
atos <- function(x,envir=sys.frame(sys.parent(0))){
  deparse(substitute(x,env=envir),width.cutoff=500)
}


## ------------------------------------------------------------------------
## packages
## ------------------------------------------------------------------------

##' Check if a package is loaded
##'
##' 
##' @title Check if a package is loaded
##' @param x package, see \code{library} or \code{require}.
##' @param envir the \code{environment} to use.
##' @param character.only see \code{library} or \code{require}.
##' @return logical
##' @author Xiaobei Zhao
##' @seealso \code{\link{check.packages}}
##' @examples
##' is.package.loaded(Xmisc)
##' is.package.loaded("Xmisc")
##' x <- "Xmisc"
##' is.package.loaded(x) #FALSE
##' is.package.loaded(x,character.only=TRUE) #TRUE
is.package.loaded <-
  function(x,envir=sys.frame(sys.parent(0)),character.only=FALSE)
{
  if (!character.only){
    x <- atos(x,envir=envir)
    x <- strip(x,'\'|"')
  }
  sprintf("package:%s",x) %in% search() 
}


##' Check if a package can be loaded. If TRUE, load it as long as it has not yet been loaded.
##'
##' 
##' @title Check if a package can be loaded
##' @param x package, see \code{library} or \code{require}.
##' @param envir the \code{environment} to use.
##' @param character.only see \code{library} or \code{require}.
##' @return logical, whether a package can be loaded.
##' @author Xiaobei Zhao
##' @seealso \code{\link{is.package.loaded}}
##' @examples
##' check.packages("Xmisc")
##' check.packages(Xmisc)
##' x <- "Xmisc"
##' check.packages(x,character.only=TRUE)
check.packages <-
  function(x,envir=sys.frame(sys.parent(0)),character.only=FALSE)
{
  if (is.package.loaded(x,envir=envir)){
    return(TRUE)
  }
  ## x %in% rownames(installed.packages()) ## slow!
  if (!character.only){
    x <- atos(x,envir=envir)
    x <- strip(x,'\'|"')
  }
  ret <-
    tryCatch(
      {library(x,character.only=TRUE,logical.return=TRUE)},
      error=function(e){cat(as.character(e));return(FALSE)}
      )
  ret
}
