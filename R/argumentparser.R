
## ************************************************************************
## 
## 
## 
## (c) Xiaobei Zhao
## 
## Mon Feb 24 08:41:03 EST 2014 -0500 (Week 08)
## 
## 
## Reference: 
## 
## 
## ************************************************************************


##' Argument
##'
##' @title Argument
##' @author Xiaobei Zhao
##' @field name character
##' @field value ANY
##' @field type character
##' @field default ANY
##' @field help character
Argument <- 
  setRefClass(
    'Argument',
    list(
      name='character',
      value='ANY',
      type='character',
      default='ANY',
      help='character'
      ),
    contains='xRefClass',
    methods=list(
      initialize=function(...){
        ''
        .idx <- c(name=1,value=2,type=3,default=4,help=5)
        callSuper(...,.index=.idx)
        ##logme(.envir,'Argument | initialize','DEBUG')
        .envir <<- globalenv()
        setvalue()
        assignme()
      },
      getname=function(){
        name
      },
      getvalue=function(){
        value
      },
      gettype=function(){
        type
      },
      getdefault=function(){
        default
      },
      gethelp=function(){
        help
      },
      setvalue=function(){
        if (!length(type)){
          type <<- 'ANY'
        }
        if (class(value) == "uninitializedField" | !length(value)) {
          ##logme("Argument | 1")
          if (class(default) == "uninitializedField" | !length(default)) {
            ##logme("Argument | 2")
            value <<- ValueParser$new(type=type)$getvalue()
          } else {
            ##logme("Argument | 3")
            value <<- ValueParser$new(default,type=type)$getvalue()
          }
        } else {
          ##logme("Argument | 4")
          value <<- ValueParser$new(value,type=type)$getvalue()      
        }
      },
      assignme=function(){
        if(length(name)){
          ## logme(name, "Argument | assignme") ##
          ## logme(value, "Argument | assignme") ##
          ## logme(.envir, "Argument | assignme") ##
          base::assign(name,value,envir=.envir)
        }
        invisible()
      }
      )
    )




##' Parser for command-line options and arguments
##'
##' @title Parser for command-line options and arguments
##' @author Xiaobei Zhao
##' @field cmdargs list
##' @field exeargs character
##' @field args list
##' @field types list
##' @field defaults list
##' @field helps list
##' @field usage character
##' @field description character
##' @examples
##' ## Test
##' require(Xmisc)
##' parser <- ArgumentParser$new()
##' parser$add_argument('--a_str',type='character')
##' parser$add_argument('--b_num',type='numeric',default='0')
##' a_str
##' ## character(0)
##' b_num
##' ## [1] 0
##' parser$make_help()
##' ## Usage:
##' ##     /bin/exec/R ...
##' ## Description:
##' ## Options:
##' ##     a_str character
##' ##     b_num numeric   [ 0 ]
##' 
##' \dontrun{
##' ## Test from a command line
##' R -q -e "
##' require(methods);require(Xmisc);
##' parser <- ArgumentParser\$new();
##' parser\$add_argument('--a_str',type='character');
##' parser\$add_argument('--b_num',type='numeric',default='0');
##' printme(a_str);printme(b_num);parser\$make_help();
##' " --args --a_str='Hello World!' --b_num=1
##' ## Loading required package: Xmisc
##' ## ## a_str ##
##' ## [1] "Hello World!"
##' ## ## b_num ##
##' ## [1] 1
##' ## ...
##' }
##' 
ArgumentParser <-
  setRefClass(
    'ArgumentParser',
    list(
      cmdargs='list',
      exeargs='character',
      args='list',
      types='list',
      defaults='list',
      helps='list',
      #narg='numeric',
      usage='character',
      description='character'
      ),
    contains='xRefClass',
    methods=list(
      initialize=function(...){
        ''
        callSuper(...)
        setme()
        setcmdargs()
      },
      setme=function(){
        ## logme(.envir,'ArgumentParser | initialize','DEBUG') ## 
        ## identical(.envir, emptyenv())
        if (identical(.envir,as.environment(.self))) {
          .envir <<- globalenv()
        }
      },
      setcmdargs=function(){
        .args <- .setcmdargs()
        exeargs <<- .args$exeargs
        cmdargs <<- .args$cmdargs        
      },
      setenvir=function(x){
        .envir <<- as.environment(x)
        ## helps <<- list()
      },
      getcmdargs=function(){
        cmdargs
      },
      getargs=function(){
        args
      },
      gethelps=function(){
        helps
      },
      add_argument=function(
        name,...,type,default,required=FALSE,help='',dest,action
        ){
        'Add an argument.'
        ## Ideas from
        ## http://docs.python.org/2/library/argparse.html#the-add-argument-method
        if(missing(action)){
          action <- 'store'
        }
        a.obj <- .add_argument(
          name,...,type=type,default=default,required=required,help=help,dest=dest,
          action=action,
          cmdargs=cmdargs,envir=.envir
          )
        args[[a.obj$getname()]] <<- a.obj$getvalue()
        types[[a.obj$getname()]] <<- a.obj$gettype()
        defaults[[a.obj$getname()]] <<- a.obj$getdefault()
        helps[[a.obj$getname()]] <<- a.obj$gethelp()
        invisible()
      },
      add_usage=function(x){
        'Add a usage.'
        usage <<- x
      },
      add_description=function(x){
        'Add a description.'
        description <<- x
      },
      make_help=function(){
        'Make and display `usage\'.'
        if(!length(usage)){
          usage <<- paste(paste(exeargs,sep='',collapse=' '),'...')
        }
        if(!length(description)){
          description <<- ''
        }
        ret <- .make_help(exeargs,args,types,defaults,helps,usage,description)
        cat(ret)
        invisible(ret)
      }
      )
    )



## ------------------------------------------------------------------------
## 
## ------------------------------------------------------------------------

## .setcmdargs.v1 <- function(trailingOnly=TRUE){
##   .cmdargs <- base::commandArgs(trailingOnly=trailingOnly)
##   ## logme(.cmdargs)
##   res <- list()
##   if (length(.cmdargs)){
##     .cmdargs <- grep("=",.cmdargs,fixed=TRUE,value=TRUE) # pick out those with "="
##     name.value.list <- sapply(.cmdargs,strsplit.first,split="=") # result as a list of list
##     name.value.mat <- do.call(rbind,name.value.list)
##     res <- as.list(name.value.mat[,2])
##     names(res) <- lstrip(name.value.mat[,1],"--")
##     cmdargs <- res
##   }
##   res
## }



##' .setcmdargs
##'
##' 
##' @title .setcmdargs
##' @return list
##' @author Xiaobei Zhao
##' @examples
##' R --args -a 1 -b 2 -T --myvar="apple" myvar2="orange"
##' .setcmdargs()
##' R --args -a 1 -b 2 -T --myvar="apple" -T2
##' .setcmdargs()
.setcmdargs <- function()
{
  .cmdargs0 <- base::commandArgs(trailingOnly=FALSE)
  .which <- which(.cmdargs0=='--args')

  if(length(.which)){
    .exeargs <- .cmdargs0[seq(1,.which-1)]
    .cmdargs <- .cmdargs0[-seq(.which)]
  } else {
    .exeargs <- .cmdargs0
    .cmdargs <- c()
  }
  ## logme(.cmdargs)
  .grep.single.hyphen <- grep('-[[:alnum:]\\.]{2}[[:space:]=]',.cmdargs)
  if (length(.grep.single.hyphen)){
    stop(sprintf('Invalid cmdargs: %s',paste(.cmdargs[.grep.single.hyphen],sep='',collapse=', ')))
  }
  .grep.nonhyphen <- grep('^[[:alnum:]\\.]+$',.cmdargs)
  if (length(.grep.nonhyphen)>=2){
    tmp.which <- which((.grep.nonhyphen[-1]-.grep.nonhyphen[-length(.grep.nonhyphen)])==1)
    if (length(tmp.which)){      
      stop(sprintf('Invalid cmdargs: %s',paste(.cmdargs[.grep.nonhyphen[c(tmp.which,tmp.which+1)]],sep='',collapse=', ')))
    }
  }
  
  .grep.equal <- grep('^[[:alnum:]\\.]+=.*$',.cmdargs)
  if (length(.grep.equal)){
    stop(sprintf('Invalid cmdargs: %s',paste(.cmdargs[.grep.equal],sep='',collapse=', ')))
  }

  ##
  tmp.length <- length(.cmdargs)
  res <- list()
  for (i in seq_along(.cmdargs)) {
    e_i <- .cmdargs[i]
    if (i<tmp.length) {
      e_j <- .cmdargs[i+1]
    } else {
      e_j <- character()
    }
    if (startswith(e_i,'-')) {
      if (!length(grep("=",e_i))){
        if (length(e_j)){
          if (startswith(e_j,'-')){
            e <- lstrip(e_i,'-')
            v <- TRUE          
          } else {
            e <- lstrip(e_i,'-')
            v <- e_j          
          }
        } else {
          e <- lstrip(e_i,'-')
          v <- TRUE        
        }
      } else {
        e <- lstrip(e_i,'-')
        ev <- strsplit.first(e,"=")[[1]]
        e <- ev[1]
        v <- ev[2]
      }
    } else {
      next
    }
    res[[e]] <- v
  }

  ret <- list(exeargs=.exeargs,cmdargs=res)
  return(ret)
}




## ------------------------------------------------------------------------
## 
## ------------------------------------------------------------------------

## .add_argument.v1 <- function(name,type,default,cmdargs,envir){
##   args <- list()
##   .any <- R5.value.default('ANY')
##   .names <- names(cmdargs)
##   if(!name %in% .names){
##     value <- .any
##   } else {
##     value <- cmdargs[name]
##   }
##   if(missing(default)){
##     default <- .any
##   }
##   ##logme(envir,"ArgumentParser | add_argument",'DEBUG')
##   a.obj <- Argument$new(name=name,value=value,type=type,default=default,envir=envir)
##   args[[a.obj$getname()]] <- a.obj$getvalue() ## <<-
##   ## invisible()
##   args
## }



##' .add_argument
##'
##' 
##' @title .add_argument
##' @param name 
##' @param ... 
##' @param type 
##' @param default 
##' @param required 
##' @param help 
##' @param dest 
##' @param action action when assigned values are not available
##' @param cmdargs 
##' @param envir 
##' @return (argument)
##' @author Xiaobei Zhao
##' @examples
##' 
##' R --args -a 1 -b 2 -T --myvar="apple" -T2
##' .setcmdargs()
##' 
##' .envir <- new.env()
##' .add_argument('-a',type='character',dest='a_char',cmdargs=.setcmdargs()$cmdargs,envir=.envir)
##' try(.add_argument('-a','--myvar',type='character',dest='a_var',cmdargs=.setcmdargs()$cmdargs,envir=.envir))
##' .add_argument('-a',type='integer',dest='a_int',cmdargs=.setcmdargs()$cmdargs,envir=.envir)
##' .add_argument('-s',type='numeric',default='99',dest='a_num',cmdargs=.setcmdargs()$cmdargs,envir=.envir)
##' as.list(.envir)
##' 
.add_argument <- function(
  name,...,type,default,required=FALSE,help='',dest,
  action=c("store","store_true","store_false"),
  cmdargs,envir
  ){
  action <- match.arg(action)

  ## 
  .flags <- list(name,...) # any character leading w/ "-"
  ##logme(.flags,'.add_argument','DEBUG')
  
  flags <- unlist(.flags)
  flags.str <- paste(flags,sep='',collapse=', ')
  if (length(names(flags))){
    printme(.flags)
    stop(sprintf('add_argument | flags (%s) must be unnamed.',flags.str))
  }
  if (!all(startswith(flags,'-'))){
    printme(.flags)
    stop(sprintf('add_argument | flags (%s) must lead by "-".',flags.str)) 
  }
  ## e
  .names <- unique(lstrip(flags,"-"))
  if (missing(dest)){
    e <- .names[which(nchar(.names)==max(nchar(.names)))]
  } else {
    if (!is.character(dest)){
      stop(sprintf('add_argument | dest (%s) must be character.',dest))
    }
    e <- dest
  }
  ## v
  .any <- R5.value.default('ANY')
  .tmp <- .names[.names %in% names(cmdargs)]
  if (length(.tmp)>=2){    
    printme(.flags)
    stop(sprintf('add_argument | duplicate argument assignments (%s).',flags.str))
  } else if (length(.tmp)==1) {
    v <- cmdargs[[.tmp]]
    if (!length(v)){
      if (action=='store_true'){
        v <- TRUE
      } else if (action=='store_fasle'){
        v <- FALSE
      }
    }
  } else {
    if (required & missing(default)) {
      stop(sprintf('add_argument | argument (%s) is required.',e))
    }
    v <- .any
  }

  if(missing(default)){
    default <- .any
  }
  
  a.obj <- Argument$new(name=e,value=v,type=type,default=default,help=help,envir=envir)
  return(a.obj)
}


## ------------------------------------------------------------------------
## help/usage
## ------------------------------------------------------------------------

.make_help <- function(
  exeargs,args,types,defaults,helps,
  usage=paste(paste(exeargs,sep='',collapse=' '),'...'),description="",
  indent.width=4,
  tot.width=72
  )
{
  .defaults <- lapply(defaults,function(e){ifelse(is.uninitializedField(e),"__uninitializedField__",e)})
  tmp <- as.data.frame(cbind(name=names(args),type=unlist(types),default=unlist(.defaults),help=unlist(helps)),stringsAsFactors=FALSE)
  ## 
  usage <- paste(
    format('',width=indent.width),
    schunk(usage,size=tot.width-indent.width,brk=''),sep='',collapse='\n'
    )
  description <- paste(
    format('',width=indent.width),
    schunk(description,size=tot.width-indent.width),sep='',collapse='\n'
    )
  ## help
  help.col.width <- tot.width-sum(c(indent.width,1,max(nchar(tmp[,"name"])),1,max(nchar(tmp[,"type"]))))
  tmp[,"help"] <- rstrip(tmp[,"help"],'.|;')
  tmp[,"help"] <- sapply(sapply(tmp[,"help"],schunk,size=help.col.width),paste,sep='',collapse=paste('\n',format("",width=tot.width-help.col.width),sep=''))
  tmp[,"help"] <- sapply(tmp[,"help"],function(e) ifelse(!length(e) | ''==e,"",sprintf('%s. ',e))) ##XB    
  tmp[,"default"] <- sapply(tmp[,"default"],function(e) ifelse(!length(e) | '__uninitializedField__'==e,"",sprintf('[ %s ]',e)))
  ## desc
  tmp <- cbind(tmp,desc=paste(tmp[,"help"],tmp[,"default"],sep=''))
  tmp <- cbind(tmp,prefix=format('',width=indent.width-1))

  ret <- ''
  ret <- paste(ret,'\n',sep='')
  ret <- paste(ret,'Usage:',sep='')
  ret <- paste(ret,'\n',sep='')
  ret <- paste(ret,usage,sep='')
  ret <- paste(ret,'\n',sep='')
  ret <- paste(ret,'\n',sep='')
  ret <- paste(ret,'Description:',sep='')
  ret <- paste(ret,'\n',sep='')
  ret <- paste(ret,description,sep='')
  ret <- paste(ret,'\n',sep='')
  ret <- paste(ret,'\n',sep='')
  ret <- paste(ret,'Options:',sep='')
  ret <- paste(ret,'\n',sep='')
  ret <- paste(ret,dfconcat(tmp[,c('prefix','name','type','desc')]),sep='')
  ret <- paste(ret,'\n',sep='')
  ret <- paste(ret,'\n',sep='')
  return(ret)
}


