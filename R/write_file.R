#' Write to a File on a FileSystem
#' 
#' @return \code{TRUE} if successful, \code{FALSE} otherwise
#' @title write_file: Write to a File on a FileSystem
#' @param fs FileSystem object
#' @param targetPath a character vector that contains the path of file to write
#' @param ... other arguments
#' @rdname write_file
#' @export write_file
write_file <- function(fs, targetPath, ...){
  UseMethod("write_file")
}

#' @rdname write_file
#' @method write_file default
#' @S3method write_file default
write_file.default <- function(fs, targetPath, ...){
  warning("Unrecognized filesystem, invoking write...")
  write(..., file=targetPath)
}
 
#' @title Create and Write to a File on HDFS
#' @rdname write_file.webhdfs
#' @method write_file webhdfs
#' @S3method write_file webhdfs
#' @param fs HDFS FileSystem object
#' @param targetPath a character vector that contains the path of file to write
#' @param srcPath path of the file whose content will be written
#' @param sizeWarn raise warning if file size exceeds threshold. Use \code{NULL} to suppress warning
#' @param append to a file if \code{TRUE}
#' @param overwrite a file if \code{TRUE} and file already exists
#' @param blocksize hdfs blocksize for the file
#' @param replication factor for the file
#' @param permission of the file as in octal mask
#' @param buffersize used in transferring data
#' @param ... additional arguments passed to \code{\link{curl_webhdfs}}
#' @return \code{TRUE} if successful, \code{FALSE} otherwise
#' @references \href{http://hadoop.apache.org/docs/stable/hadoop-project-dist/hadoop-hdfs/WebHDFS.html}{WebHDFS}
#' \href{http://hadoop.apache.org/docs/stable/api/org/apache/hadoop/fs/FileSystem.html}{HDFSFileSystem}
#' @importFrom RCurl basicHeaderGatherer
#' @include curl_webhdfs.R
write_file.webhdfs <- function(fs, targetPath, srcPath, sizeWarn=1e8,
                              append=FALSE, overwrite=FALSE,
                              blocksize=NULL, replication=NULL,
                              permission=755, buffersize=NULL, ...){
  #Check path is non empty
  if(!nzchar(targetPath))
    stop("Target Path must be non-empty")
  #Check source path exists
  if(!missing(srcPath) && !file.exists(srcPath))
    stop("Source path doesn't exist")
  
  if(append){
    url <- paste0(targetPath,"?op=APPEND")
  }else{
    url <- paste0(targetPath,"?op=CREATE")
    if(isTRUE(overwrite))
      url <- paste0(url,"&overwrite=true")
    if(is.numeric(blocksize) && blocksize > 0)
      url <- paste0(url,"&blocksize=",as.integer(blocksize))
    if(is.numeric(replication) && replication > 0)
      url <- paste0(url,"&replication=",as.integer(replication))
    if(!is.null(permission) && grepl("^[01]?[0-7]{3}$", permission))
      url <- paste0(url,"&permission=",as.integer(permission))
    else
      warning("invalid permission code")
  }
  
  if(is.numeric(buffersize) && buffersize > 0)
    url <- paste0(url, "&buffersize=",as.integer(buffersize))
  
  h <- basicHeaderGatherer()
  curl_webhdfs(fs, url, if(append) "POST" else "PUT", followlocation = FALSE, headerfunction = h$update, ...)
  if(h$value()["status"]!="307")
    warning("Unrecognized header content: ", h$value(), "[expect 307]")
  location <- h$value()["Location"]
  h$reset()
  
  if(!missing(srcPath)){
    #Check file is valid and not too big
    info <- file.info(srcPath)
    if(info$isdir)
      stop("Source path is a directory not a file!")
    if(isTRUE(info$size > sizeWarn)){
      message("File ", basename(srcPath), 
              " (",info$size," bytes) exceeds threshold size: ", sizeWarn)
      readline("Press [Enter] to continue...")  
    }
  }
  
  response <- curl_webhdfs(fs, location, if(append) "POST" else "PUT", 
                           putContent= if(missing(srcPath)) NULL else readLines(srcPath), 
                           headerfunction = h$update, ...)
  if(append){
    if(h$value()["status"]!="200" || h$value()["statusMessage"]!="OK"){
      warning("Failed to append file: ", h$value(), "\n", response)
      return(FALSE)
    }
  }else{ 
    if(h$value()["status"]!="201" || h$value()["statusMessage"]!="Created"){
      warning("Failed to write file: ", h$value(), "\n", response)
      return(FALSE)
    }
  }
  
  return(TRUE)
}