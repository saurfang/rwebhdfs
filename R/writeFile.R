#' Write to a File on a FileSystem
#' 
#' @return \code{TRUE} if successful, \code{FALSE} otherwise
#' @title writeFile: Write to a File on a FileSystem
#' @param fs FileSystem object
#' @param targetPath a character vector that contains the path of file to write
#' @param ... other arguments
#' @rdname writeFile
#' @export writeFile
writeFile <- function(fs, targetPath, ...){
  UseMethod("writeFile")
}

#' @rdname writeFile
#' @method writeFile default
#' @S3method writeFile default
writeFile.default <- function(fs, targetPath, ...){
  warning("Unrecognized filesystem, invoking write...")
  write(..., file=targetPath)
}
 
#' @title Create and Write to a File on HDFS
#' @rdname writeFile.hdfs
#' @method writeFile webhdfs
#' @S3method writeFile webhdfs
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
#' @param ... additional arguments passed to \code{\link[RCurl]{getURL}}
#' @return \code{TRUE} if successful, \code{FALSE} otherwise
#' @references \href{http://hadoop.apache.org/docs/stable/hadoop-project-dist/hadoop-hdfs/WebHDFS.html#Create_and_Write_to_a_File}{WebHDFS}
#' \href{http://hadoop.apache.org/docs/stable/api/org/apache/hadoop/fs/FileSystem.html}{HDFS FileSystem}
#' @importFrom RCurl basicHeaderGatherer
writeFile.webhdfs <- function(fs, targetPath, srcPath, sizeWarn=1e8,
                              append=FALSE, overwrite=FALSE,
                              blocksize=NULL, replication=NULL,
                              permission=755, buffersize=NULL, ...){
  #Check path is absolute
  if(!nzchar(targetPath) || substring(targetPath, 1, 1)!="/")
    stop("Target Path must be non-empty and start with slash '/'")
  #Check source path exists
  if(!file.exists(srcPath))
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
    if(grepl("^[01]?[0-7]{3}$", permission))
      url <- paste0(url,"&permission=",as.integer(permission))
    else
      warning("invalid permission code")
  }
  
  if(is.numeric(buffersize) && buffersize > 0)
    url <- paste0(url, "&buffersize=",as.integer(buffersize))
  
  h <- basicHeaderGatherer()
  curlWebHDFS(fs, url, "PUT", followlocation = FALSE, headerfunction = h$update, ...)
  if(h$value()["status"]!="307")
    warning("Unrecognized header content: ", h$value(), "[expect 307]")
  location <- h$value()["Location"]
  h$reset()
  
  #Check file is valid and not too big
  info <- file.info(srcPath)
  if(info$isdir)
    stop("Source path is a directory not a file!")
  if(isTRUE(info$size > sizeWarn)){
    message("File ", basename(srcPath), 
            " (",info$size," bytes) exceeds threshold size: ", sizeWarn)
    readline("Press [Enter] to continue...")  
  }
  
  response <- curlWebHDFS(fs, location, "PUT", putContent=readLines(srcPath), headerfunction = h$update, ...)
  if(h$value()["status"]!="201" || h$value()["statusMessage"]!="Created"){
    warning("Failed to write file: ", h$value(), "\n", response)
    return(FALSE)
  }
  return(TRUE)
}