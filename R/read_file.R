#' Read a File on a FileSystem
#' 
#' @return a character vector that contains the contents of the File
#' @title read_file: Read a File on a FileSystem
#' @param fs FileSystem object
#' @param path a character vector that contains the path of file to read
#' @param ... other arguments
#' @rdname read_file
#' @export read_file
read_file <- function(fs, path, ...){
  UseMethod("read_file")
}

#' @rdname read_file
#' @method read_file default
#' @export
read_file.default <- function(fs, path, ...){
  warning("Unrecognized filesystem, invoking readLines...")
  readLines(path, ...)
}
 
#' @title Read to a File on HDFS
#' @rdname read_file.webhdfs
#' @method read_file webhdfs
#' @export
#' @param fs HDFS FileSystem object
#' @param path a character vector that contains the path of file to read
#' @param offset The starting byte position
#' @param length The number of bytes to be processed
#' @param buffersize used in transferring data
#' @param ... additional arguments passed to \code{\link{curl_webhdfs}}
#' @return a character vector that contains the contents of the File
#' @references \href{http://hadoop.apache.org/docs/stable/hadoop-project-dist/hadoop-hdfs/WebHDFS.html}{WebHDFS}
#' \href{http://hadoop.apache.org/docs/stable/api/org/apache/hadoop/fs/FileSystem.html}{HDFSFileSystem}
#' @importFrom RCurl basicHeaderGatherer
#' @include curl_webhdfs.R
read_file.webhdfs <- function(fs, path, offset=NULL, length=NULL, buffersize=NULL, ...){
  #Check path is non empty
  if(!nzchar(path))
    stop("Path must be non-empty")
  
  url <- paste0(path,"?op=OPEN")
  if(is.numeric(offset) && offset > 0)
    url <- paste0(url,"&offset=",as.integer(offset))
  if(is.numeric(length) && length > 0)
    url <- paste0(url,"&length=",as.integer(length))  
  if(is.numeric(buffersize) && buffersize > 0)
    url <- paste0(url, "&buffersize=",as.integer(buffersize))
  
  h <- basicHeaderGatherer()
  response <- curl_webhdfs(fs, url, "GET", followlocation = TRUE, headerfunction = h$update, ...)
  
  if(h$value()["status"]!="200" || h$value()["statusMessage"]!="OK"){
    warning("Failed to read file: ", h$value(), "\n", response)
    return(NULL)
  }
  
  return(response)
}
