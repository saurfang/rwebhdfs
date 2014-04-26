#' Status of a File/Directory on a FileSystem
#' 
#' @return a list of status information on file/directory
#' @title file_stat: Status of a File/Directory on a FileSystem
#' @param fs FileSystem object
#' @param path a character vector that contains the path of file/directory
#' @param ... other arguments
#' @rdname file_stat
#' @export file_stat
file_stat <- function(fs, path, ...){
  UseMethod("file_stat")
}

#' @rdname file_stat
#' @method file_stat default
#' @S3method file_stat default
file_stat.default <- function(fs, path, ...){
  warning("Unrecognized filesystem, invoking file.info...")
  file.info(path, ...)
}
 
#' @rdname file_stat
#' @method file_stat webhdfs
#' @S3method file_stat webhdfs
#' @importFrom jsonlite fromJSON
#' @include curl_webhdfs.R
file_stat.webhdfs <- function(fs, path, ...){
  #Check path is non-empty
  if(!nzchar(path))
    stop("Path must be non-empty!")
  
  response <- curl_webhdfs(fs, paste0(path,"?op=GETFILESTATUS"), "GET", ...)
  status <- fromJSON(response)$FileStatus
  status$accessTime <- as.POSIXlt(status$accessTime/1000, origin="1970-01-01")
  status$modificationTime <- as.POSIXlt(status$modificationTime/1000, origin="1970-01-01")
  status
}