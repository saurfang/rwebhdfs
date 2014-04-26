#' Get File Checksum on a FileSystem
#' 
#' @return a list of Checksum information
#' @title file_checksum: Get File Checksum on a FileSystem
#' @param fs FileSystem object
#' @param path a character vector that contains the path of file
#' @param ... other arguments
#' @rdname file_checksum
#' @export file_checksum
file_checksum <- function(fs, path, ...){
  UseMethod("file_checksum")
}

#' @rdname file_checksum
#' @method file_checksum default
#' @S3method file_checksum default
file_checksum.default <- function(fs, path, ...){
  .NotYetImplemented()
}
 
#' @rdname file_checksum
#' @method file_checksum webhdfs
#' @S3method file_checksum webhdfs
#' @importFrom jsonlite fromJSON
#' @include curl_webhdfs.R
file_checksum.webhdfs <- function(fs, path, ...){  
  response <- curl_webhdfs(fs, paste0(path,"?op=GETFILECHECKSUM"), "GET", followlocation = TRUE ...)
  fromJSON(response)$FileChecksum
}