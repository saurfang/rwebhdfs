#' Rename a File/Directory on a FileSystem
#' 
#' @return \code{TRUE} if successful, \code{FALSE} otherwise
#' @title delete_file: Rename a File/Directory on a FileSystem
#' @param fs FileSystem object
#' @param path a character vector that contains the path renaming from
#' @param recursive whether to delete contents of directory recursively
#' @param ... other arguments
#' @rdname delete_file
#' @export delete_file
delete_file <- function(fs, path, recursive, ...){
  UseMethod("delete_file")
}

#' @rdname delete_file
#' @method delete_file default
#' @export
delete_file.default <- function(fs, path, recursive=FALSE, ...){
  warning("Unrecognized filesystem, invoking file.remove...")
  file.remove(path, recursive)
}

#' @rdname delete_file
#' @method delete_file webhdfs
#' @importFrom RCurl basicHeaderGatherer
#' @importFrom jsonlite fromJSON
#' @include curl_webhdfs.R
#' @export
delete_file.webhdfs <- function(fs, path, recursive=FALSE, ...){  
  url <- paste0(path,"?op=DELETE&recursive=",if(isTRUE(recursive)) "true" else "false")
  response <- curl_webhdfs(fs, url, "DELETE" , ...)  
  return(fromJSON(response)$boolean)
}
