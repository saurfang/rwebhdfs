#' Rename a File/Directory on a FileSystem
#' 
#' @return \code{TRUE} if successful, \code{FALSE} otherwise
#' @title rename_file: Rename a File/Directory on a FileSystem
#' @param fs FileSystem object
#' @param fromPath a character vector that contains the path renaming from
#' @param toPath a character vector that contains the path renaming to
#' @param ... other arguments
#' @rdname rename_file
#' @export rename_file
rename_file <- function(fs, fromPath, toPath, ...){
  UseMethod("rename_file")
}

#' @rdname rename_file
#' @method rename_file default
#' @S3method rename_file default
rename_file.default <- function(fs, fromPath, toPath, ...){
  warning("Unrecognized filesystem, invoking file.rename...")
  file.rename(fromPath, toPath)
}

#' @rdname rename_file
#' @method rename_file webhdfs
#' @S3method rename_file webhdfs
#' @importFrom RCurl basicHeaderGatherer
#' @importFrom jsonlite fromJSON
#' @include curl_webhdfs.R
rename_file.webhdfs <- function(fs, fromPath, toPath, ...){  
  #Padding toPath with home directory if needed
  if(grep("^/", toPath, invert=TRUE))
    toPath <- paste0(get_webhdfs_home(fs, ...), "/", toPath)
  
  url <- paste0(fromPath,"?op=RENAME&destination=",toPath)
  response <- curl_webhdfs(fs, url, "PUT" , ...)
  
  return(fromJSON(response)$boolean)
}