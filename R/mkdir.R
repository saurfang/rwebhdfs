#' Create directory on a FileSystem
#' 
#' @return \code{TRUE} if seccessful, \code{FALSE} otherwise
#' @title mkdir: Create Directory
#' @param fs FileSystem object
#' @param path a character vector that contains the path of folder to create
#' @param ... other arguments
#' @rdname mkdir
#' @export mkdir
mkdir <- function(fs, path, ...){
  UseMethod("mkdir")
}

#' @rdname mkdir
#' @method mkdir default
#' @export
mkdir.default <- function(fs, path, ...){
  warning("Unrecognized filesystem, invoking dir.create...")
  dir.create(path, ...)
}
 
#' @rdname mkdir
#' @method mkdir webhdfs
#' @export
#' @importFrom jsonlite fromJSON
#' @include curl_webhdfs.R
mkdir.webhdfs <- function(fs, path, ...){
  #Check path is non-empty
  if(!nzchar(path))
    stop("Path must be non-empty!")
  
  response <- curl_webhdfs(fs, paste0(path,"?op=MKDIRS"), "PUT", ...)
  return(fromJSON(response)$boolean)
}
