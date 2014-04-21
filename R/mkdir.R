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
#' @S3method mkdir default
mkdir.default <- function(fs, path, ...){
  warning("Unrecognized filesystem, invoking dir.create...")
  dir.create(path, ...)
}
 
#' @rdname mkdir
#' @method mkdir webhdfs
#' @S3method mkdir webhdfs
#' @importFrom jsonlite fromJSON
mkdir.webhdfs <- function(fs, path, ...){
  #Check path is absolute
  if(!nzchar(path) || substring(path, 1, 1)!="/")
    stop("Path must be non-empty and start with slash '/'")
  
  response <- curlWebHDFS(fs, paste0(path,"?op=MKDIRS"), "PUT", ...)
  return(fromJSON(response)$boolean)
}