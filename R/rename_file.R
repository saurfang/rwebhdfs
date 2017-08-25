#' Rename a File/Directory on a FileSystem
#' 
#' @return \code{TRUE} if successful, \code{FALSE} otherwise
#' @title rename_file: Rename a File/Directory on a FileSystem
#' @param fs FileSystem object
#' @param fromPath a character vector that contains the path renaming from
#' @param toPath a character vector that contains the path renaming to.
#'               If it starts with \code{/} the absolute path is taken,
#'               otherwise \code{toPath} is relative to webhdfs home
#'               derived by \code{\link{get_webhdfs_home}}
#' @param ... other arguments
#' @seealso \code{\link{get_webhdfs_home}}
#' @rdname rename_file
#' @export rename_file
rename_file <- function(fs, fromPath, toPath, ...){
  UseMethod("rename_file")
}

#' @rdname rename_file
#' @method rename_file default
#' @export
rename_file.default <- function(fs, fromPath, toPath, ...){
  warning("Unrecognized filesystem, invoking file.rename...")
  file.rename(fromPath, toPath)
}

#' @rdname rename_file
#' @method rename_file webhdfs
#' @export
#' @importFrom RCurl basicHeaderGatherer
#' @importFrom jsonlite fromJSON
#' @include curl_webhdfs.R
#' 
#' @examples
#' \dontrun{
#' ## move file /user/<username>/foo (in hdfs) to
#' ## /user/<username>/bar/foo
#' rename_file(hdfs, "foo", "bar/foo")
#' 
#' ## move file /user/<username>/foo (in hdfs) to
#' ## /mypath/foo
#' rename_file(hdfs, "foo", "/mypath/foo")
#'}
#'
rename_file.webhdfs <- function(fs, fromPath, toPath, ...){  
  #Padding toPath with home directory if needed
  if(!grep("^/", toPath))
    toPath <- paste0(get_webhdfs_home(fs, ...), "/", toPath)
  
  url <- paste0(fromPath,"?op=RENAME&destination=",toPath)
  response <- curl_webhdfs(fs, url, "PUT" , ...)
  
  return(fromJSON(response)$boolean)
}
