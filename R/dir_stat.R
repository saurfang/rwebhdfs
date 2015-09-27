#' Status of a Directory on a FileSystem
#' 
#' @return a list of status information on directory
#' @title dir_stat: Status of a Directory on a FileSystem
#' @param fs FileSystem object
#' @param path a character vector that contains the path of directory
#' @param ... other arguments
#' @rdname dir_stat
#' @export dir_stat
dir_stat <- function(fs, path, ...){
  UseMethod("dir_stat")
}

#' @rdname dir_stat
#' @method dir_stat default
#' @export
dir_stat.default <- function(fs, path, ...){
  warning("Unrecognized filesystem, invoking file.info...")
  file.info(list.files(path), ...)
}
 
#' @rdname dir_stat
#' @method dir_stat webhdfs
#' @export
#' @importFrom jsonlite fromJSON
#' @include curl_webhdfs.R
dir_stat.webhdfs <- function(fs, path, ...){  
  response <- curl_webhdfs(fs, paste0(path,"?op=LISTSTATUS"), "GET", ...)
  
  status <- fromJSON(response)$FileStatuses$FileStatus
  rownames(status) <- status$pathSuffix
  status$accessTime <- as.POSIXlt(status$accessTime/1000, origin="1970-01-01")
  status$modificationTime <- as.POSIXlt(status$modificationTime/1000, origin="1970-01-01")
  status
}
