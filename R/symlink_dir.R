#' Create a Symbolic Link on a FileSystem
#' 
#' @return \code{TRUE} if successful, \code{FALSE} otherwise
#' @title symlink_dir: Create a Symbolic Link on a FileSystem
#' @param fs FileSystem object
#' @param fromPath a character vector that contains the path linking from
#' @param toPath a character vector that contains the path linking to
#' @param createParent creates parent folder if \code{TRUE}
#' @param ... other arguments
#' @rdname symlink_dir
#' @export symlink_dir
symlink_dir <- function(fs, fromPath, toPath, createParent, ...){
  UseMethod("symlink_dir")
}

#' @rdname symlink_dir
#' @method symlink_dir default
#' @export
symlink_dir.default <- function(fs, fromPath, toPath, createParent=TRUE, ...){
  warning("Unrecognized filesystem, invoking file.symlink...")
  if(!file.exists(fromPath))
    stop("Source doesn't exist!")
  if(!file.exists(dirname(toPath)))
    if(createParent)
      dir.create(dirname(toPath), recursive=TRUE)
    else
      stop("Target's parent doesn't exist!")
  file.symlink(fromPath, toPath)
}

#' @rdname symlink_dir
#' @method symlink_dir webhdfs
#' @export
#' @importFrom RCurl basicHeaderGatherer
#' @include curl_webhdfs.R
symlink_dir.webhdfs <- function(fs, fromPath, toPath, createParent=TRUE, ...){  
  #Padding toPath with home directory if needed
  if(grep("^/", toPath, invert=TRUE))
    toPath <- paste0(get_webhdfs_home(fs, ...), "/", toPath)
  
  url <- paste0(fromPath,"?op=CREATESYMLINK&destination=",toPath,
                "&createParent=",if(isTRUE(createParent)) "true" else "false")
    
  response <- curl_webhdfs(fs, url, "PUT" , ...)
  
  invisible(TRUE)
}
