#' Concat File(s) on a FileSystem
#' 
#' @return \code{TRUE} if successful, \code{FALSE} otherwise
#' @title concat_file: Concat File(s) on a FileSystem
#' @param fs FileSystem object
#' @param targetPath a character vector that contains the path of file to write
#' @param sourcePath a list of character vector that contains the path of file(s) to read
#' @param ... other arguments
#' @rdname concat_file
#' @export concat_file
concat_file <- function(fs, targetPath, sourcePath, ...){
  UseMethod("concat_file")
}

#' @rdname concat_file
#' @method concat_file default
#' @S3method concat_file default
concat_file.default <- function(fs, targetPath, sourcePath, ...){
  warning("Unrecognized filesystem, invoking read/writeLines...")
  if(file.exists(targetPath))
    stop("File already exists: ", targetPath)
  for(path in sourcePath)
    write(readLines(path), file=targetPath, append=TRUE)
}

#' @rdname concat_file
#' @method concat_file webhdfs
#' @S3method concat_file webhdfs
#' @importFrom RCurl basicHeaderGatherer
#' @include curl_webhdfs.R get_webhdfs_home.R
concat_file.webhdfs <- function(fs, targetPath, sourcePath, ...){
  #Check path is non empty
  if(!nzchar(targetPath))
    stop("Target Path must be non-empty")
  
  #Padding sourcePath with home directory if needed
  indices <- grep("^/", sourcePath, invert=TRUE)
  if(length(indices) > 0){
    home <- get_webhdfs_home(fs, ...)
    sourcePath[indices] <- paste0(home, "/", sourcePath[indices])
  }
  url <- paste0(targetPath,"?op=CONCAT&sources=",paste0(sourcePath, collapse=","))
    
  h <- basicHeaderGatherer()
  response <- curl_webhdfs(fs, url, "POST" , headerfunction = h$update, ...)
  if(h$value()["status"]!="200" || h$value()["statusMessage"]!="OK"){
    warning("Failed to concat file: ", h$value(), "\n", response)
    return(FALSE)
  }
  
  return(TRUE)
}