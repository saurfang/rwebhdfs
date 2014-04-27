#' Get Home Directory
#' 
#' Return the current user's home directory in this filesystem. 
#' The default implementation returns "/user/$USER/".
#' 
#' @param webhdfs a \code{\link{webhdfs}} object
#' @param doas user whose home folder will be retrieve
#' @param ... additional arguments passed to \code{\link{curl_webhdfs}}
#' @include curl_webhdfs.R
#' @importFrom jsonlite fromJSON
#' @export
get_webhdfs_home <- function(webhdfs, doas=NULL, ...){
  response <- curl_webhdfs(webhdfs, "/?op=GETHOMEDIRECTORY", "GET", doas=doas, ...)
  return(fromJSON(response)$Path)
}