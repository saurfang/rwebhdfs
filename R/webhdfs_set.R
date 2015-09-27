#' Set WebHDFS File Properties
#' 
#' Miscellaneous set functions for file properties:
#' \enumerate{
#'    \item Permission
#'    \item Owner
#'    \item Replication Factor
#'    \item Access or Modication time
#' }
#' 
#' @param webhdfs a \code{\link{webhdfs}} object
#' @param path The path to file/directory
#' @param permission The permission of a file/directory
#' @param owner The username who is the owner of a file/directory
#' @param group The name of user group
#' @param replication The number of replications of a file
#' @param modificationtime The modification time of a file/directory (in unix milliseconds timestamp)
#' @param accesstime The access time of a file/directory (in unix milliseconds timestamp)
#' @param ... additional arguments passed to \code{\link{curl_webhdfs}}
#' @return result from \code{\link{file_stat}} on the updated file/directory
#' @include curl_webhdfs.R
#' @importFrom jsonlite fromJSON
#' @export
webhdfs_set <- function(webhdfs, path, permission=NULL, owner=NULL, group=NULL,
                             replication=NULL, modificationtime=NULL, accesstime=NULL, ...){
  if(!missing(permission)){
    url <- paste0(path, "?op=SETPERMISSION")
    if(!is.null(permission) && grepl("^[01]?[0-7]{3}$", permission))
      url <- paste0(url, "&permission=", permission)
    curl_webhdfs(webhdfs, url, "PUT", ...)
  }
  
  if(!missing(owner) || !missing(group)){
    url <- paste0(path, "?op=SETOWNER")
    if(!is.null(owner) && nzchar(owner))
      url <- paste0(url, "&owner=", owner)
    if(!is.null(group) && nzchar(group))
      url <- paste0(url, "&group=", group)
    curl_webhdfs(webhdfs, url, "PUT", ...)
  }
  
  if(!missing(replication)){
    url <- paste0(path, "?op=SETREPLICATION")
    if(is.numeric(replication) && replication > 0)
      url <- paste0(url, "&replication=", replication)
    curl_webhdfs(webhdfs, url, "PUT", ...)
  }
  
  if(!missing(modificationtime) || !missing(accesstime)){
    url <- paste0(path, "?op=SETTIMES")
    if(is.numeric(modificationtime))
      url <- paste0(url, "&modificationtime=", round(modificationtime))
    if(is.numeric(accesstime))
      url <- paste0(url, "&accesstime=", round(accesstime))
    curl_webhdfs(webhdfs, url, "PUT", ...)
  }
  
  file_stat(webhdfs, path)
}
