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
#' @param permission The permission of a file/directory
#' @param owner The username who is the owner of a file/directory
#' @param group The name of user group
#' @param replication The number of replications of a file
#' @param modificationtime The modification time of a file/directory
#' @param accesstime The access time of a file/directory
#' @param ... additional arguments passed to \code{\link[RCurl]{getURL}}
#' @include curl_webhdfs.R
#' @importFrom jsonlite fromJSON
#' @export
get_webhdfs_home <- function(webhdfs, permission=NULL, owner=NULL, group=NULL,
                             replication=NULL, modificationtime=...){
  response <- curl_webhdfs(webhdfs, "/?op=GETHOMEDIRECTORY", "GET", doas=doas, ...)
  return(fromJSON(response)$Path)
}