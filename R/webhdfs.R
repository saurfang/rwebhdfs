#' Initialize WebHDFS Object
#' 
#' @section Enable WebHDFS:
#' To enable WebHDFS in your Hadoop Installation add the following configuration 
#' to your hdfs_site.xml (requires Hadoop >0.20.205.0):
#'
#'  <property>
#'  <name>dfs.webhdfs.enabled</name>
#'  <value>true</value>
#'  </property>  
#'  
#'  see: \url{https://issues.apache.org/jira/secure/attachment/12500090/WebHdfsAPI20111020.pdf}
#' 
#' @param namenode_host namenode host address
#' @param namenode_port namenode host port
#' @param hdfs_username webhdfs access username
#' @param securityON \code{TRUE} if HDFS security is on, \code{FALSE} otherwise
#' @param token Hadoop delegation token to use when security is ON. \code{NULL} implies Kerberos SPNEGO authentication if \code{securityON} is \code{TRUE}
#' @return a webhdfs object that captures the configuration
#' @importFrom RCurl httpPUT httpGET curlOptions
#' @export
webhdfs <- function(namenode_host, namenode_port, hdfs_username,
                    securityON = FALSE, token = NULL){
  #validate hostname
  ValidIpAddressRegex <- "^(([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])\\.){3}([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])$"
  ValidHostnameRegex <- "^(([a-zA-Z0-9]|[a-zA-Z0-9][a-zA-Z0-9\\-]*[a-zA-Z0-9])\\.)*([A-Za-z0-9]|[A-Za-z0-9][A-Za-z0-9\\-]*[A-Za-z0-9])$"
  if(!grepl(ValidIpAddressRegex, namenode_host) && !grepl(ValidHostnameRegex, namenode_host))
    stop("hostname is not valid: ", namenode_host)
  
  #validate port number
  tryCatch({
    namenode_port <- as.integer(namenode_port)
    if(is.na(namenode_port) || namenode_port < 1 || namenode_port > 65535)
      stop("port number must between 1 and 65535: ", namenode_port)
  }, warning = function(x){
    stop("port must be a valid integer!")
  })
  
  #validate non-empty token if supplied
  if(!is.null(token) && !isTRUE(nzchar(token)))
    warning("Delegation token is empty")
  
  structure(list(host=namenode_host,
                 port=namenode_port,
                 user=hdfs_username,
                 security=securityON,
                 token=token),
            class = "webhdfs")
}
