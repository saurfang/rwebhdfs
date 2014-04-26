#' Dispatch a WebHDFS Curl request
#' @param webhdfs A \code{\link{webhdfs}} object
#' @param url The curl to request
#' @param requestType a character vector chosen from "GET", "POST", "PUT"
#' @param doas proxy username that will be impersonated by the query user above
#' @param putContent content to send in a PUT request
#' @param .opts a \code{\link{list}} of \code{\link[RCurl]{curlOptions}}
#' @param headerfunction a function that processes the header information
#' @param ... additional arguments passed to \code{\link[RCurl]{getURL}}
#' @return response content
#' @export
#' @importFrom RCurl getURL basicHeaderGatherer
#' @importFrom jsonlite fromJSON
curl_webhdfs <- function(webhdfs, url, requestType = c("GET","POST","PUT","DELETE"), 
                        doas = NULL, putContent = NULL, .opts = curlOptions(), 
                        headerfunction = NULL, ...){
  if(!inherits(webhdfs, "webhdfs"))
    stop("Need a valid webhdfs object: ", webhdfs)
  
  #piece together request URL
  if(!grepl("^http://", url)){
    if(substring(url, 1, 1) != "/")
      url <- paste0(get_webhdfs_home(webhdfs, doas), "/", url)
    url <- paste0("http://",webhdfs$host,":",webhdfs$port,"/webhdfs/v1",url)
  }
  if(webhdfs$security && isTRUE(nzchar(webhdfs$token)))
    url <- paste0(url,"&token=",webhdfs$token)
  if(isTRUE(nzchar(webhdfs$user)))
    url <- paste0(url,"&user.name=", webhdfs$user)
  if(isTRUE(nzchar(doas)))
    url <- paste0(url,"&doas=",doas)
  
  opts <- if(inherits(.opts, "curlOptions")) .opts else curlOptions()
  opts <- curlOptions(..., .opts=opts)
  #Enable Kerberos SPNEGO
  if(webhdfs$security && is.null(webhdfs$token))
      opts[["username"]] <- ":"
  
  #We want to capture some header information for error analysis
  h <- basicHeaderGatherer()
  hFunc <- function(str){
    #User might also have interests in this information
    if(is.function(headerfunction))
      headerfunction(str)
    h$update(str)
  }
  opts[["headerfunction"]] <- hFunc
  
  opts[["customrequest"]] <- requestType <- match.arg(requestType)
  if(requestType %in% c("PUT", "POST") && !is.null(putContent)){
    val = if (is.character(putContent)) 
      charToRaw(paste(putContent, collapse = "\n"))
    else if (is.raw(putContent)) 
      putContent
    else 
      stop("not certain how to convert content to the target type for a PUT request")
    opts <- curlOptions(infilesize = length(val), readfunction = val, 
                        upload = TRUE, .opts=opts)
  }
  
  response <- getURL(url, .opts=opts)
  
  if(as.integer(h$value()["status"]) >= 400)
    stop("Request failed: ", h$value()["statusMessage"],
         tryCatch({fromJSON(response, simplifyDataFrame=FALSE)$RemoteException},
                  error = function(x){response}),"\n",url)
  
  response
}