#' Dispatch a WebHDFS Curl request
#' @param webhdfs A \code{\link{webhdfs}} object
#' @param url The curl to request
#' @param requestType a character vector chosen from "GET", "POST", "PUT"
#' @param doas proxy username
#' @param putContent content to send in a PUT request
#' @param .opts a \code{\link{list}} of \code{\link[RCurl]{curlOptions}}
#' @param headerfunction a function that processes the header information
#' @param ... additional arguments passed to \code{\link[RCurl]{getURL}}
#' @return response content
#' @export
#' @importFrom RCurl httpGET httpPUT httpPOST basicHeaderGatherer
#' @importFrom jsonlite fromJSON
curlWebHDFS <- function(webhdfs, url, requestType = c("GET","POST","PUT"), 
                        doas = NULL, putContent = NULL, .opts = curlOptions(),
                        headerfunction = NULL, ...){
  if(!inherits(webhdfs, "webhdfs"))
    stop("Need a valid webhdfs object: ", webhdfs)
  
  #piece together request URL
  if(!grepl("^http://", url))
    url <- paste0("http://",webhdfs$host,":",webhdfs$port,"/webhdfs/v1",url)
  if(webhdfs$security && isTRUE(nzchar(webhdfs$token)))
    url <- paste0(url,"&token=",webhdfs$token)
  if(isTRUE(nzchar(doas)))
    url <- paste0(url,"&doas=",doas)
  
  opts <- if(inherits(.opts, "curlOptions")) .opts else curlOptions()
  opts <- curlOptions(..., opts)
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
  
  requestType <- opts[["customrequest"]] <- match.arg(requestType)
  response <- switch(requestType,
                     GET = {
                       httpGET(url, .opts=opts)
                     },
                     PUT = {
                       if(is.null(putContent))
                         httpPUT(url, .opts=opts)
                       else
                         httpPUT(url, putContent, .opts=opts)
                     },
                     POST = {
                       ##TODO: Double check this is correct
                       httpPOST(url, .opts=opts)
                     })
  #response <- getURL(url, .opts=opts)
  
  if(h$value()["status"] %in% c("400","401","403","404","500"))
    stop("Request failed: ", h$value()["statusMessage"],
         try(fromJSON(response, simplifyDataFrame=FALSE)$RemoteException, T))
  
  response
}