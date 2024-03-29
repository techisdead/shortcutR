#' shortcut api
#'
#' The default base function to allow generic api calls
#' @param verb the api verb from the httr package (e.g. GET)
#' @param url the full request url
#' @param config additional configuration to add to header
#' @param ... other parameters to pass on to httr api call
#' 
#' @importFrom httr GET PUT PATCH POST DELETE http_status
sc_api <- function (verb,
                    url,
                    config=list(), ...) {
  

  FUN <- get(verb, envir=asNamespace("httr"))
  resp <- FUN(url, ..., config=c(sc_config(), config))
  
  #check for errors
  if (resp$status_code > 201L)  {  # error
    msg <- httr::http_status(resp)$message
    stop(msg, call.=FALSE)
  } else { # return response
    return(resp)
  }
}

#' @importFrom httr add_headers
sc_config <- function () {
  httr::add_headers(`user-agent`=ua("shortcutr"),
                    `content-type`="application/json",
                    `http_version` = 1.1 # get around occasional http2 bug
                    )

}

#' @importFrom utils packageVersion
ua <- function (pkg) paste(pkg, as.character(packageVersion(pkg)), sep="/")


#' shortcut url
#' 
#' Function to construct urls for shortcut api calls
#' 
#' @param sc_base_url the base url. Defaults to V2 of API.
#' @param endpoint endpoint to retrieve (e.g. project, team, story)
#' @param id id of single record to be retrieved using `get_one` function
#' @param sc_token shortcut API token. 
#' @param query optional query search term
#' 
#' 
#' @details 
#' This function constructs the url for each API call. \code{sc_token} 
#' may be input as function parameters or set once with \code{set_token}.
#' 
#' @export
sc_url <- function (sc_base_url = get_url(),
                    endpoint = NULL,
                    id = NULL,
                    sc_token = get_token(), 
                    query = NULL) {

  # example :
  # https://api.shortcut.io/api/v2/teams/{team-public-id}?token=$shortcut_API_TOKEN

  url <- paste0(sc_base_url,
                if(!is.null(endpoint)){"/"}, endpoint,
                if(!is.null(id)){"/"}, id,
                "?token=", sc_token,
                if(!is.null(query)){"&query="}, query
  )
  return(url)
}



#' @importFrom jsonlite fromJSON
#' @importFrom httr content
sc_GET <- function (url, 
                    config=list(),  ...) {
  
  resp <- sc_api("GET", url, config, ...)

  if(length(resp) == 0) warning(
    "Empty list returned. Do you have data at this endpoint?")
  return( jsonlite::fromJSON( httr::content(resp
                                            , as = "text"
                                            , encoding = "ISO-8859-1") ) )
  
}
  
sc_PUT <- function (url, ...) sc_api("PUT", url, ...)

sc_DELETE <- function (url, ...) sc_api("DELETE", url, ...)

sc_POST <- function (url, ...) sc_api("POST", url, ...)

# sc_PATCH <- function (url, ...) sc_api("PATCH", url, ...)
#
