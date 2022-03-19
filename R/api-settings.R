#' Set token
#' 
#' Set the default token for use with the shortcut API.  
#' 
#' @param token shortcut API token
#' 
#' @details 
#' Using \code{set_token} allows you to set your API token once and it is then used for 
#' all functions that access the API. 
#' 
#' See https://app.shortcut.io/settings/account/api-tokens to get your API token.  
#' 
#' @examples 
#' 
#' set_token(token = "my_shortcut_token")
#' 
#' @export
set_token <- function(token){
  
  options <- getOption("shortcutr")
  options[["ch_token"]] <- token
  class(options) <- "shortcut_api"
  options(shortcutr = options)
  invisible(NULL)

}

#' Set base url
#' 
#' Change the default API URL for use with the shortcut API.  
#' 
#' @param url shortcut API URL
#' 
#' @details 
#' Using \code{set_url} allows you to change the API requests to point to a different API root.
#' By default \code{shortcutr} functions use the V2.0 API, "https://api.shortcut.io/api/v2",
#' as the basis for all requests
#' 
#' If you want to access an different version of the API you will need to use the \code{set_url}
#' function to change the API root
#' 
#' @examples
#' 
#' # access the beta version of the API
#' set_url(url = "https://shortcut.io/api/beta")
#' 
#' @export
set_url <- function(url){
  
  options <- getOption("shortcutr")
  options[["ch_base_url"]] <- url
  class(options) <- "shortcut_api"
  options(shortcutr = options)
  invisible(NULL)
  
}


#' Get token
#' 
#' Retrieve user token
#' 
#' @export
get_token <- function(){
  
  ch_token <- getOption("shortcutr")[["ch_token"]]
  if(is.na(ch_token)){
    stop("shortcut API token is missing.  Please use set_token() to set it or include a non-null 'token' parameter in your function call.")
  }
  return(ch_token)
}


#' Get URL
#' 
#' Retrieve base API url
#' 
#' @export
get_url <- function(){
  
  ch_base_url <- getOption("shortcutr")[["ch_base_url"]]
  if(is.na(ch_base_url)){
    stop("shortcut API URL is missing.  Please use set_url() to set it or include a non-null 'url' parameter in your function call.")
  }
  return(ch_base_url)
}

#' Reset API options
#' 
#' Set API url to default and clear API token
#' 
#' @examples
#' 
#' set_token("mytoken") # set token
#' get_token()  # check value updated
#' reset_api()  # clear values
#' 
#' @export
reset_api <- function(){
  
  options <- list(
    ch_base_url="https://api.shortcut.io/api/v2",
    ch_token=NA_character_
  )
  attr(options, "class") <- "shortcut_api"
  options(shortcutr = options)
  invisible(NULL)
  
}
