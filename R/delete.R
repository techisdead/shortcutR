#'delete iteration
#'
#'@param iteration_public_id Required. The unique ID of the Iteration.
#' @param config (optional) additional configuration to add to header
#' @param sc_token shortcut API token.  Defaults to \code{get_token()}
#' 
#'@export
delete_iteration <- function(
    iteration_public_id,
    config=list(),
    sc_token = get_token()
    ){
  

  df <- sc_DELETE(url = sc_url(endpoint = "iterations",
                               id = iteration_public_id, 
                               sc_token = sc_token), 
                  config = config)
  
  return(df) 
}


