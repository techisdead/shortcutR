#'delete iteration
#'
#'@param iteration_public_id Required. The unique ID of the Iteration.
#'
#'@export
delete_iteration <- function(
    iteration_public_id,
    config=list(),
    sc_token = get_token()
    ){
  

  df <- sc_DELETE(url = sc_url(endpoint = "iterations",
                               id = id, 
                               sc_token = sc_token), 
                  config = config)
  
  return(df) 
}


