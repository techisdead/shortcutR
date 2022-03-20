#' Get endpoint data
#' 
#' @param url shortcut API url.  Defaults to API V2
#' @param endpoint  data to list. One of "categories", "entity-templates","epic-workflow", "epics"
#' ,"files", "groups", "iterations","labels", "linked-files", "members",
#' "milestones", "projects", "repositories", 
#' "workflows"
#' @param config additional configuration to add to header
#' @param sc_token shortcut API token. 
#' @param response_type either "full" (all data) or "minimal" (just "entity_type", "id", 
#'"name", "description"). Defaults to "full"
#'
#'@examples
#'
#' \dontrun{
#'
#' # Retrieve all projects
#' sc_list_all(endpoint = "projects")
#' 
#' # List epics then retrieve full details about the first one
#' df <- sc_list_all("epics", response_type = "minimal")
#' sc_get_one("epics", id = df[ 1, "id"] )
#' }
#' 
#' @export
sc_list_all <- function(endpoint = NULL,
                        url = NULL,
                        config=list(),
                        sc_token = get_token(),
                        response_type = "full"){
  
  match.arg(endpoint, choices = c("categories", "entity-templates","epic-workflow", "epics", 
            "files", "groups", "iterations","labels", "linked-files", "members",
            "milestones", "projects", "repositories", 
             "workflows"))
  match.arg(response_type, choices = c("full", "minimal"))
  if(is.null(endpoint) & is.null(url)) stop("Please specify one of url or endpoint")
  if(!is.null(endpoint) & !is.null(url)) stop("Please specify only one of full url or endpoint")

  df <- sc_GET(url = ifelse( is.null(url), 
                             sc_url(endpoint = endpoint, sc_token = sc_token), 
                             url),
               config = config)

  # account for different structure for these two endpoints
  if(endpoint == "epic-workflow" & response_type == "minimal"){df <- df$epic_states}
  if(endpoint == "members" & response_type == "minimal"){df <- df$profile}
  
  # return data
  switch (response_type,
    full = return(df),
    minimal = if(length(df) == 0) {message("Can't return minimal columns - no records")
      } else{return(df[, c("entity_type", "id", "name")])} 
  )
  
  
}


#' Get endpoint data for one item
#' 
#' @param url shortcut API url.  Defaults to API V2
#' @param endpoint  data to list. One of "categories", "entity-templates", "epic-workflows", "epics", 
#' "files", "groups", "iterations"  ,"labels", "linked-files", 
#' "members", "milestones", "projects", "repositories", "stories",
#' "story-links"
#' @param id the id for the record you want to retrieve
#' @param config additional configuration to add to header
#' @param sc_token shortcut API token. 
#'
#'@examples
#'\dontrun{
#' # Retrieve all projects
#' sc_list_all(endpoint = "projects")
#' 
#' # List epics then retrieve full details about the first one
#' df <- sc_list_all("epics", response_type = "minimal")
#' sc_get_one("epics", id = df[ 1, "id"] )
#' }
#' 
#' @export
sc_get_one <- function( id, 
                        endpoint = NULL,
                        url = NULL,
                        config=list(),
                        sc_token = get_token()){
  
  # TODO: "members" endpoint is failing - need to add
  
  match.arg(endpoint, choices = c("categories", "entity-templates", "epic-workflows", "epics", 
                                  "files", "groups", "iterations"  ,"labels", "linked-files", 
                                  "members", "milestones", "projects", "repositories", "stories",
                                  "story-links"))
  
  if(is.null(endpoint) & is.null(url)) stop("Please specify one of url or endpoint")
  if(!is.null(endpoint) & !is.null(url)) stop("Please specify only one of full url or endpoint")
  
  df <- sc_GET(url = ifelse( is.null(url), 
                             sc_url(endpoint = endpoint, id = id, sc_token = sc_token), 
                             url),
               config = config)
  
  return(df)

}

