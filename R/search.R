#' 
#' Search stories
#' 
#' @param query Search query.  See https://help.shortcut.io/hc/en-us/articles/115005967026 for more detail on allowed search structure.
#' @param searsc_limit Maximum records to retrieve.  Defaults to 1000.
#' @param url Alternatively specify query as a url string. Useful if you want to use a different version of the API (default is V2)
#' @param config additional configuration to add to header
#' @param response_type either "full" (all data as a nested list) or
#' "summary" (flattened data frame with nested lists removed) or
#' "minimal" (just "entity_type", "id", "name", "description"). Defaults to "summary"
#' @param sc_token shortcut API token.
#' 
#' 
#' @examples
#' \dontrun{
#' # Search for the term "foo" anywhere in stories
#' search_stories(query = "foo")
#' 
#' # Search for epic names that contain specified text
#' # Note - no whitespace areound the ":"
#' search_stories(query = "epic:foo")
#' 
#' # Combine search terms with "AND" ("OR" combinations are not supported by the API V2)
#' # Note the whitespace between each filter
#' search_stories(query = "epic:foo is:done type:bug")
#' 
#' 
#' }
#' 
# @export
# search_stories <- function( query,
#                         searsc_limit = 1000,
#                         url = NULL,
#                         config=list(),
#                         response_type = "summary",
#                         sc_token = get_token()){
# 
# 
#   match.arg(response_type, c("full", "summary", "minimal"))
#   # TODO add in checks for url structure
#   # TODO add in checks for the query structure??
# 
#   
#   if(is.null(url)){ 
# 
#     url <- sc_url(endpoint = "search/stories", 
#                   query = query, sc_token = sc_token) 
#   }
#   
#   res <- sc_GET(url =  url, config = config)
#   
#   
#   if( length(res[["data"]]) == 0){
#     message("Query result empty")
#     return(NULL)
#   }
#   
#   lst <- list()
#   lst[[1]] <- as.data.frame( res$data,row.names = NULL )
#   
#   nrec <- nrow(lst[[1]]) 
#   i <- 2
#   while( length(res[["next"]]) == 1 & nrec < searsc_limit){
#     
#     url <- paste0( "https://api.shortcut.io", res[["next"]], 
#                    "&token=", sc_token)
#     res <- sc_GET(url =  url, config = config)
#     lst[[i]] <- as.data.frame( res$data, row.names = NULL  )
#     
#     nrec <- nrec+nrow(lst[[i]])
#     i <- i+1
#   }
#   
#   # return data
#   switch (response_type,
#           full = return(lst),
#           summary = {
#             cols <- c('description', 'archived', 'started', 'entity_type',  'story_type', 
#                       'workflow_id', 'completed_at_override', 'started_at', 
#                       'completed_at', 'name', 'completed',  'blocker', 'epic_id', 
#                       'requested_by_id', 'iteration_id', 'started_at_override', 
#                       'group_id', 'workflow_state_id', 'updated_at',  'external_id', 
#                       'id', 'estimate', 'position', 'blocked', 'project_id', 'deadline', 
#                       'created_at', 'moved_at', 'lead_time', 'cycle_time')
#             
#             df <- do.call("rbind", lapply(lst, "[", cols) ) 
#             if(nrow(df) > searsc_limit){
#               return(df[1:searsc_limit,])
#             } else{
#             return(df)
#             }
#           }, 
#           minimal = {
#             
#             cols <- c("entity_type", "id", "name", "description")
#             df <- do.call("rbind", lapply(lst, "[", cols) ) 
#             if(nrow(df) > searsc_limit){
#               return(df[1:searsc_limit,])
#             } else{
#               return(df)
#             }
#           }
#   )
# 
# }


#' Search endpoint
#' 
#' Update to match new v3 function that allows search of stories, epics, milestones and iterations
#' 
#' @param endpoint data to list. One of "stories", "epics",  "milestones", "iterations". Defaults to "stories"
#' @param query the required query. See [https://help.shortcut.com/hc/en-us/articles/360000046646-Search-Operators](https://help.shortcut.com/hc/en-us/articles/360000046646-Search-Operators) for options and the difference between story earch operators and general search operators
#' @param response_type \code{'full'} or \code{'slim'} or \code{'summary'}. Slim omits larger full text fields and only references related items by id. Summary simplifies list columns and returns (in my personal opinion) useful columns. Minimal only returns 'entity_type', 'id', 'name', 'description'. Default is 'full'
#' @param url Alternatively specify query as a url string. Useful if you want to use a different version of the API (default is V3)
#' @param searsc_limit Maximum records to retrieve.  Defaults to 1000.
#' @param page_size The number of search results to include in a page. Min 1 max 25. Default 25
#' @param config additional configuration to add to header
#' @param sc_token shortcut API token.
#'
#'
#'@examples
#'\dontrun{
#' # Search for the term "foo" anywhere in stories
#' search(endpoint = "stories" query = "foo")
#'
#' # Search for epics that contain specified text in name or description
#' # Note - no whitespace areound the ":"
#' search(endpoint = "epic", query = "title:foo")
#'
#' # Combine search terms with "AND" ("OR" combinations are not supported by the API V2)
#' # Note the whitespace between each filter
#' search(endpoint = "stories", query = "epic:foo is:done type:bug")
#' }
#'
#' @export
search_endpoint <- function( endpoint = "stories",
                             query, 
                             response_type = "slim",
                             url = NULL, 
                             searsc_limit = 1000,
                             page_size = 25,
                             config=list(),
                             sc_token = get_token()){
  
  match.arg(response_type, c("full", "slim", "summary" , "minimal"))
  
  detail <- ifelse(response_type == 'full', 'full', 'slim')
  
  
  match.arg(endpoint, c("stories", "epics", "milestones", "iterations"))
  
  if(is.null(endpoint) & is.null(url)) stop("Please specify one of url or endpoint")
  if(!is.null(endpoint) & !is.null(url)) stop("Please specify only one of full url or endpoint")
  
  df <- sc_GET(url = ifelse( is.null(url), 
                             sc_url(endpoint = paste0("search/", endpoint), 
                                    query = paste0(query, "&detail=", detail),
                                    sc_token = sc_token), 
                             url),
               config = config)
  
  
    if(is.null(url)){

      url <- sc_url(endpoint = paste0("search/", endpoint),
                    query = paste0(query, "&detail=", detail), 
                    sc_token = sc_token)
    }

    res <- sc_GET(url =  url, config = config)


    if( length(res[["data"]]) == 0){
      message("Query result empty")
      return(NULL)
    }

    lst <- list()
    lst[[1]] <- as.data.frame( res$data,row.names = NULL )

    nrec <- nrow(lst[[1]])
    i <- 2
    while( length(res[["next"]]) == 1 & nrec < searsc_limit){

      url <- paste0( "https://api.app.shortcut.com", res[["next"]],
                     "&token=", sc_token)
      res <- sc_GET(url =  url, config = config)
      lst[[i]] <- as.data.frame( res$data, row.names = NULL  )

      nrec <- nrec+nrow(lst[[i]])
      i <- i+1
    }
  
    switch (response_type,
            full = return(lst),
            slim = return(lst),
            summary = {
              cols <- c( 'archived', 'started', 'entity_type',  'story_type',
                        'workflow_id', 'completed_at_override', 'started_at',
                        'completed_at', 'name', 'completed',  'blocker', 'epic_id',
                        'requested_by_id', 'iteration_id', 'started_at_override',
                        'group_id', 'workflow_state_id', 'updated_at',  'external_id',
                        'id', 'estimate', 'position', 'blocked', 'project_id', 'deadline',
                        'created_at', 'moved_at', 'lead_time', 'cycle_time')
              df <- do.call("rbind", lapply(lst, "[", cols) )
              if(nrow(df) > searsc_limit){
                return(df[1:searsc_limit,])
              } else{
                return(df)
              }
            },
            minimal = {
              
              cols <- c("entity_type", "id", "name", "description")
              df <- do.call("rbind", lapply(lst, "[", cols) )
              if(nrow(df) > searsc_limit){
                return(df[1:searsc_limit,])
              } else{
                return(df)
              }
            }
    )

    
    
}

#' List Epic Stories
#'
#' @param epic_id Epic Id as numeric / character string
#' @param config additional configuration to add to header
#' @param sc_token shortcut API token
#'
#' @return Data frame with stories belonging to that Epic
#' @export
#'
#' @examples
#' list_epic_stories(123)
list_epic_stories <- function(epic_id
                              , config=list()
                              , sc_token = get_token()){
  
  
  
  if(is.null(epic_id)) stop("Please specify epic_id")
  
  df <- sc_GET(url = paste0(get_url(), "/epics/", epic_id, "/stories?token=", sc_token)
               , config = config)
  
  return(df)
  
}

