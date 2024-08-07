#' create epic
#'
#' @param name Required. The Epic’s name.
#' @param created_at Time created. Default is \code{Sys.time()}
#' @param deadline POSIXct format. The Epic’s deadline. Default NULL
#' @param description chr The Epic’s description. Default NULL
#' @param epic_state_id int The ID of the Epic State. See
#'        /code{get_all("epic-workflow")}, Defaults to your accounts default state
#' @param milestone_id int The ID of the Milestone this Epic is related to. Default NULL
#' @param planned_start_date POSIXct. The Epic’s planned start date. Default NULL
#' @param requested_by_id uuid The ID of the member that requested the epic. Default NULL
#' @param owner_ids An array of UUIDs for any members you want to add as Owners on this new Epic.
#' @param group_ids An array of UUIDS for Groups to which this Epic is related.
#' @param config (optional) additional configuration to add to header
#' @param sc_token shortcut API token.  Defaults to \code{get_token()}
#'
#' @examples
#' \dontrun{
#' create_epic( name = "foobar",
#'              deadline = as.POSIXct("2022-06-30 17:00:00"),
#'              description = "this is a test"
#'              )
#'}
#' @export
#'
create_epic <- function(name,
            created_at = Sys.time(),
            deadline = NULL,
            description = NULL,
            epic_state_id = get_all("epic-workflow")$default_epic_state_id,
            milestone_id = NA,
            planned_start_date = NA,
            owner_ids = NULL,
            group_ids = NULL,
          #  follower_ids = NULL,
          #  labels = NULL,
            requested_by_id = NULL,
            config=list(),
            sc_token = get_token()
){

  lst_body <- list(
    name = name,
    created_at = fmt_date_string(created_at),
    deadline = fmt_date_string(deadline),
    description = description,
    epic_state_id = epic_state_id,
    milestone_id = milestone_id,
    planned_start_date = fmt_date_string(planned_start_date),
    group_ids = group_ids,
    owner_ids = owner_ids
  )

  body_content <- jsonlite::toJSON(lst_body, null = "null", auto_unbox = TRUE)

  df <- sc_POST(url = sc_url(endpoint = "epics", sc_token = sc_token),
                body = body_content,
                config = config)

  return(df)
}



#' create iteration
#' 
#' @param name Required. The name of this Iteration.
#' @param start_date as character. Required. The date this Iteration begins, e.g. "2019-07-01".
#' @param end_date as character. Required. The date this Iteration ends, e.g. "2019-07-01".
#' @param description Required. The description of the Iteration.
#' @param config (optional) additional configuration to add to header
#' @param sc_token shortcut API token.  Defaults to \code{get_token()}
#' 
#' @details 
#' arguments for \code{follower_ids}, \code{group_ids}, \code{labels} are not yet implemented. See \href{https://github.com/techisdead/shortcutR/issues/2}{issue 2} to contribute.
#' 
#' @examples 
#' \dontrun{
#' create_iteration(
#'   name = "09 Jan - 15 Jan 2022",
#'   description = "Sprint Q1-1 2022",
#'   start_date = "2022-01-09",
#'   end_date = "2022-01-15"
#' )
#' }
#' 
#' @export
#' 
create_iteration <- function(
    name, 
    start_date,
    end_date,
    description,
   # follower_ids = NA,
   # group_ids = NA,
   #  labels = NA,
    config=list(),
    sc_token = get_token()
){
  
  ## TODO - check formatting for the array types
  ## follower_ids TODO An array of UUIDs for any Members you want to add as Followers.
  ## group_ids TODO An array of UUIDs for any Groups you want to add as Followers. Currently, only one Group association is presented in our web UI.
  ##  labels An array of Labels attached to the Iteration.
  
  follower_ids <- NA
  group_ids <- NA
  labels <- NA
  
  lst_body <- list(
    description = description,
    end_date = end_date,
    follower_ids = follower_ids,
    group_ids = group_ids,
    labels = labels,
    name = name,
    start_date = start_date
  )
  
  body_content <- jsonlite::toJSON(lst_body, null = "null", auto_unbox = TRUE)

  df <- sc_POST(url = sc_url(endpoint = "iterations", sc_token = sc_token),
                body = body_content,
                config = config)
  
  return(df)
  
}



fmt_date_string <- function(dte){
  
  ifelse(is.null(dte),NULL, format(dte, "%Y-%m-%dT%H:%M:%SZ") )
  
}
