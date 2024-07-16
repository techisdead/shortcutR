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
          #  follower_ids = NULL,
          #  group_id = NULL,
          #  labels = NULL,
          # owner_ids = NULL,
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
    planned_start_date = fmt_date_string(planned_start_date)
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
  
  if (is.null(dte)) {
    return(NULL)
  } else {
    return(format(dte, "%Y-%m-%dT%H:%M:%SZ"))
  }
  
}

#' create story
#'
#' @param name Character (Required). The Story’s name.
#' @param description Character (Required). The description of the story.
#' @param epic_id Numeric (Required). The ID of the epic the story belongs to. To get the Epic ID, see \code{get_all("epics")$id}
#' @param owner_ids List of Strings (Required). An list of UUIDs of the owners of this story.To get the Owner UUID, see \code{get_all("members")$id}
#' @param project_id Numeric (Required). The ID of the project the story belongs to. To get the  Project ID, see \code{get_all("projects")$id}
#' @param workflow_state_id Numeric (Required). The ID of the workflow state the story will be in. To get the Workflow State ID, see \code{get_all("workflows")$states$id}
#' @param group_id Character (Required). The ID of the group to associate with this story. To get the  Group ID, see \code{get_all("groups")$id}
#' @param archived Character (Optional). Controls the story’s archived state. Defaults to FALSE
#' @param story_type Character (Optional). The type of story (feature, bug, chore). Defaults to feature.
#' @param created_at POSIXct (optional). The time/date the Story was created. Defaults to Sys.time().
#' @param estimate Numeric (Optional). The numeric point estimate of the story. Defaults to NULL, which means unestimated.
#' @param iteration_id Numeric (Optional). The ID of the iteration the story belongs to. To get the iteration_id, see \code{get_all("iterations")$id}
#' @param deadline POSIXct (Optional). The due date of the story.
#' @param labels List of Labels (Optional). A list of labels attached to the story.
#' @param config (optional) additional configuration to add to header
#' @param sc_token shortcut API token.  Defaults to \code{get_token()}
#' 
#' @examples
#' \dontrun{
#' create_story(
#'   name = "Test Story",
#'   epic_id = 123,
#'   owner_ids = list("12345678-9012-3456-7890-123456789012"),
#'   workflow_state_id = 123,
#'   project_id = 123,
#'   group_id = "12345678-9012-3456-7890-123456789012",
#'   archived = FALSE,
#'   story_type = "feature",
#'   description = "Test Description"
#' )
#' 
#' 
#' label_story <- list(
#'   list(
#'       color = '#49a940',
#'       name = 'in sprint'
#'     )
#'     , list(
#'       color = '#cc5856',
#'       name = 'new request'
#'     )
#' )
#' 
#' create_story(
#'   name = "Test Story",
#'   epic_id = 123,
#'   owner_ids = list("12345678-9012-3456-7890-123456789012"),
#'   workflow_state_id = 123,
#'   project_id = 123,
#'   group_id = "12345678-9012-3456-7890-123456789012",
#'   archived = FALSE,
#'   story_type = "feature",
#'   description = "Test Description",
#'   labels = label_story
#' )
#'}
#' @export
#'
create_story <- function(name
                         , description
                         , epic_id
                         , owner_ids
                         , project_id
                         , workflow_state_id
                         , group_id
                         , archived = c(FALSE, TRUE)
                         , story_type = c('feature', 'bug', 'chore')
                         , created_at = Sys.time()
                         , estimate = NULL
                         , iteration_id = NULL
                         , deadline = NULL
                         , labels = NULL
                         , config=list()
                         , sc_token = get_token()
){
  
  lst_body <- list(
    name = name,
    description = description,
    epic_id = epic_id,
    owner_ids = owner_ids,
    project_id = project_id,
    workflow_state_id = workflow_state_id,
    group_id = group_id,
    archived = archived,
    story_type = story_type,
    created_at = fmt_date_string(created_at),
    estimate = estimate,
    iteration_id = iteration_id,
    deadline = fmt_date_string(deadline),
    labels = labels
  )
  
  body_content <- jsonlite::toJSON(lst_body, null = "null", auto_unbox = TRUE)

  df <- sc_POST(url = sc_url(endpoint = "stories", sc_token = sc_token),
                body = body_content,
                config = config)
  
  return(df)
}
