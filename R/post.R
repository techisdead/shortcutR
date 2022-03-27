#' create epic
#'
#' @param name Required. The Epic’s name.
#' @param deadline POSIXct format. The Epic’s deadline. Default NULL
#' @param description chr The Epic’s description. Default NULL
#' @param epic_state_id int The ID of the Epic State. See
#'        /code{sc_list_all("epic-workflow")}, Defaults to your accounts default state
#' @param milestone_id int The ID of the Milestone this Epic is related to. Default NULL
#' @param planned_start_date POSIXct. The Epic’s planned start date. Default NULL
#' @param requested_by_id uuid The ID of the member that requested the epic. Default NULL
#'
#'
#' @examples
#'
#' create_epic( name = "foobar",
#'              deadline = as.POSIXct("2022-06-30 17:00:00"),
#'              description = "this is a test"
#'              )
#'
#' @export
#'
create_epic <- function(name,
            created_at = Sys.time(),
            deadline = NULL,
            description = NULL,
            epic_state_id = sc_list_all("epic-workflow")$default_epic_state_id,
            milestone_id = NULL,
            planned_start_date = NULL,
          #  follower_ids = NULL,
          #  group_id = NULL,
          #  labels = NULL,
          # owner_ids = NULL,
          # requested_by_id = NULL,
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





fmt_date_string <- function(dte){
  
  ifelse(is.null(dte),NULL, format(deadline, "%Y-%m-%dT%H:%M:%SZ") )
  
}
