# library(shortcutr)
# 
# # if(!is.null(endpoint) & !is.null(url)) stop("Please specify only one of full url or endpoint")
# set_token(secret::get_secret('sc_token'))
# query <- "completed:2023-04-01..2023-05-15"
# endpoint = "stories"
# response_type <- "summary"
# url = NULL
# searsc_limit = 50
# page_size = 25
# config=list()
# sc_token = get_token()
# match.arg(response_type, c("full", "slim", "summary" , "minimal"))
# detail <- ifelse(response_type == 'full', 'full', 'slim')
# match.arg(endpoint, c("stories", "epics", "milestones", "iterations"))
# if(is.null(endpoint) & is.null(url)) stop("Please specify one of url or endpoint")
# if(!is.null(endpoint) & !is.null(url)) stop("Please specify only one of full url or endpoint")
# 
# # 
# get_all("milestones")
