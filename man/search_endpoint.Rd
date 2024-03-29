% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/search.R
\name{search_endpoint}
\alias{search_endpoint}
\title{Search stories}
\usage{
search_endpoint(
  endpoint = "stories",
  query,
  response_type = "slim",
  url = NULL,
  searsc_limit = 1000,
  page_size = 25,
  config = list(),
  sc_token = get_token()
)
}
\arguments{
\item{endpoint}{data to list. One of "stories", "epics",  "milestones", "iterations". Defaults to "stories"}

\item{query}{the required query. See \url{https://help.shortcut.com/hc/en-us/articles/360000046646-Search-Operators} for options and the difference between story earch operators and general search operators}

\item{response_type}{\code{'full'} or \code{'slim'} or \code{'summary'}. Slim omits larger full text fields and only references related items by id. Summary simplifies list columns and returns (in my personal opinion) useful columns. Minimal only returns 'entity_type', 'id', 'name', 'description'. Default is 'full'}

\item{url}{Alternatively specify query as a url string. Useful if you want to use a different version of the API (default is V3)}

\item{searsc_limit}{Maximum records to retrieve.  Defaults to 1000.}

\item{page_size}{The number of search results to include in a page. Min 1 max 25. Default 25}

\item{config}{additional configuration to add to header}

\item{sc_token}{shortcut API token.}
}
\description{
Search stories
}
\examples{
\dontrun{
# Search for the term "foo" anywhere in stories
search_stories(query = "foo")

# Search for epic names that contain specified text
# Note - no whitespace areound the ":"
search_stories(query = "epic:foo")

# Combine search terms with "AND" ("OR" combinations are not supported by the API V2)
# Note the whitespace between each filter
search_stories(query = "epic:foo is:done type:bug")


}

Search endpoint

Update to match new v3 function that allows search of stories, epics, milestones and iterations

\dontrun{
# Search for the term "foo" anywhere in stories
search(endpoint = "stories" query = "foo")

# Search for epics that contain specified text in name or description
# Note - no whitespace areound the ":"
search(endpoint = "epic", query = "title:foo")

# Combine search terms with "AND" ("OR" combinations are not supported by the API V2)
# Note the whitespace between each filter
search(endpoint = "stories", query = "epic:foo is:done type:bug")
}

}
