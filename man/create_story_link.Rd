% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/post.R
\name{create_story_link}
\alias{create_story_link}
\title{create story link}
\usage{
create_story_link(
  object_id,
  subject_id,
  verb = c("blocks", "duplicates", "relates to"),
  config = list(),
  sc_token = get_token()
)
}
\arguments{
\item{object_id}{Numeric (Required). The ID of the object Story}

\item{subject_id}{Numeric (Required). The ID of the subject Story}

\item{verb}{Character (Required). The type of link. One of blocks, duplicates, relates to. Defaults to blocks}

\item{config}{(optional) additional configuration to add to header}

\item{sc_token}{shortcut API token.  Defaults to \code{get_token()}}
}
\description{
create story link
}
\examples{
\dontrun{
create_story_link(
  object_id = 123,
  subject_id = 321,
  verb = "duplicates"
)
}

}
